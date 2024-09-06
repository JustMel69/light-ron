use lexer::{Lexer, Token};

mod lexer;

enum InternalState<'a> {
    SecondValue,
    Map,
    Struct { name: Option<&'a str> },
    Tuple { name: Option<&'a str> },
    List,
    OptionalSomeValue,
    EndedOptionalSomeValue,
}


pub struct RonDeserializer<'a> {
    lexer: Lexer<'a>,
    tok_queue: Vec<Token>,
    stack: Vec<InternalState<'a>>
}

impl<'a> RonDeserializer<'a> {
    pub fn new(src: &'a str) -> Self {
        return Self { lexer: Lexer::new(src), tok_queue: Vec::new(), stack: Vec::new() };
    }

    pub fn next_event(&mut self) -> RonEvent<'a> {
        loop {
            match self.stack.last() {
                Some(InternalState::Map) => {
                    match self.next_token() {
                        Some(Token::Comma) => {},
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ',' or '}}', got EOF!"),
                    }

                    match self.next_token() {
                        Some(Token::RCurly) => {
                            self.stack.pop();
                            return RonEvent::MapEnd;
                        },
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected '}}' when closing the map, got EOF!"),
                    }

                    let key = self.try_value().expect("Expected key in map!");

                    assert!(self.next_token() == Some(Token::Colon), "Expected ':'");
                    
                    self.stack.push(InternalState::SecondValue);
                    return key;
                },
                Some(InternalState::Struct { name }) => {
                    let name = *name;
                    match self.next_token() {
                        Some(Token::Comma) => {},
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ',' or ')', got EOF!"),
                    }

                    match self.next_token() {
                        Some(Token::RParen) => {
                            self.stack.pop();
                            return RonEvent::StructEnd { name };
                        },
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ')' when closing the struct, got EOF!"),
                    }

                    let ident = match self.next_token() {
                        Some(Token::Ident(a, b)) => self.lexer.get_string(a, b),
                        x => panic!("Expected IDENTIFIER, got {x:?}"),
                    };

                    assert!(self.next_token() == Some(Token::Colon), "Expected ':'");
                    
                    self.stack.push(InternalState::SecondValue);
                    return RonEvent::NamedField(ident);
                },
                Some(InternalState::SecondValue) => {
                    self.stack.pop();
                    return self.try_value().expect("Expected value!");
                }
                Some(InternalState::Tuple { name }) => {
                    let name = *name;
                    match self.next_token() {
                        Some(Token::Comma) => {},
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ',' or ')', got EOF!"),
                    }

                    match self.next_token() {
                        Some(Token::RParen) => {
                            self.stack.pop();
                            return RonEvent::TupleEnd { name };
                        },
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ')' when closing the tuple, got EOF!"),
                    }
                    
                    return self.try_value().expect("Expected value!");
                },
                Some(InternalState::List) => {
                    match self.next_token() {
                        Some(Token::Comma) => {},
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ',' or ']', got EOF!"),
                    }

                    match self.next_token() {
                        Some(Token::RBracket) => {
                            self.stack.pop();
                            return RonEvent::ListEnd;
                        },
                        Some(x) => self.tok_queue.insert(0, x),
                        None => panic!("Expected ']' when closing the list, got EOF!"),
                    }
                    
                    return self.try_value().expect("Expected value!");
                },
                Some(InternalState::OptionalSomeValue) => {
                    self.stack.pop();
                    self.stack.push(InternalState::EndedOptionalSomeValue);
                    return self.try_value().expect("Expected value inside Some option!");
                }
                Some(InternalState::EndedOptionalSomeValue) => {
                    assert!(self.next_token() == Some(Token::RParen), "Expected ')'!");
                    self.stack.pop();
                    continue;
                }
                None => {
                    if let Some(x) = self.try_value() {
                        return x;
                    } else {
                        return RonEvent::Eof;
                    }
                },
            }
        }
    }

    fn try_value(&mut self) -> Option<RonEvent<'a>> {
        if let Some(x) = self.try_struct() {
            return Some(x);
        }
        
        if let Some(x) = self.try_tuple() {
            return Some(x);
        }
        
        if let Some(x) = self.try_primitive() {
            return Some(x);
        }
        
        if let Some(x) = self.try_optional_some() {
            return Some(x);
        }

        if let Some(x) = self.try_map() {
            return Some(x);
        }
        
        return self.try_list();
    }

    fn try_struct(&mut self) -> Option<RonEvent<'a>> {
        let ident_tok = self.next_token()?;
        let name = if let Token::Ident(a, b) = ident_tok {
            Some(self.lexer.get_string(a, b))
        } else {
            self.tok_queue.insert(0, ident_tok);
            None
        };
        
        let Some(paren_tok) = self.next_token() else {
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        let Token::LParen = paren_tok else {
            self.tok_queue.insert(0, paren_tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        let Some(field_tok) = self.next_token() else {
            self.tok_queue.insert(0, paren_tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        let Token::Ident(_, _) = field_tok else {
            self.tok_queue.insert(0, field_tok);
            self.tok_queue.insert(0, paren_tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        let Some(colon_tok) = self.next_token() else {
            self.tok_queue.insert(0, field_tok);
            self.tok_queue.insert(0, paren_tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        let Token::Colon = colon_tok else {
            self.tok_queue.insert(0, colon_tok);
            self.tok_queue.insert(0, field_tok);
            self.tok_queue.insert(0, paren_tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) }
            return None;
        };

        self.tok_queue.insert(0, colon_tok);
        self.tok_queue.insert(0, field_tok);

        self.stack.push(InternalState::Struct { name });
        return Some(RonEvent::StructStart { name });
    }

    fn try_tuple(&mut self) -> Option<RonEvent<'a>> {
        let ident_tok = self.next_token()?;
        let name = if let Token::Ident(a, b) = ident_tok {
            Some(self.lexer.get_string(a, b))
        } else {
            self.tok_queue.insert(0, ident_tok);
            None
        };
        
        if let Some(tok) = self.next_token() {
            if let Token::LParen = tok {
                self.stack.push(InternalState::Tuple { name });
                return Some(RonEvent::TupleStart { name });
            }
            self.tok_queue.insert(0, tok);
            if name.is_some() { self.tok_queue.insert(0, ident_tok) };
            return None;
        } else {
            if name.is_some() { self.tok_queue.insert(0, ident_tok) };
            return None;
        }
    }

    fn try_list(&mut self) -> Option<RonEvent<'a>> {
        let tok = self.next_token()?;
        if let Token::LBracket = tok {
            self.stack.push(InternalState::List);
            return Some(RonEvent::ListStart);
        }
        self.tok_queue.insert(0, tok);
        return None;
    }

    fn try_map(&mut self) -> Option<RonEvent<'a>> {
        let tok = self.next_token()?;
        if let Token::LCurly = tok {
            self.stack.push(InternalState::Map);
            return Some(RonEvent::MapStart);
        }
        self.tok_queue.insert(0, tok);
        return None;
    }

    fn try_optional_some(&mut self) -> Option<RonEvent<'a>> {
        let tok = self.next_token()?;
        if let Token::SomeOptValue = tok {
            assert!(self.next_token() == Some(Token::LParen), "Expected a '(' after a 'Some'!");

            self.stack.push(InternalState::OptionalSomeValue);
            return Some(RonEvent::OptionalSomeValue);
        }
        self.tok_queue.insert(0, tok);
        return None;
    }

    fn try_primitive(&mut self) -> Option<RonEvent<'a>> {
        let tok = self.next_token()?;
        match tok {
            Token::Ident(a, b) => Some(RonEvent::Primitive(RonPrimitive::Enum(self.lexer.get_string(a, b)))),
            Token::Bool(x) => Some(RonEvent::Primitive(RonPrimitive::Bool(x))),
            Token::Float(x) => Some(RonEvent::Primitive(RonPrimitive::Float(x))),
            Token::Int(x) => Some(RonEvent::Primitive(RonPrimitive::Int(x))),
            Token::Char(x) => Some(RonEvent::Primitive(RonPrimitive::Char(x))),
            Token::Str(a, b) => Some(RonEvent::Primitive(RonPrimitive::Str(self.lexer.get_string(a, b)))),
            Token::NoneOptValue => Some(RonEvent::Primitive(RonPrimitive::NoneOptValue)),
            _ => {
                self.tok_queue.insert(0, tok);
                None
            },
        }
    }

    fn next_token(&mut self) -> Option<Token> {
        if self.tok_queue.len() > 0 {
            return Some(self.tok_queue.remove(0));
        }
        return self.lexer.next_token();
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RonEvent<'a> {
    /// Signals that the next events will contain its wrapped value. There's no end event.
    OptionalSomeValue,

    /// Contains a value. There's no end event.
    Primitive(RonPrimitive<'a>),

    /// Signals the start of a struct. All values are preceded by a `NamedField` event. End event is `StructEnd`.
    StructStart { name: Option<&'a str> },

    /// Signals that the next events will contain its value. There's no end event.
    NamedField(&'a str),

    /// Signals the end of a struct. Start event is `StructStart`.
    StructEnd { name: Option<&'a str> },

    /// Signals the start of a tuple. All values are recieved in order. End event is `StructEnd`.
    TupleStart { name: Option<&'a str> },

    /// Signals the end of a tuple. Start event is `StructStart`.
    TupleEnd { name: Option<&'a str> },

    /// Signals the start of a map. All key value pairs are represented by a set of events for the key and then a set of events for the value. End event is `MapEnd`.
    MapStart,

    /// Signals the end of a map. Start event is `MapStart`.
    MapEnd,

    /// Signals the start of a list. All values are recieved in order. End event is 'ListEnd'.
    ListStart,

    /// Signals the end of a list. End event is 'ListEnd'.
    ListEnd,

    /// Signals the end of the deserialization.
    Eof
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum RonPrimitive<'a> {
    NoneOptValue, Int(i64), Float(f64), Bool(bool), Char(char), Str(&'a str), Enum(&'a str),
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn none_test() {
        let mut parser = RonDeserializer::new("None");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::NoneOptValue));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }
   
    #[test]
    fn bool_true_test() {
        let mut parser = RonDeserializer::new("true");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Bool(true)));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn bool_false_test() {
        let mut parser = RonDeserializer::new("false");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Bool(false)));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn int_test() {
        let mut parser = RonDeserializer::new("123");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(123)));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn float_test() {
        let mut parser = RonDeserializer::new("123.0");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Float(123.0)));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn char_test() {
        let mut parser = RonDeserializer::new("'a'");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Char('a')));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn str_test() {
        let mut parser = RonDeserializer::new("\"abc\"");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("abc")));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn enum_test() {
        let mut parser = RonDeserializer::new("SomeEnum");

        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("SomeEnum")));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn some_test() {
        let mut parser = RonDeserializer::new("Some(420)");

        assert_eq!(parser.next_event(), RonEvent::OptionalSomeValue);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(420)));
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn list_test() {
        let mut parser = RonDeserializer::new("[1, 2, None, 4, EnumVal, Some(6),]");
        assert_eq!(parser.next_event(), RonEvent::ListStart);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::NoneOptValue));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(4)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("EnumVal")));
        assert_eq!(parser.next_event(), RonEvent::OptionalSomeValue);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(6)));
        assert_eq!(parser.next_event(), RonEvent::ListEnd);
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn unnamed_tuple_test() {
        let mut parser = RonDeserializer::new("(1, 2, 3)");

        assert_eq!(parser.next_event(), RonEvent::TupleStart { name: None });
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(3)));
        assert_eq!(parser.next_event(), RonEvent::TupleEnd { name: None });
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn named_tuple_test() {
        let mut parser = RonDeserializer::new("Named(1, 2, 3)");

        assert_eq!(parser.next_event(), RonEvent::TupleStart { name: Some("Named") });
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(3)));
        assert_eq!(parser.next_event(), RonEvent::TupleEnd { name: Some("Named") });
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn unnamed_struct_test() {
        let mut parser = RonDeserializer::new("(first: 1, second: 2, third: 3)");

        assert_eq!(parser.next_event(), RonEvent::StructStart { name: None });
        assert_eq!(parser.next_event(), RonEvent::NamedField("first"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::NamedField("second"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::NamedField("third"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(3)));
        assert_eq!(parser.next_event(), RonEvent::StructEnd { name: None });
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn named_struct_test() {
        let mut parser = RonDeserializer::new("Named(first: 1, second: 2, third: 3)");

        assert_eq!(parser.next_event(), RonEvent::StructStart { name: Some("Named") });
        assert_eq!(parser.next_event(), RonEvent::NamedField("first"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::NamedField("second"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::NamedField("third"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(3)));
        assert_eq!(parser.next_event(), RonEvent::StructEnd { name: Some("Named") });
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn map_test() {
        let mut parser = RonDeserializer::new(r#"{ "red": 0, "green": 1, "blue": 2 }"#);
        assert_eq!(parser.next_event(), RonEvent::MapStart);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("red")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(0)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("green")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(1)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("blue")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(2)));
        assert_eq!(parser.next_event(), RonEvent::MapEnd);
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }

    #[test]
    fn nested_test() {
        let mut parser = RonDeserializer::new(r#"
        Player(
            name: "SomePlayer69",
            pos: (0.0, 0.0, 0.0),
            factions: {
                "pirates": -100,
                "alliance": 20,
                "crabs": 30,
                "neutral": 0,
            },
            powers: [Fire, Water, Ice, Air],
        )
        "#);

        assert_eq!(parser.next_event(), RonEvent::StructStart { name: Some("Player") });
        assert_eq!(parser.next_event(), RonEvent::NamedField("name"));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("SomePlayer69")));
        assert_eq!(parser.next_event(), RonEvent::NamedField("pos"));
        assert_eq!(parser.next_event(), RonEvent::TupleStart { name: None });
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Float(0.0)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Float(0.0)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Float(0.0)));
        assert_eq!(parser.next_event(), RonEvent::TupleEnd { name: None });
        assert_eq!(parser.next_event(), RonEvent::NamedField("factions"));
        assert_eq!(parser.next_event(), RonEvent::MapStart);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("pirates")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(-100)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("alliance")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(20)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("crabs")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(30)));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Str("neutral")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Int(0)));
        assert_eq!(parser.next_event(), RonEvent::MapEnd);
        assert_eq!(parser.next_event(), RonEvent::NamedField("powers"));
        assert_eq!(parser.next_event(), RonEvent::ListStart);
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("Fire")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("Water")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("Ice")));
        assert_eq!(parser.next_event(), RonEvent::Primitive(RonPrimitive::Enum("Air")));
        assert_eq!(parser.next_event(), RonEvent::ListEnd);
        assert_eq!(parser.next_event(), RonEvent::StructEnd { name: Some("Player") });
        assert_eq!(parser.next_event(), RonEvent::Eof);
    }
}