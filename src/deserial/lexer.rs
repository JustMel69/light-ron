use std::str::CharIndices;

pub struct Lexer<'a> {
    src: &'a str,
    iter: CharIndices<'a>,
    trailing: Option<(usize, char)>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        return Self { src, iter: src.char_indices(), trailing: None };
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.ignore_whitespaces()?;

        let (char_byte, char) = self.next_char()?;
        return Some(match char {
            '(' => Token::LParen,
            ')' => Token::RParen,
            '[' => Token::LBracket,
            ']' => Token::RBracket,
            '{' => Token::LCurly,
            '}' => Token::RCurly,
            ':' => Token::Colon,
            ',' => Token::Comma,
            '"' => self.read_string()?,
            '\'' => Token::Char(self.read_char()?),
            '0'..='9' | '-' => match self.read_number(char_byte)? {
                Number::Int(x) => Token::Int(x),
                Number::Float(x) => Token::Float(x),
            },
            _ => {
                let ident = self.read_ident(char_byte)?;
                match &self.src[ident.0..ident.1] {
                    "false" => Token::Bool(false),
                    "true" => Token::Bool(true),
                    "Some" => Token::SomeOptValue,
                    "None" => Token::NoneOptValue,
                    _ => Token::Ident(ident.0, ident.1), 
                }
            },
        });
    }

    pub fn get_string(&self, start: usize, end: usize) -> &'a str {
        return &self.src[start..end];
    }

    fn ignore_whitespaces(&mut self) -> Option<()> {
        let mut val = self.next_char()?;
        while val.1.is_whitespace() {
            val = self.iter.next()?;
        }

        self.trailing = Some(val);
        return Some(());
    }

    fn next_char(&mut self) -> Option<(usize, char)> {
        if let Some(trailing) = self.trailing {
            let val = Some(trailing);
            self.trailing = None;
            return val;
        }

        return self.iter.next();
    }

    fn read_string(&mut self) -> Option<Token> {
        let start = self.next_char()?;
        if start.1 == '"' {
            return Some(Token::Str(0, 0));
        }

        let mut val = self.next_char()?;
        while val.1 != '"' {
            val = self.next_char()?;
        };

        return Some(Token::Str(start.0, val.0));
    }

    fn read_char(&mut self) -> Option<char> {
        let start = self.next_char()?;
        if start.1 == '\'' {
            panic!("Char was empty!");
        }

        let end = self.next_char()?;
        if end.1 != '\'' {
            panic!("More than one char inside char!") // TODO: Implement char escapign
        }

        return Some(start.1);
    }

    fn read_number(&mut self, start_byte: usize) -> Option<Number> {
        // TODO: Add support for 0x, 0b and 0o.

        let mut last_byte = self.src.len();
        while let Some(val) = self.next_char() {
            if val.1.is_numeric() || val.1 == '.' {
                continue;
            }
            self.trailing = Some(val);
            last_byte = val.0;
            break;
        }

        let str = &self.src[start_byte..last_byte];
        if let Ok(x) = str.trim().parse::<i64>() {
            return Some(Number::Int(x));
        }

        if let Ok(x) = str.trim().parse::<f64>() {
            return Some(Number::Float(x));
        }

        panic!("Invalid number (got \"{str}\")!");
    }

    fn read_ident(&mut self, start_byte: usize) -> Option<(usize, usize)> {
        let mut last_byte = self.src.len();
        while let Some(val) = self.next_char() {
            if val.1.is_alphanumeric() || val.1 == '_' {
                continue;
            }
            self.trailing = Some(val);
            last_byte = val.0;
            break;
        }
        return Some((start_byte, last_byte));
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Token {
    LParen, RParen, LBracket, RBracket, LCurly, RCurly, Colon, Comma, 
    Ident(usize, usize), Bool(bool), Float(f64), Int(i64), Char(char), Str(usize, usize), SomeOptValue, NoneOptValue,
}

enum Number {
    Int(i64), Float(f64),
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_next_token() {
        let src = r#" ( ) [ ] { } : , true Test false Some None 123.456 "text" 69420 -69420 'a'"#;
        let mut lexer = Lexer::new(src);
        assert_eq!(lexer.next_token(), Some(Token::LParen));
        assert_eq!(lexer.next_token(), Some(Token::RParen));
        assert_eq!(lexer.next_token(), Some(Token::LBracket));
        assert_eq!(lexer.next_token(), Some(Token::RBracket));
        assert_eq!(lexer.next_token(), Some(Token::LCurly));
        assert_eq!(lexer.next_token(), Some(Token::RCurly));
        assert_eq!(lexer.next_token(), Some(Token::Colon));
        assert_eq!(lexer.next_token(), Some(Token::Comma));
        assert_eq!(lexer.next_token(), Some(Token::Bool(true)));
        assert_eq!(lexer.next_token(), Some(Token::Ident(22, 26)));
        assert_eq!(lexer.next_token(), Some(Token::Bool(false)));
        assert_eq!(lexer.next_token(), Some(Token::SomeOptValue));
        assert_eq!(lexer.next_token(), Some(Token::NoneOptValue));
        assert_eq!(lexer.next_token(), Some(Token::Float(123.456)));
        assert_eq!(lexer.next_token(), Some(Token::Str(52, 56)));
        assert_eq!(lexer.next_token(), Some(Token::Int(69420)));
        assert_eq!(lexer.next_token(), Some(Token::Int(-69420)));
        assert_eq!(lexer.next_token(), Some(Token::Char('a')));
        assert_eq!(lexer.next_token(), None);
    }

    #[test]
    fn none() {
        let mut lexer = Lexer::new("None");
        assert_eq!(lexer.next_token(), Some(Token::NoneOptValue));
        assert_eq!(lexer.next_token(), None);
    }
}