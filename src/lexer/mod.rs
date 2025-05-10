#![allow(dead_code)]

use tokens::{Token, TokenKind};

pub mod tokens;

#[derive(Clone)]
pub struct Lexer {
    content: String,
    position: usize,
}

impl Lexer {
    pub fn new(program: &str) -> Self {
        Lexer {
            content: program.to_string(),
            position: 0,
        }
    }

    fn next_token(&mut self) -> Token {
        use tokens::TokenKind::*;

        self.skip_whitespace();

        let Some(cur_char) = self.peek(0) else {
            return Token::new(Eof, &self.content, self.position, 1);
        };

        match cur_char {
            '#' => {
                self.advance(1);
                while let Some(c) = self.peek(0) {
                    if c == '\n' {
                        break;
                    }
                    self.advance(1);
                }
                self.next_token()
            }
            '!' => match self.peek(1) {
                Some('=') => self.make_double_char_token(NotEqual),
                _ => self.make_single_char_token(Not),
            },
            '+' => self.make_single_char_token(Plus),
            '-' => self.make_single_char_token(Minus),
            '/' => self.make_single_char_token(Slash),
            '*' => self.make_single_char_token(Asterisk),
            '&' => self.make_single_char_token(BitAnd),
            '|' => self.make_single_char_token(BitOr),
            '^' => self.make_single_char_token(BitXor),
            '=' => match self.peek(1) {
                Some('=') => self.make_double_char_token(DoubleEqual),
                _ => self.make_single_char_token(Equal),
            },
            '<' => match self.peek(1) {
                Some('=') => self.make_double_char_token(LessThanOrEqual),
                _ => self.make_single_char_token(LessThan),
            },
            '>' => match self.peek(1) {
                Some('=') => self.make_double_char_token(GreaterThanOrEqual),
                _ => self.make_single_char_token(GreaterThan),
            },
            '(' => self.make_single_char_token(LParen),
            ')' => self.make_single_char_token(RParen),
            '{' => self.make_single_char_token(LBrace),
            '}' => self.make_single_char_token(RBrace),
            '~' => self.make_single_char_token(Tilde),
            ':' => self.make_single_char_token(Colon),
            ',' => self.make_single_char_token(Comma),
            ';' => self.make_single_char_token(Semicolon),
            'a'..='z' | 'A'..='Z' | '_' => {
                let initial_position = self.position + 1;
                let ident = self.read_ident();
                match ident.as_str() {
                    "fun" => Token::new(Fun, &self.content, initial_position, 3),
                    "unit" => Token::new(Unit, &self.content, initial_position, 4),
                    "true" => Token::new(True, &self.content, initial_position, 4),
                    "false" => Token::new(False, &self.content, initial_position, 5),
                    s => Token::new(
                        Identifier(s.to_string()),
                        &self.content,
                        initial_position,
                        s.len(),
                    ),
                }
            }
            '0'..='9' => {
                let initial_position = self.position + 1;
                let int = self.read_int();
                Token::new(
                    IntLiteral(int),
                    &self.content,
                    initial_position,
                    self.position - initial_position + 1,
                )
            }
            c => panic!("illegal token: {c}"),
        }
    }

    fn peek(&self, distance: usize) -> Option<char> {
        self.content.chars().nth(self.position + distance)
    }

    fn next(&mut self) -> Option<char> {
        let char = self.peek(0);
        self.position += 1;
        char
    }

    fn advance(&mut self, distance: usize) {
        self.position += distance;
    }

    fn make_single_char_token(&mut self, kind: TokenKind) -> Token {
        let tok = Token::new(kind, &self.content, self.position + 1, 1);
        self.advance(1);
        tok
    }

    fn make_double_char_token(&mut self, kind: TokenKind) -> Token {
        let tok = Token::new(kind, &self.content, self.position, 2);
        self.advance(2);
        tok
    }

    fn read_ident(&mut self) -> String {
        let initial_position = self.position;
        self.advance(1);
        while matches!(self.peek(0), Some('a'..='z' | 'A'..='Z' | '_' | '0'..='9')) {
            self.advance(1);
        }
        self.content[initial_position..self.position].to_string()
    }

    fn read_int(&mut self) -> isize {
        let initial_position = self.position;
        self.advance(1);
        while matches!(self.peek(0), Some('0'..='9')) {
            self.advance(1);
        }
        self.content[initial_position..self.position]
            .to_string()
            .parse::<isize>()
            .unwrap()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(0), Some(' ' | '\r' | '\t' | '\n')) {
            self.advance(1);
        }
    }
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.next_token();
        if tok.kind == TokenKind::Eof {
            None
        } else {
            Some(tok)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use pretty_assertions::assert_eq;
    use tokens::{TokenKind, TokenKind::*};

    fn expect_tok(lexer: &mut Lexer, expected: Vec<TokenKind>) {
        let token_kinds = lexer
            .into_iter()
            .map(|t| t.kind.clone())
            .collect::<Vec<_>>();
        assert_eq!(token_kinds, expected);
    }

    #[test]
    fn all_syntax() {
        let input = r#"
# variable declaration
foo :: 4;
bar : Int = 33;

unit;

# functions
calc :: fun (~x, ~y: Int) Int {
  z :: x / y;
  true;
  false;
  z^2
};

calc(foo, bar)"#
            .trim();
        let mut lexer = Lexer::new(input);
        expect_tok(
            &mut lexer,
            vec![
                Identifier("foo".to_string()),
                Colon,
                Colon,
                IntLiteral(4),
                Semicolon,
                Identifier("bar".to_string()),
                Colon,
                Identifier("Int".to_string()),
                Equal,
                IntLiteral(33),
                Semicolon,
                Unit,
                Semicolon,
                Identifier("calc".to_string()),
                Colon,
                Colon,
                Fun,
                LParen,
                Tilde,
                Identifier("x".to_string()),
                Comma,
                Tilde,
                Identifier("y".to_string()),
                Colon,
                Identifier("Int".to_string()),
                RParen,
                Identifier("Int".to_string()),
                LBrace,
                Identifier("z".to_string()),
                Colon,
                Colon,
                Identifier("x".to_string()),
                Slash,
                Identifier("y".to_string()),
                Semicolon,
                True,
                Semicolon,
                False,
                Semicolon,
                Identifier("z".to_string()),
                BitXor,
                IntLiteral(2),
                RBrace,
                Semicolon,
                Identifier("calc".to_string()),
                LParen,
                Identifier("foo".to_string()),
                Comma,
                Identifier("bar".to_string()),
                RParen,
            ],
        );
    }
}
