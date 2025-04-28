#![allow(dead_code)]

use tokens::{Token, TokenType};

mod span;
mod tokens;

struct Lexer {
    content: String,
    position: usize,
}

impl Lexer {
    fn new(program: String) -> Self {
        Lexer {
            content: program,
            position: 0,
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

    fn make_single_char_token(&mut self, kind: TokenType) -> Token {
        let tok = Token::new(kind, self.position, 1);
        self.advance(1);
        tok
    }

    fn make_double_char_token(&mut self, kind: TokenType) -> Token {
        let tok = Token::new(kind, self.position, 2);
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

    fn read_int(&mut self) -> usize {
        let initial_position = self.position;
        self.advance(1);
        while matches!(self.peek(0), Some('0'..='9')) {
            self.advance(1);
        }
        self.content[initial_position..self.position]
            .to_string()
            .parse::<usize>()
            .unwrap()
    }

    fn skip_whitespace(&mut self) {
        while matches!(self.peek(0), Some(' ' | '\r' | '\t')) {
            self.advance(1);
        }
    }

    pub fn next_token(&mut self) -> Token {
        use tokens::TokenType::*;

        self.skip_whitespace();

        let Some(cur_char) = self.peek(0) else {
            return Token::new(Eof, self.position, 1);
        };

        match cur_char {
            '!' => match self.peek(1) {
                Some('=') => self.make_double_char_token(NotEqual),
                _ => self.make_single_char_token(Not),
            },
            '+' => self.make_single_char_token(Plus),
            '-' => match self.peek(1) {
                Some('-') => {
                    self.advance(2);
                    while let Some(c) = self.peek(0) {
                        if c == '\n' {
                            break;
                        }
                        self.advance(1);
                    }
                    self.next_token()
                }
                Some('>') => self.make_double_char_token(RArrow),
                _ => self.make_single_char_token(Minus),
            },
            '/' => self.make_single_char_token(Divide),
            '*' => self.make_single_char_token(Multiply),
            '^' => self.make_single_char_token(Exponent),
            '=' => self.make_single_char_token(Equal),
            '<' => match self.peek(1) {
                Some('-') => self.make_double_char_token(LArrow),
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
            ':' => self.make_single_char_token(Colon),
            ',' => self.make_single_char_token(Comma),
            '\n' => self.make_single_char_token(Newline),
            'a'..='z' | 'A'..='Z' | '_' => {
                let initial_position = self.position;
                let ident = self.read_ident();
                match ident.as_str() {
                    "val" => Token::new(Val, initial_position, 3),
                    "mut" => Token::new(Mut, initial_position, 3),
                    "fun" => Token::new(Fun, initial_position, 3),
                    s => Token::new(Identifier(s.to_string()), initial_position, s.len()),
                }
            }
            '0'..='9' => {
                let initial_position = self.position;
                let int = self.read_int();
                Token::new(
                    IntLiteral(int),
                    initial_position,
                    self.position - initial_position + 1,
                )
            }
            c => panic!("illegal token: {c}"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokens::{TokenType, TokenType::*};

    fn expect_tok(lexer: &mut Lexer, expected: TokenType) {
        let token = lexer.next_token();
        assert_eq!(token.kind, expected);
    }

    #[test]
    fn all_syntax() {
        let input =
            "val mut fun<- lorem_ipsum123    ->(){ }:,\n -- lorem lorem \n!+   - /*^= != < > <= >=\n-- comment";
        let mut lexer = Lexer::new(input.to_string());
        expect_tok(&mut lexer, Val);
        expect_tok(&mut lexer, Mut);
        expect_tok(&mut lexer, Fun);
        expect_tok(&mut lexer, LArrow);
        expect_tok(&mut lexer, Identifier("lorem_ipsum123".to_string()));
        expect_tok(&mut lexer, RArrow);
        expect_tok(&mut lexer, LParen);
        expect_tok(&mut lexer, RParen);
        expect_tok(&mut lexer, LBrace);
        expect_tok(&mut lexer, RBrace);
        expect_tok(&mut lexer, Colon);
        expect_tok(&mut lexer, Comma);
        expect_tok(&mut lexer, Newline);
        expect_tok(&mut lexer, Newline);
        expect_tok(&mut lexer, Not);
        expect_tok(&mut lexer, Plus);
        expect_tok(&mut lexer, Minus);
        expect_tok(&mut lexer, Divide);
        expect_tok(&mut lexer, Multiply);
        expect_tok(&mut lexer, Exponent);
        expect_tok(&mut lexer, Equal);
        expect_tok(&mut lexer, NotEqual);
        expect_tok(&mut lexer, LessThan);
        expect_tok(&mut lexer, GreaterThan);
        expect_tok(&mut lexer, LessThanOrEqual);
        expect_tok(&mut lexer, GreaterThanOrEqual);
        expect_tok(&mut lexer, Newline);
        expect_tok(&mut lexer, Eof);
        expect_tok(&mut lexer, Eof);
        expect_tok(&mut lexer, Eof);
    }
}
