#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn from_text(text: &str, start: usize, end: usize) -> Self {
        Self {
            start: Position::from_char_index(text, start),
            end: Position::from_char_index(text, end),
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Position {
    pub line: usize,
    pub column: usize,
}

impl Position {
    pub fn from_char_index(text: &str, index: usize) -> Self {
        let bytes = text.chars().collect::<Vec<_>>();

        let mut line = 1;
        let mut column = 1;
        let mut position = 1;

        for byte in bytes {
            match byte {
                '\n' => {
                    line += 1;
                    column = 1
                }
                _ => column += 1,
            }
            position += 1;
            if position >= index {
                break;
            }
        }

        Self { line, column }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn position_from_char_index() {
        let pos = Position::from_char_index("hi there\nI am Tom", 12);
        assert_eq!(pos, Position { line: 2, column: 3 });

        let pos = Position::from_char_index("hi there\nI am Tom\nWho are you???", 25);
        assert_eq!(pos, Position { line: 3, column: 7 });
    }
}
