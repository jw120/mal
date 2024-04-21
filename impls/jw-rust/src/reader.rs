// Reader implemented following mal instructions

use crate::types::Mal;
use regex::Regex;

// Top-level interface to reader. Returns None if input is empty or only comments
pub fn read_str(s: &str) -> Option<Result<Mal, ReadError>> {
    let mut reader = Reader::new(s);
    // println!("tokens: {:?}", reader.tokens);
    if reader.tokens.is_empty() {
        return None;
    }
    Some(reader.read_form())
}

pub enum ReadError {
    Internal(String), // Internal errors that should not happen
    Parse(String),    // Errors called by illegal mal code
}

struct Reader<'a> {
    tokens: Vec<&'a str>,
    current: usize,
}

impl Reader<'_> {
    // Initialise a new reader
    fn new<'a>(source: &'a str) -> Reader<'a> {
        let re = Regex::new(
            r###"(?x)
            [\s,]* # Whitespace and commas not captured
            ( # Capture one of these alternatives
                ~@|                 # Special two-character ~@ token
                [\[\]{}()'`~^@]|    # Single character special token
                "(?:\\.|[^\\"])*"?| # From double-quote to next un-escaped double-quote
                ;.*|                # Anything starting with ;
                [^\s\[\]{}('"`,;)]* # Zero-or-more non-special characters
            )
            "###,
        )
        .unwrap();
        Reader {
            tokens: re
                .captures_iter(source)
                .map(|c| c.extract::<1>().1[0])
                .filter(|s| !s.is_empty() && s.chars().next() != Some(';'))
                .collect(),
            current: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Result<&str, ReadError> {
        if self.is_empty() {
            Err(ReadError::Internal("Peek on empty reader".to_string()))
        } else {
            Ok(self.tokens[self.current])
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn next(&mut self) -> Result<&str, ReadError> {
        if self.is_empty() {
            Err(ReadError::Internal("Next on empty reader".to_string()))
        } else {
            let token = self.tokens[self.current];
            self.advance();
            Ok(token)
        }
    }

    fn read_form(&mut self) -> Result<Mal, ReadError> {
        let first = self.peek()?;
        match first.chars().next() {
            Some('(') => {
                self.advance();
                self.read_list()
            }
            Some(_) => self.read_atom(),
            None => Err(ReadError::Internal("Empty token in read_form".to_string())),
        }
    }

    fn read_list(&mut self) -> Result<Mal, ReadError> {
        let mut contents: Vec<Mal> = vec![];
        loop {
            if self.is_empty() {
                return Err(ReadError::Parse(
                    "Expected close paren, found end of input".to_string(),
                ));
            }
            if self.peek()? == ")" {
                self.advance();
                return Ok(Mal::List(contents));
            }
            contents.push(self.read_form()?);
        }
    }

    fn read_atom(&mut self) -> Result<Mal, ReadError> {
        let token = self.next()?;
        match token {
            "nil" => return Ok(Mal::Nil),
            "true" => return Ok(Mal::True),
            "false" => return Ok(Mal::False),
            _ => {}
        }
        if token.chars().all(|c| c.is_ascii_digit()) {
            return Ok(Mal::Int(token.parse::<i64>().unwrap()));
        }
        if token.chars().next() == Some('"') {
            let mut s = String::new();
            let mut cs = token.chars();
            cs.next();
            let mut in_quote: bool = false;
            loop {
                match (in_quote, cs.next()) {
                    (false, Some('\\')) => in_quote = true,
                    (false, Some('\"')) => {
                        if cs.next().is_none() {
                            return Ok(Mal::String(s));
                        } else {
                            return Err(ReadError::Internal(
                                "Interior double-quote in string".to_string(),
                            ));
                        }
                    }
                    (false, Some(c)) => s.push(c),
                    (false, None) => {
                        return Err(ReadError::Parse(
                            "Expected closing double-quote, found end of input".to_string(),
                        ))
                    }
                    (true, Some('\\')) => {
                        s.push('\\');
                        in_quote = false;
                    }
                    (true, Some('n')) => {
                        s.push('\n');
                        in_quote = false;
                    }
                    (true, Some('\"')) => {
                        s.push('\"');
                        in_quote = false;
                    }
                    (true, _) => return Err(ReadError::Parse("Bad escape sequence".to_string())),
                }
            }
            // if token.len() > 1 && token.chars().last() == Some('"') {
            //     let mut s = token.to_string();
            //     s.pop();
            //     s.remove(0);
            //     return Ok(Mal::String(s));
            // }
            // return Err(ReadError::Parse(
            //     "Expected closing double-quote, found end of input".to_string(),
            // ));
        }
        Ok(Mal::Symbol(token.to_string()))
    }
}
