// Reader implemented following mal instructions
// Used from step 1 onwards

use regex::Regex;
use std::collections::HashMap;
use std::rc::Rc;

use crate::types::{err, into_key, into_mal_hashmap, Mal, MalError, MalKey, MalResult};

// Top-level interface to reader. Returns Nil if input is empty or only comments
pub fn read_str(s: &str) -> MalResult {
    let mut reader = Reader::new(s);
    // println!("tokens: {:?}", reader.tokens);
    if reader.tokens.is_empty() {
        return Ok(Mal::Nil);
    }
    reader.read_form()
}

// Implementation based on mutable reader object
struct Reader<'a> {
    tokens: Vec<&'a str>,
    current: usize,
}

impl Reader<'_> {
    // Initialise a new reader
    fn new(source: &str) -> Reader {
        let re = Regex::new(
            r#"(?x)
            [\s,]* # Whitespace and commas not captured
            ( # Capture one of these alternatives
                ~@|                 # Special two-character ~@ token
                [\[\]{}()'`~^@]|    # Single character special token
                "(?:\\.|[^\\"])*"?| # From double-quote to next un-escaped double-quote
                ;.*|                # Anything starting with ;
                [^\s\[\]{}('"`,;)]* # Zero-or-more non-special characters
            )
            "#,
        )
        .unwrap();
        Reader {
            tokens: re
                .captures_iter(source)
                .map(|c| c.extract::<1>().1[0])
                .filter(|s| !s.is_empty() && !s.starts_with(';'))
                .collect(),
            current: 0,
        }
    }

    fn is_empty(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Result<&str, MalError> {
        if self.is_empty() {
            err("Peek on empty reader")
        } else {
            Ok(self.tokens[self.current])
        }
    }

    fn advance(&mut self) {
        self.current += 1;
    }

    fn next(&mut self) -> Result<&str, MalError> {
        if self.is_empty() {
            err("Next on empty reader")
        } else {
            let token = self.tokens[self.current];
            self.advance();
            Ok(token)
        }
    }

    fn read_form(&mut self) -> MalResult {
        let first = self.peek()?;
        match first.chars().next() {
            Some('(') => {
                self.advance();
                self.read_seq(")")
                    .map(|xs| Mal::Seq(true, xs, Rc::new(Mal::Nil)))
            }
            Some('[') => {
                self.advance();
                self.read_seq("]")
                    .map(|xs| Mal::Seq(false, xs, Rc::new(Mal::Nil)))
            }
            Some('{') => {
                self.advance();
                let ys = self.read_seq("}")?;
                let mut ys_iter = ys.iter();
                let mut m: HashMap<MalKey, Mal> = HashMap::new();
                loop {
                    match (ys_iter.next(), ys_iter.next()) {
                        (Some(key), Some(value)) => match into_key(key) {
                            Some(k) => m.insert(k, value.clone()),
                            None => return err("Bad key type in hash-map"),
                        },
                        (Some(_), None) => {
                            return Ok(into_mal_hashmap(m));
                        }
                        _ => return Ok(into_mal_hashmap(m)),
                    };
                }
            }
            Some(_) => self.read_atom(),
            None => err("Empty token in read_form"),
        }
    }

    fn read_seq(&mut self, closing: &str) -> Result<Rc<Vec<Mal>>, MalError> {
        let mut contents: Vec<Mal> = vec![];
        loop {
            if self.is_empty() {
                return err("Expected sequence close, found end of input");
            }
            if self.peek()? == closing {
                self.advance();
                return Ok(Rc::new(contents));
            }
            contents.push(self.read_form()?);
        }
    }

    fn read_atom(&mut self) -> MalResult {
        let token = self.next()?;
        match token {
            "nil" => return Ok(Mal::Nil),
            "true" => return Ok(Mal::Bool(true)),
            "false" => return Ok(Mal::Bool(false)),
            _ => {}
        }
        if token.chars().all(|c| c.is_ascii_digit())
            || (token.starts_with('-')
                && token.len() > 1
                && token.chars().skip(1).all(|c| c.is_ascii_digit()))
        {
            return Ok(Mal::Int(token.parse::<i64>().unwrap()));
        }
        if token.starts_with('"') {
            let mut s = String::new();
            let mut cs = token.chars();
            cs.next();
            let mut in_quote: bool = false;
            loop {
                match (in_quote, cs.next()) {
                    (false, Some('\\')) => in_quote = true,
                    (false, Some('\"')) => {
                        return if cs.next().is_none() {
                            Ok(Mal::String(s))
                        } else {
                            err("Interior double-quote in string")
                        };
                    }
                    (false, Some(c)) => s.push(c),
                    (false, None) => {
                        return err("Expected closing double-quote, found end of input");
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
                    (true, _) => return err("Bad escape sequence"),
                }
            }
        }
        if token.starts_with(':') {
            if token.len() == 1 {
                return err("Empty keyword name");
            }
            let rest = token.to_string().split_off(1);
            return Ok(Mal::Keyword(rest));
        }
        match token {
            "'" => Ok(Mal::Seq(
                true,
                Rc::new(vec![Mal::Symbol("quote".to_string()), self.read_form()?]),
                Rc::new(Mal::Nil),
            )),
            "`" => Ok(Mal::Seq(
                true,
                Rc::new(vec![
                    Mal::Symbol("quasiquote".to_string()),
                    self.read_form()?,
                ]),
                Rc::new(Mal::Nil),
            )),
            "~" => Ok(Mal::Seq(
                true,
                Rc::new(vec![Mal::Symbol("unquote".to_string()), self.read_form()?]),
                Rc::new(Mal::Nil),
            )),
            "@" => Ok(Mal::Seq(
                true,
                Rc::new(vec![Mal::Symbol("deref".to_string()), self.read_form()?]),
                Rc::new(Mal::Nil),
            )),
            "~@" => Ok(Mal::Seq(
                true,
                Rc::new(vec![
                    Mal::Symbol("splice-unquote".to_string()),
                    self.read_form()?,
                ]),
                Rc::new(Mal::Nil),
            )),
            "^" => {
                let meta = self.read_form()?;
                let value = self.read_form()?;
                Ok(Mal::Seq(
                    true,
                    Rc::new(vec![Mal::Symbol("with-meta".to_string()), value, meta]),
                    Rc::new(Mal::Nil),
                ))
            }
            _ => Ok(Mal::Symbol(token.to_string())),
        }
    }
}
