// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser_char - Character-related parsers for DIY parser combinator library.
//
// char
// anyChar
// string
// satisfy
// eol
//
// Lex helper
//
// lexeme

/// Return a Parser that matches the given character
public func char(_ c: Character) -> Parser<Character> { {
    (state: ParseState) -> (ParseState, ParseResult<Character>) in
        if state.input.first == .some(c) {
            return (state.advance(), .success(c))
        } else {
            return (state, .failure(ParseError(state: state, label: "Expected '\(c)'")))
        }
    }
}

/// A Parser that matches any single character
public func anyChar(_ state: ParseState) -> (ParseState, ParseResult<Character>) {
    if let c = state.input.first {
        return (state.advance(), .success(c))
    }
    return (state, .failure(ParseError(state: state, label: "Expected any character")))
}

/// Return a Parser that matches the given string (and that backtracks when failing)
public func string(_ s: String) -> Parser<String> { {
    (state: ParseState) -> (ParseState, ParseResult<String>) in
        if state.input.hasPrefix(s) {
            return (state.advance(s.count), .success(s))
        }
    return (state, .failure(ParseError(state: state, label: "Expected '\(s)'")))
    }
}

/// Return a Parser that matches any character for which the given function is true
public func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (state: ParseState) -> (ParseState, ParseResult<Character>) in
        if let firstChar = state.input.first {
            if f(firstChar) {
                return (state.advance(), .success(firstChar))
            }
        }
        return (state, .failure(ParseError(state: state, label: "Expectation not satisfied")))
    }
}

/// Parser that matches an end of line character
public let eol: Parser<Void> = "Expected end of line" <! () <^ satisfy { c in c.isNewline }

//
// Lex helper
//

/// Makes the given parser ignore any trailing space (as defined by the spaceConsumer parser, which
/// should not fail on empty input)
public func lexeme<T, U>(_ p: @escaping Parser<T>, spaceConsumer: @escaping Parser<U>) -> Parser<T> {
    p <* spaceConsumer
}
