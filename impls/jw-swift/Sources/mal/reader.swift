// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

import Foundation

public enum ReadResult: Equatable {
    case value(Mal)
    case err(String)
    case nothing

    public func isErr() -> Bool {
        switch self {
        case .err:
            return true
        default:
            return false
        }
    }
}

/// Attempt to convert the String into a Mal value. May return an error if
/// the input is mal-formed or nothing if there is nothing there (except
/// Mal white space or comments)
public func read_str(_ s: String) -> ReadResult {
    // Remove leading spaces/comments (and give up if nothing to parse)
    let (state, _) = malSpaceConsumer(ParseState(Substring(s)))
    if state.input.isEmpty {
        return .nothing
    }

    let (updatedState, result) = expr(state)
    switch result {
    case failure(let e):
        return .err("\(e.label) at \(e.state.row),\(e.state.col)")
    case .success(let val):
        if updatedState.input.isEmpty {
            return .value(val)
        } else {
            return .err("Unexpected leftovers at \(updatedState.row),\(updatedState.col)")
        }
    }
}

// Handle white-space

fileprivate func isMalWhitespace(_ c: Character) -> Bool {
    c.isWhitespace || c == ","
}

fileprivate let malWhitespace: Parser<Void> =
    "Expected whitespace" <! () <^ satisfy(isMalWhitespace(_:))

fileprivate let comment: Parser<Void> =
    "Expected a comment" <! char(";") *> (() <^ manyTill(anyChar, eol <|> eof))

fileprivate let malSpaceConsumer: Parser<Void> = () <^ many(malWhitespace <|> comment)

// Wraps a parser so it skips trailing white space (including comments)
fileprivate func lex<T>(_ p: @escaping Parser<T>) -> Parser<T> {
    lexeme(p, spaceConsumer: malSpaceConsumer)
}

// Top-level parser

/// Parser that matches a Mal expression
public func expr(_ state: ParseState) -> (ParseState, ParseResult<Mal>) {
    // Defining as a function rather than with let as mutually recursive lets seem to kill swift
    let p = int <|> list  <|> vector <|> hashmap <|> str <|> sym
    return p(state)
}

// Integer parser

/// Parser that matches a Mal integer
public let int: Parser<Mal> =
    lex(intCombine <^> negativeSign <*> many1(digit))

fileprivate let negativeSign: Parser<Character?> =
    optional(char("-"))

fileprivate let digit: Parser<Character> =
    "Expected a digit" <! satisfy { c in c.isASCII && c.isNumber }

fileprivate func intCombine(_ negative: Character?) -> ([Character]) -> Mal { {
    (cs: [Character]) -> Mal in
        switch (negative, Int(String(cs))) {
        case (.none, .some(let val)):
            return .int(val)
        case (.some, .some(let val)):
            return .int(-val)
        case (_, .none):
            fatalError("fileprivate error - digits should be an int")
        }
    }
}

// Collection parsers

/// Parser that matches a Mal list
public let list: Parser<Mal> =
    lex({ xs in .list(xs) } <^> (lex(char("(")) *> many(expr) <* lex(char(")"))))

/// Parser that matches a Mal vector
public let vector: Parser<Mal> =
    lex({ xs in .vec(xs) } <^> (lex(char("[")) *> many(expr) <* lex(char("]"))))

/// Parser that matches a Mal hashmap
public let hashmap: Parser<Mal> =
    lex({ xs in .hashmap(xs) } <^> (lex(char("{")) *> many(expr) <* lex(char("}"))))

// Symbol parser

/// Parser that matches a Mal symbol
public let sym: Parser<Mal> =
    lex(symCombine <^> many1(satisfy(isNormalChar)))

fileprivate func isNormalChar(_ c: Character) -> Bool {
    !isMalWhitespace(c) && !"[]{}()'`~^@".contains(c)
}

fileprivate func symCombine(_ cs: [Character]) -> Mal {
    let symStr = String(cs)
    switch symStr {
    case "true":
        return .bool(true)
    case "false":
        return .bool(false)
    case "nil":
        return .null
    default:
        return .sym(symStr)
    }
}

// String parser

/// Parser that matches a Mal string
public let str: Parser<Mal> =
    lex({ cs in .str(String(cs)) } <^> (char("\"") *> manyTill(anyChar, char("\""))))
