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

    func isErr() -> Bool {
        switch self {
        case .err:
            return true
        default:
            return false
         }
     }
}

public func read_str(_ s: String) -> ReadResult {

    // Remove leading spaces/comments (and give up if nothing to parse)
    let (state, _) = malSpaceConsumer(ParseState(Substring(s)))
    if (state.input.isEmpty) {
        return .nothing
    }

    switch expr(state) {
    case (_, .failure(let e)):
        return .err("\(e.label) at \(e.state.row),\(e.state.col)")
    case (let updatedState, .success(let val)):
        if (updatedState.input.isEmpty) {
            return .value(val)
        } else {
            return .err("Unexpected leftovers at \(updatedState.row),\(updatedState.col)")
        }
    }
}

// Handle white-space

fileprivate func isMalWhitespace(_ c: Character) -> Bool {
    return c.isWhitespace || c == ","
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

// Defining as a function rather than with let as mutually recursive lets seem to kill swift
public func expr(_ state: ParseState) -> (ParseState, ParseResult<Mal>) {
    let p = int <|> list  <|> vector <|> hashmap <|> str <|> sym
    return p(state)
}

// Integer parser

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

public let list: Parser<Mal> =
    lex({xs in .list(xs)} <^> (lex(char("(")) *> many(expr) <* lex(char(")"))))

public let vector: Parser<Mal> =
    lex({xs in .vec(xs)} <^> (lex(char("[")) *> many(expr) <* lex(char("]"))))

public let hashmap: Parser<Mal> =
    lex({xs in .hashmap(xs)} <^> (lex(char("{")) *> many(expr) <* lex(char("}"))))


// Symbol parser

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

public let str: Parser<Mal> =
    lex({cs in .str(String(cs))} <^> (char("\"") *> manyTill(anyChar, char("\""))))


