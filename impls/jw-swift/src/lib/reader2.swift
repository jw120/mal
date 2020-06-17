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
    let (state, _) = malSpaceConsumer(ParserState(s))
    if (state.isEmpty) {
        return .nothing
    }

    switch expr(state) {
    case ("", .success(let val)):
        return .value(val)
    case (_, ParserResult.failure(let e):
        return .err("\(e.label) at \(e.state.row),\(e.state.col)")
    case (leftover, .success)
        return .err("Unexpected leftovers at \(leftover.row),\(leftover.col)")
    }
}

internal func isMalWhitespace(_ c: Character) -> Bool {
    c.isWhitespace || c == ","
}
internal let malWhitespace: Parser<Character> = "Expected whitespace" <!> satisfy(isMalWhitespace)
internal let comment: Parser<[Character]> = char(";") *> manyTill(anyChar, eol <|> eof)
internal let malSpaceConsumer: Parser<Void> = () <^ many(malWhitespace <|> comment)
internal let lex = lexeme(malSpaceConsumer)

public let expr: Parser<Mal> = lex(int <|> list <|> vector <|> hashmap <|> string <|> symbol)

public func int(_ state: ParserState) -> (ParserState, ParserResult<Mal>) {

    let negativeSign: Parser<Character?> = optional(char("-"))

    let digit: Parser<Character> = "Expected a digit" <!> satisfy { c in c.isASCII && c.isNumber }

    func combine(_ negative: Character?)(_ digits: [Character]) -> Mal {
        switch (negative, Int(String(digits))) {
            case (.none, .some(let val)):
                return .int(val)
            case (.some, .some(let val)):
                return .int(-val)
            case (_, .none):
                print("Internal error - digits should be an int")
                abort()
        }
    }

    let p : Parser<Mal> = malLexeme(combine <^> negativeSign <*> many1(digit))

    return p(input)
}

public func list(_ input: Substring) -> (Substring, Result<Mal, ParserError>) {

    func combine(_ xs: [Mal]) -> Mal {
        .list(xs)
    }
    let opener = malLexeme(char(""))
    let closer = malLexeme(char(")"))
    let p: Parser<Mal> = combine <^> (opener *> many(expr) <* closer)

    return p(input)
}

public func vector(_ s: Substring) -> ParserResult<Mal> {
    let p1 = char("[") *> many(expr)
    switch p1(s) {
    case .failure(let msg, let remaining):
        return .failure(msg, remaining)
    case .success(let xs, let remaining):
        let p2 = spaces *> char("]")
        switch p2(remaining) {
        case .success(_, let final):
            return .success(Mal.vec(xs), final)
        case .failure(_, let final):
            return .failure("Square brackets unbalanced", final)
        }
    }
}

public func hashmap(_ s: Substring) -> ParserResult<Mal> {
    let p1 = char("{") *> many(expr)
    switch p1(s) {
    case .failure(let msg, let remaining):
        return .failure(msg, remaining)
    case .success(let xs, let remaining):
        let p2 = spaces *> char("}")
        switch p2(remaining) {
        case .success(_, let final):
            return .success(Mal.hashmap(xs), final)
        case .failure(_, let final):
            return .failure("Square brackets unbalanced", final)
        }
    }
}

public func symbol(_ s: Substring) -> ParserResult<Mal> {

    let specials =  "[]{}()'`~^@"

    func isNormal(_ c: Character) -> Bool {
        !isMalSpace(c) && !specials.contains(c)
    }

    let p: Parser<[Character]> = many1(satisfy(isNormal))
    switch p(s) {
    case .success(let symChars, let rest):
        let symStr = String(symChars)
        switch symStr {
        case "true":
            return .success(.bool(true), rest)
        case "false":
            return .success(.bool(false), rest)
        case "nil":
            return .success(.null, rest)
        default:
            return .success(.sym(symStr), rest)
        }
    case .failure(let msg, let rest):
        return .failure(msg, rest)
    }

}

public func string(_ s: Substring) -> ParserResult<Mal> {

    func notDoubleQuote(_ c: Character) -> Bool {
        c != "\""
    }

    let p = char("\"") *> many(satisfy(notDoubleQuote))
    switch p(s) {
    case .failure(let msg, let rest):
        return .failure(msg, rest)
    case .success(let contents, let rest):
        switch char("\"")(rest) {
        case .success(_, let final):
            return .success(.str(String(contents)), final)
        case .failure(_, let final):
            return .failure("Double quotes unbalanced", final)
        }
    }

}
