// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

import Foundation

public enum ReadResult {
    case value(Mal)
    case err(String)
    case nothing
}

public func read_str(_ s: String) -> ReadResult {
    switch expr(Substring(s)) {
    case ParserResult.success(let e, _):
        return .value(e)
    case ParserResult.failure(let message, _):
        return .err(message)
    }
}

public let expr: Parser<Mal> = spaces *> choice([
    int,
    list
])

private func digitsToMal(_ cs: [Character]) -> Mal {
    if let i = Int(String(cs)) {
        return .int(i)
    }
    print("Internal error - digits should be an int")
    abort()
}

public let int: Parser<Mal> = "Expected an integer" <!> (digitsToMal <^> many1(digit))

public let digit: Parser<Character> = "Expected a digit" <!> satisfy { c in c.isASCII && c.isNumber }

//public let list: Parser<Mal> = { es in Mal.list(es) } <^>
//    between(many(expr), open: char("("), close: spaces *> char(")"))

public func list(_ s: Substring) -> ParserResult<Mal> {
    let r = between(many(expr), open: char("("), close: spaces *> char(")"))(s)
    switch r {
    case .success(let xs, let remaining):
        return .success(Mal.list(xs), remaining)
    case .failure(let msg, let remaining):
        return .failure(msg, remaining)
    }
}
