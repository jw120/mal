// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

import Foundation

enum ReadResult {
    case value(Mal)
    case err(String)
    case nothing
}

func read_str(_ s: String) -> ReadResult {
    switch expr(Substring(s)) {
    case ParserResult.success(let e, _):
        return .value(e)
    case ParserResult.failure(let message, _):
        return .err(message)
    }
}

let expr: Parser<Mal> = spaces *> choice(
    int,
    list
)

private func digitsToMal(_ cs: [Character]) -> Mal {
    if let i = Int(String(cs)) {
        return .int(i)
    }
    print("Internal error - digits should be an int")
    abort()
}

let int: Parser<Mal> = digitsToMal <^> many1(digit)

let digit: Parser<Character> = satisfy { c in c.isASCII && c.isNumber }

let list: Parser<Mal> = { es in Mal.list(es) } <^> between(open: char("("), close: char(")"), contents: many(expr))
