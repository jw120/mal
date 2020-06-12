// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

public enum ReadResult {
    case value(Mal)
    case err(String)
    case nothing
}

public func read_str(_ s: String) -> ReadResult {
    switch expr(Substring.init(s)) {
        case ParserResult.success(let e, _):
            return .value(e)
        case ParserResult.failure(let message, _):
            return .err(message)
    }
}

let expr : Parser<Mal> = spaces *> choice(
    int,
    list
)

let int : Parser<Mal> =  { cs in Mal.int(Int(String(cs))!) } <^> many1(digit)

let digit : Parser<Character> = satisfy({ c in c.isASCII && c.isNumber })

let list : Parser<Mal> = {es in Mal.list(es)} <^> between(open: char("("), close: char(")"), contents: many(expr))

