// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

import parser

public func read_str(_ s: String) -> String {
    switch expr(Substring.init(s)) {
        case ParserResult.success(let e, let remaining):
            return e
        case ParserFailure.failure(let failMessage, let failPoint):
           return failMessage
    }
}

let expr : Parser<String> = spaces *> choice [
    int,
    list
]

let int : Parser<Int> =  { (cs) -> Int(String(s))! } <$1> many1(digit)

let digit : Parser<Character> = satisfy { (c) -> c.isAscii() && c.isNumber() }

let list : Parser<[Int]> = between(open: char("("), close: char(")"), contents: many(expr))

