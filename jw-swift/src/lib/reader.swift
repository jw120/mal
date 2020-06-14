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
    // If the input is empty (only spaces), then return nothing
    switch spaces(Substring(s)) {
    case .success(_, ""):
        return .nothing
    default:
        break
    }

    // otherwise parse an expression
    switch expr(Substring(s)) {
    case ParserResult.success(let e, _):
        return .value(e)
    case ParserResult.failure(let message, _):
        return .err(message)
    }
}

public let expr: Parser<Mal> = spaces *> choice([
    int,
    list,
    symbol
])

public let digit: Parser<Character> = "Expected a digit" <!> satisfy { c in c.isASCII && c.isNumber }

public func int(_ input: Substring) -> ParserResult<Mal> {
    var sign = 1
    var digitInput = input
    switch char("-")(input) {
    case .success(_, let remaining):
        sign = -1
        digitInput = remaining
    default:
        break
    }

    switch many1(digit)(digitInput) {
    case .success(let cs, let remaining):
        if let i = Int(String(cs)) {
            return .success(.int(i * sign), remaining)
        }
        print("Internal error - digits should be an int")
        abort()
    case .failure(_, let remaining):
        return .failure("Expected an integer", remaining)
    }
}

public func list(_ s: Substring) -> ParserResult<Mal> {
    let r = between(many(expr), open: char("("), close: spaces *> char(")"))(s)
    switch r {
    case .success(let xs, let remaining):
        return .success(Mal.list(xs), remaining)
    case .failure(let msg, let remaining):
        return .failure(msg, remaining)
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
