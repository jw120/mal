// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser - DIY parser combinator library

// Revised version
// * Tuple as type
// * Non backtracking
// * Try to do better error messages (start of parse that fails)
// * All in on combinators
// * Follow megaparsec

// TODO
// eof
// try?

// Combinators implements
// Here     Haskell
// <^>      <$>     Apply function to returned value
// <*>      <*>             (need's a curried function)
// <|>      <|>     Alternatives (no back-tracking). Return error from parser that consumed more
// *>       *>      Combine parsers, discarding success value from first
// <*       <*      Combine parsers, discarding success value from second
// <^       <$      Replace value of success


// <!>
// <?>    Same as Megaparsec <?>    Replace the lookingFor

// Available operator character: / = - + ! * % < > & | ^ ~ ?

// many
// many1
// manyTill
// optional
// try


public typealias Parser<T: Equatable> = (Substring) -> (Substring, Result<T, ParseError>)

enum ParseErrorKind {
    case eof // Found EOF unexpectedly
    case unexpected // Found non-EOF unexpectedly whe looking for string
}

struct ParseError: Error {
    let kind: ParseErrorKind
    let lookingFor: Substring // Megaparsec (label) suggests to be used only when parserFails without consuming any input?
    let startedAt: SubString?
    let failedAt: Substring
}

//
// Combinators
//

// Apply function to parser's success result
infix operator <^> MultiplicationPrecedence
func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        let r = p(input)
        switch r {
        case (let rest, .success(let val)):
            return (rest, .success(f(val))
        case (_, .failure):
            return r
        }
    }
}

// Alternative
infix operator <|> AdditionPrecedence
func <|> <T> (_ p1: @escaping Parser<T>, _ p2: @escaping Parser<T>) -> Parser<T> { {
    (input: Substring) -> ParserResult<T2> in
        let r1 = p1(input)
        switch r1 {
        case (_, .success)
            return r1
        case (let rest1, .failure(let err1)):
            let r2 = p2(rest1):
            switch r2 {
                case (_, .success):
                    return r2
                case (let rest2, .failure(let err2)):
                    // Return failure from the parser that consumed the most input
                    let consumed1 = rest1.startIndex - input.startIndex
                    let consumed2 = rest2.startIndex - rest1.startIndex
                    return consumed1 > consumed2 ? .failure(rest2, err1) : .failure(rest2, err2)
            }
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *> MultiplicationPrecedence
func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        let r1 = p1(input)
        switch r1 {
        case (let rest, .success):
            return p2(rest)
        case (_, .failure)
            return r1
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the second result
infix operator <* MultiplicationPrecedence
func <* <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (input: Substring) -> ParserResult<T1> in
        let r1 = p1(input)
        switch r1 {
        case (_, .failure):
            return r1
        case (rest1, .success(let val1)):
            let r2 = p1(rest1)
            switch r2 {
            case (_, .failure)
                return r2
            case (let rest2, .success):
                return (rest2, .success(val1))
            }
        }
    }
}

// Change the error of the given parser
infix operator <!>
func <!> <T> (_ e: ParseError, p: @escaping Parser<T>) -> Parser<T> { {
    (input: Substring) -> ParserResult<T> in
        let r = p(input)
        switch r {
        case (_, .success):
            return r
        case (let rest, .failure)
            return (rest, e)
        }
    }
}

func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (input: Substring) -> ParserResult<[T]> in
        var results: [T] = []
        var remaining = input
        while true {
            switch p(remaining) {
            case (let rest, .success(let val)):
                results.append(val)
                remaining = rest
            case (let rest, .failure):
                return (rest, .success(results))
            }
        }
    }
}

func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (input: Substring) -> ParserResult<[T]> in
        let r = many(p)(input)
        switch r {
            case (let rest, .success(let xs)):
                return xs.isEmpty ? p("") : r
            case (_, .failure):
                return r
        }
    }
}

//
// Lex helpers
//

// Make the given parser ignore any trailing space (as defined by the spaceConsumer parser, which
// should not fail on empty input)
let lexeme<T, U>: (_ p: Parser<T>, spaceConsumer: Parser<U>) -> Parser<T> =
    p <* spaceConsumer

//
// Character and string parsers
//

func char(_ c: Character) -> Parser<Character> { {
    (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if firstChar == c {
                return .success(firstChar, input.dropFirst())
            }
        }
        return .failure("Expected '\(c)'", input)
    }
}

func anyChar(_ input: Substring) -> ParserResult<Character> {
    if input.isEmpty {
        return .failure("Expected any character")
    } else {
        return .success()
    }
}

func string(_ s: String) -> Parser<String> { {
    (input: Substring) -> ParserResult<String> in
        if input.hasPrefix(s) {
            return .success(s, input.dropFirst(s.count))
        }
        return .failure("Expected '\(s)'", input)
    }
}

// satisfy
func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if f(firstChar) {
                return .success(firstChar, input.dropFirst())
            }
        }
        return .failure("Expectation not satisfied", input)
    }
}
