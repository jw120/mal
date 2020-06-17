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

// Combinators implements
// Here     Haskell
// <^>      <$>     Apply function to returned value
// <*>      <*>             (need's a curried function)
// <|>      <|>     Alternatives (no back-tracking). Return error from parser that consumed more
// *>       *>      Combine parsers, discarding success value from first
// <*       <*      Combine parsers, discarding success value from second
// <^       <$      Replace value of success
// <!>

// <?> NYI   Same as Megaparsec <?>    Replace the lookingFor

// Available operator character: / = - + ! * % < > & | ^ ~ ?

// many
// many1
// manyTill
// eof
// optional

// why equatable?
public typealias Parser<T: Equatable> = (ParseState) -> (ParseState, Result<T, ParseError>)

struct ParseState {
    var row: Int
    var col: Int
    var input: Substring

    init(s: String) {
        row = 0
        col = 0
        input = s
    }

    mutating func advance(n: Int = 1) {
        for c in input.prefix(n) {
            if c.isNewline {
                row += 1
                col = 0
            } else {
                col += 1
            }
        }
        input.dropFirst(n)
    }

}


struct ParseError: Error {

    let state: ParseState
    let label: String // Megaparsec (label) suggests to be used only when parserFails without consuming any input?
}


//
// Combinators
//

// Apply function to parser's success result
infix operator <^> MultiplicationPrecedence
func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    (state: PareserState) -> ParserResult<T2> in
        let r = p(state)
        switch r {
        case (updatedState, .success(let val)):
            return (updatedState, .success(f(val))
        case (_, .failure):
            return r
        }
    }
}

infix operator <*> MultiplicationPrecedence
func <*> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    NYI
}


// Alternative
infix operator <|> AdditionPrecedence
func <|> <T> (_ p1: @escaping Parser<T>, _ p2: @escaping Parser<T>) -> Parser<T> { {
    (state: ParserState) -> ParserResult<T2> in
        let r1 = p1(state)
        switch r1 {
        case (_, .success)
            return r1
        case (let state1, .failure(let err1)):
            let r2 = p2(state1):
            switch r2 {
                case (_, .success):
                    return r2
                case (let state2, .failure(let err2)):
                    // Return failure from the parser that consumed the most input
                    let consumed1 = state1.input.startIndex - state.input.startIndex
                    let consumed2 = state2.input.startIndex - state1.input.startIndex
                    return (state2, consumed1 > consumed2 ? .failure(err1) : .failure(err2))
            }
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *> MultiplicationPrecedence
func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> { {
    (state: ParserState) -> ParserResult<T2> in
        let r1 = p1(state)
        switch r1 {
        case (let state1, .success):
            return p2(state1)
        case (_, .failure)
            return r1
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the second result
infix operator <* MultiplicationPrecedence
func <* <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (state: ParserState) -> ParserResult<T1> in
        let r1 = p1(state)
        switch r1 {
        case (_, .failure):
            return r1
        case (state1, .success(let val1)):
            let r2 = p1(state1)
            switch r2 {
            case (_, .failure)
                return r2
            case (let state2, .success):
                return (state2, .success(val1))
            }
        }
    }
}

// Replace success value
infix operator <^ AdditionPrecedence
func <^ <T1, T2> (_ t: T1, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (state: ParserState) -> ParserResult<T1> in
        let r2 = p2(state)
        switch r2 {
        case (state2, .success)
            return (state2, .success(t))
        case (_, .failure)
            return r2
        }
    }
}

// Change the error of the given parser
infix operator <!>
func <!> <T> (_ e: ParseError, p: @escaping Parser<T>) -> Parser<T> { {
    (state: ParserState) -> ParserResult<T> in
        let r = p(state)
        switch r {
        case (_, .success):
            return r
        case (let updatedState, .failure)
            return (updatedState, e)
        }
    }
}

func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (state: ParserState) -> ParserResult<[T]> in
        var state = state
        var results: [T] = []
        while true {
            switch p(state) {
            case (let updatedState, .success(let val)):
                results.append(val)
                state = updatedState
            case (let updatedState, .failure):
                return (updatedState, .success(results))
            }
        }
    }
}

func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (state: ParserState) -> ParserResult<[T]> in
        var state = state
        var results: [T] = []
        while true {
            switch p(state) {
            case (let updatedState, .success(let val)):
                results.append(val)
                state = updatedState
            case (let updatedState, .failure(let err)):
                return (updatedState, results.isEmpty ? .failure(err) ? .success(results))
            }
        }
    }
}

fun optional<T>(_ p: @escaping Parser<T>) -> Parser<T?> { {
    (state: ParserState) -> ParserResult<T?> in
        let r = p(state)
        switch p(state) {
            case (_ , .success):
                return r
            case (let updatedState, .failure)
                return (updatedState, .success(nil))
        }
    }
}

func eof(_ state: ParserState) -> ParserResult<Void> {
    if state.input.isEmpty {
        return (state, .success(()))
    }
    return (state, .failure(state: state, label: "Expected EOF"))
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
    (state: ParserState) -> ParserResult<Character> in
        guard let firstChar = state.input.first && firstChar == c else {
            return (state, .failure(state: state, label: "Expected '\(c)''"))
        }
        return (state.advance(), .success(firstChar))
    }
}

func anyChar(_ state: ParserState) -> ParserResult<Character> {
    guard state.input.isEmpty else {
        return (state, .failure(state: state, label: "Expected any character"))
    }
    return (state.advance(), .success(input.first))
}

func string(_ s: String) -> Parser<String> { {
    (state: ParserState) -> ParserResult<String> in
        if state.input.hasPrefix(s) {
            return (state.advance(s.count)), .success(s))
        }
        return .failure("Expected '\(s)'", input)
    }
}

func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (state: ParserState) -> ParserResult<Character> in
        if let firstChar = input.first  && f(firstChar) {
                return (state.advance(), .success(firstChar))
        }
        return (state, .failure(state: state, label: "Expectation not satisfied")
    }
}

let eol = satisfy {c in c.isNewline }
