// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser - DIY parser combinator library.
//
// Modelled roughly on Megaparsec (Haskell). No backtracking

// Combinators implemented
//
// Here     Haskell (if different)
//
// <^>      <$>     Apply function to returned value
// <*>              Combine arguments to use with a (curried) function
// <|>              Alternatives (no back-tracking). Return error from parser that consumed more
// *>               Combine parsers, discarding success value from first
// <*               Combine parsers, discarding success value from second
// <^               Replace value of success
// <!       label   Replace value of failure
// many
// many1
// manyTill
// optional
// eof

public typealias ParseResult<T> = Result<T, ParseError>
public typealias Parser<T> = (ParseState) -> (ParseState, Result<T, ParseError>)

public struct ParseState: Equatable {
    var row: Int
    var col: Int
    public var input: Substring

    public init(_ s: Substring, row: Int = 0, col: Int = 0) {
        self.row = row
        self.col = col
        input = s
    }

    public func advance(_ n: Int = 1) -> ParseState {
        var newRow = row
        var newCol = col
        for c in input.prefix(n) {
            if c.isNewline {
                newRow += 1
                newCol = 0
            } else {
                newCol += 1
            }
        }
        return ParseState(input.dropFirst(n), row: newRow, col: newCol)
    }

}

public struct ParseError: Error, Equatable {
    public let state: ParseState
    public let label: String
}

//
// Combinators
//

// Apply function to parser's success result
infix operator <^>: MultiplicationPrecedence
public func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    (state: ParseState) -> (ParseState, ParseResult<T2>) in
        let (updatedState, r) = p(state)
        switch r {
        case .success(let val):
            return (updatedState, .success(f(val)))
        case .failure(let e):
            return (updatedState, .failure(e))
        }
    }
}

infix operator <*>: MultiplicationPrecedence
public func <*> <T1, T2> (_ p1: @escaping Parser<(T2) -> T1>, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (state: ParseState) -> (ParseState, ParseResult<T1>) in
        let (updatedState, r1) = p1(state)
        switch r1 {
        case .success(let f):
            let (finalState, r2) = p2(updatedState)
            switch r2 {
            case .success(let val):
                return (finalState, .success(f(val)))
            case .failure(let e):
                return (finalState, .failure(e))
            }
        case .failure(let e):
            return (updatedState, .failure(e))
        }
    }
}

// Alternative
infix operator <|>: AdditionPrecedence
public func <|> <T> (_ p1: @escaping Parser<T>, _ p2: @escaping Parser<T>) -> Parser<T> { {
    (state: ParseState) -> (ParseState, ParseResult<T>) in
        let (updatedState, r1) = p1(state)
        switch r1 {
        case .success:
            return (updatedState, r1)
        case .failure(let err1):
            let (finalState, r2) = p2(updatedState)
            switch r2 {
                case .success:
                    return (finalState, r2)
                case .failure(let err2):
                    // Return failure from the parser that consumed the most input
                    let consumed1 = state.input.count - updatedState.input.count
                    let consumed2 = updatedState.input.count - finalState.input.count
                    if consumed1 == consumed2 {
                        return (finalState,
                                .failure(ParseError(state: finalState, label: "Expected one of multiple alternatives")))
                    } else if consumed1 > consumed2 {
                        return (finalState, .failure(err1))
                    } else {
                        return (finalState, .failure(err2))
                    }
            }
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *>: MultiplicationPrecedence
public func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> { {
    (state: ParseState) -> (ParseState, ParseResult<T2>) in
        let (updatedState, r1) = p1(state)
        switch r1 {
        case .success:
            return p2(updatedState)
        case .failure(let e):
            return (updatedState, .failure(e))
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the second result
infix operator <*: MultiplicationPrecedence
public func <* <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (state: ParseState) -> (ParseState, ParseResult<T1>) in
        let (updatedState, r1) = p1(state)
        switch r1 {
        case .failure:
            return (updatedState, r1)
        case .success(let val):
            let (finalState, r2) = p2(updatedState)
            switch r2 {
            case .failure(let e):
                return (finalState, .failure(e))
            case .success:
                return (finalState, .success(val))
            }
        }
    }
}

// Replace success value
infix operator <^: MultiplicationPrecedence
public func <^ <T1, T2> (_ t: T1, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (state: ParseState) -> (ParseState, ParseResult<T1>) in
        let (updatedState, r2) = p2(state)
        switch r2 {
        case .success:
            return (updatedState, .success(t))
        case .failure(let e):
            return (updatedState, .failure(e))
        }
    }
}

// Change the error label of the given parser
infix operator <!: AdditionPrecedence
public func <! <T> (_ s: String, p: @escaping Parser<T>) -> Parser<T> { {
    (state: ParseState) -> (ParseState, ParseResult<T>) in
        let (updatedState, r) = p(state)
        switch r {
        case .success:
            return (updatedState, r)
        case .failure(let e):
            return (updatedState, .failure(ParseError(state: e.state, label: s)))
        }
    }
}

public func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (state: ParseState) -> (ParseState, ParseResult<[T]>) in
        var state = state
        var results: [T] = []
        while true {
            let (updatedState, r) = p(state)
            switch r {
            case .success(let val):
                results.append(val)
                state = updatedState
            case .failure:
                return (updatedState, .success(results))
            }
        }
    }
}

public func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (state: ParseState) -> (ParseState, ParseResult<[T]>) in
        var state = state
        var results: [T] = []
        while true {
            let (updatedState, r) = p(state)
            switch r {
            case .success(let val):
                results.append(val)
                state = updatedState
            case .failure(let err):
                return (updatedState, results.isEmpty ? .failure(err) : .success(results))
            }
        }
    }
}

public func manyTill<T, U>(_ p: @escaping Parser<T>, _ ending: @escaping Parser<U>) -> Parser<[T]> { {
    (state: ParseState) -> (ParseState, ParseResult<[T]>) in
        var state = state
        var results: [T] = []
        manyLoop: while true {
            let (updatedState, r) = ending(state)
            state = updatedState
            switch r {
            case .success:
                break manyLoop
            case .failure:
                break
            }

            let (secondState, r2) = p(state)
            state = secondState
            switch r2 {
            case .success(let val):
                results.append(val)
            case .failure:
                break manyLoop
            }
        }
        return (state, .success(results))
    }
}

public func optional<T>(_ p: @escaping Parser<T>) -> Parser<T?> { {
    (state: ParseState) -> (ParseState, ParseResult<T?>) in
        let (updatedState, r) = p(state)
        switch r {
            case .success(let val):
                return (updatedState, .success(.some(val)))
        case .failure:
                return (updatedState, .success(nil))
        }
    }
}

public func eof(_ state: ParseState) -> (ParseState, ParseResult<Void>) {
    if state.input.isEmpty {
        return (state, .success(()))
    }
    return (state, .failure(ParseError(state: state, label: "Expected EOF")))
}

//
// Lex helpers
//

// Make the given parser ignore any trailing space (as defined by the spaceConsumer parser, which
// should not fail on empty input)
public func lexeme<T, U>(_ p: @escaping Parser<T>, spaceConsumer: @escaping Parser<U>) -> Parser<T> {
    p <* spaceConsumer
}

//
// Character and string parsers
//

public func char(_ c: Character) -> Parser<Character> { {
    (state: ParseState) -> (ParseState, ParseResult<Character>) in
        if state.input.first == .some(c) {
            return (state.advance(), .success(state.input.first!))
        } else {
            return (state, .failure(ParseError(state: state, label: "Expected '\(c)'")))
        }
    }
}

public func anyChar(_ state: ParseState) -> (ParseState, ParseResult<Character>) {
    if let c = state.input.first {
        return (state.advance(), .success(c))
    }
    return (state, .failure(ParseError(state: state, label: "Expected any character")))
}

public func string(_ s: String) -> Parser<String> { {
    (state: ParseState) -> (ParseState, ParseResult<String>) in
        if state.input.hasPrefix(s) {
            return (state.advance(s.count), .success(s))
        }
    return (state, .failure(ParseError(state: state, label: "Expected '\(s)'")))
    }
}

public func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (state: ParseState) -> (ParseState, ParseResult<Character>) in
        if let firstChar = state.input.first {
            if f(firstChar) {
                return (state.advance(), .success(firstChar))
            }
        }
        return (state, .failure(ParseError(state: state, label: "Expectation not satisfied")))
    }
}

public let eol: Parser<Void> = () <^ satisfy {c in c.isNewline }
