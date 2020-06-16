// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser - DIY parser combinator library

public enum ParserResult<T: Equatable>: Equatable {
    case success(T, Substring) // Matched value and remaining input
    case failure(String, Substring) // Error message and input at failure
}

public typealias Parser<T: Equatable> = (Substring) -> ParserResult<T>

//
// Combinators
//

// Create a parser that tries each of the given parsers in sequence and return the first successful match
public func choice<T>(_ parsers: [Parser<T>]) -> Parser<T> { {
    (input: Substring) -> ParserResult<T> in
        if parsers.isEmpty {
            return .failure("No parsers found in choice", input)
        }
        for p in parsers {
            let r = p(input)
            switch r {
            case .success:
                return r
            case .failure:
                break
            }
        }
        return .failure("Expected one of multiple choices", input)
    }
}

public func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (input: Substring) -> ParserResult<[T]> in
        var results: [T] = []
        var remaining = input
        while true {
            switch p(remaining) {
            case .success(let successVal, let successRemaining):
                results.append(successVal)
                remaining = successRemaining
            case .failure:
                return .success(results, remaining)
            }
        }
    }
}

public func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
    (input: Substring) -> ParserResult<[T]> in
        var results: [T] = []
        var remaining = input
        while true {
            switch p(remaining) {
            case .success(let successVal, let successRemaining):
                results.append(successVal)
                remaining = successRemaining
            case .failure(let failMessage, _):
                return results.isEmpty ? .failure(failMessage, input) : .success(results, remaining)
            }
        }
    }
}

// between
public func between<T, U, V>(
    _ contents: @escaping Parser<T>,
    open: @escaping Parser<U>,
    close: @escaping Parser<V>) -> Parser<T> {
    open *> (contents <* close)
}

infix operator <^>
public func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        switch p(input) {
        case .success(let val, let remaining):
            return .success(f(val), remaining)
        case .failure(let message, let remaining):
            return .failure(message, remaining)
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *>
public func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        switch p1(input) {
        case .success(_, let remaining):
            return p2(remaining)
        case .failure(let message, let remaining):
            return .failure(message, remaining)
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the second result
infix operator <*
public func <* <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T1> { {
    (input: Substring) -> ParserResult<T1> in
        switch p1(input) {
        case .success(let firstVal, let firstRemaining):
            switch p2(firstRemaining) {
            case .success(_, let secondRemaining):
                return .success(firstVal, secondRemaining)
            case .failure(let message, let secondRemaining):
                return .failure(message, secondRemaining)
            }
        case .failure(let message, let remaining):
            return .failure(message, remaining)
        }
    }
}

// Change the failure message of the given parser
infix operator <!>
public func <!> <T> (_ s: String, p: @escaping Parser<T>) -> Parser<T> { {
    (input: Substring) -> ParserResult<T> in
        switch p(input) {
        case .success(let val, let rest):
            return .success(val, rest)
        case .failure(_, let rest):
            return .failure(s, rest)
        }
    }
}

//
// Character and string parsers
//

public func char(_ c: Character) -> Parser<Character> { {
    (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if firstChar == c {
                return .success(firstChar, input.dropFirst())
            }
        }
        return .failure("Expected '\(c)'", input)
    }
}

public func string(_ s: String) -> Parser<String> { {
    (input: Substring) -> ParserResult<String> in
        if input.hasPrefix(s) {
            return .success(s, input.dropFirst(s.count))
        }
        return .failure("Expected '\(s)'", input)
    }
}

// In Mal comma counts as a space
public func isMalSpace(_ c: Character) -> Bool {
    c.isWhitespace || c == ","
}

// space
public var space: Parser<Character> = "Expected whitespace" <!> satisfy(isMalSpace)

// spaces
public var spaces: Parser<String> = { cs in String(cs) } <^> many(space)

// spaces1
public var spaces1: Parser<String> = { cs in String(cs) } <^> many1(space)

// satisfy
public func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if f(firstChar) {
                return .success(firstChar, input.dropFirst())
            }
        }
        return .failure("Expectation not satisfied", input)
    }
}
