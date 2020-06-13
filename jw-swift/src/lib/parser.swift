// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser - DIY parser combinator library

enum ParserResult<T: Equatable>: Equatable {
    case success(T, Substring) // Matched value and remaining input
    case failure(String, Substring) // Error message and input at failure

    func isSuccess() -> Bool {
        switch self {
        case .success:
            return true
        case .failure:
            return false
        }
    }
}

typealias Parser<T: Equatable> = (Substring) -> ParserResult<T>

func parse<T>(_ p: Parser<T>, _ s: String) {
    switch p(Substring(s)) {
    case.success(let match, let remainder):
        print("Success found '\(match)' with remainder '\(remainder)'")
    case .failure(let message, let input):
        print("Failed with error '\(message)'  parsing '\(input)'")
    }
}

//
// Combinators
//

// Create a parser that tries each of the given parsers in sequence and return the first successful match
func choice<T>(_ parsers: Parser<T>...) -> Parser<T> { {
    (input: Substring) -> ParserResult<T> in
        var r = ParserResult<T>.failure("No parsers found in choice", input)
        for p in parsers {
            r = p(input)
            if r.isSuccess() {
                return r
            }
        }
        return .failure("Expected one of multiple choices", input)
    }
}

func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
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

func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> { {
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
func between<T, U, V>(open: Parser<T>, close: Parser<U>, contents: Parser<V>) -> Parser<V> { {
    (input: Substring) -> ParserResult<V> in
        .failure("NYI", input)
    }
}

infix operator <^>
func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        switch p(input) {
        case .success(let val, let remaining):
            return .success(f(val), remaining)
        case .failure(let message, _):
            return .failure(message, input)
        }
    }
}

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *>
func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> { {
    (input: Substring) -> ParserResult<T2> in
        switch p1(input) {
        case .success(_, let remaining):
            return p2(remaining)
        case .failure(let message, _):
            return .failure(message, input)
        }
    }
}

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

func string(_ s: String) -> Parser<String> { {
    (input: Substring) -> ParserResult<String> in
        if input.hasPrefix(s) {
            return .success(s, input.dropFirst(s.count))
        }
        return .failure("Expected '\(s)'", input)
    }
}

// spaces
var spaces: Parser<String> = { cs in String(cs) } <^> many(char(" "))

// satisfy
func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> { {
    (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if f(firstChar) {
                return .success(firstChar, input.dropFirst())
            }
        }
        return .failure("Expectation not satisfied'", input)
    }
}
