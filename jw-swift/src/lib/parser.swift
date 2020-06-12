// DIY parser combinators

public enum ParserResult<T : Equatable> : Equatable {
    case success(T, Substring) // Matched value and remaining input
    case failure(String, Substring) // Error message and input at failure

    func isSuccess() -> Bool {
        switch self {
            case .success(_, _):
                return true
            case .failure(_, _):
                return false
        }
    }
}

public typealias Parser<T : Equatable> = (Substring) -> ParserResult<T>

func parse<T>(_ p: Parser<T>, _ s: String) -> () {
    switch p(Substring.init(s)) {
        case ParserResult.success(let match, let remainder):
            print("Success found '\(match)' with remainder '\(remainder)'")
        case ParserResult.failure(let message, let input):
            print("Failed with error '\(message)'  parsing '\(input)'")
    }
}

// Create a parser that tries each of the given parsers in sequence and return the first successful match
public func choice<T>(_ parsers: Parser<T>...) -> Parser<T> {
    return {
        (input: Substring) -> ParserResult<T> in
            var r = ParserResult<T>.failure("No parsers found in choice", input)
            for p in parsers {
                r = p(input)
                if r.isSuccess() {
                    return r
                }
            }
            return ParserResult<T>.failure("Expected one of multiple choices", input)
    }
}

// many
public func many<T>(_ p: @escaping Parser<T>) -> Parser<[T]> {
    return {
        (input: Substring) -> ParserResult<[T]> in
        var results : [T] = []
        var remaining = input
        while true {
            switch p(remaining) {
            case .success(let successVal, let successRemaining):
                results.append(successVal)
                remaining = successRemaining
            case .failure(_, _):
                return .success(results, remaining)
            }
        }
    }
}

// many1
public func many1<T>(_ p: @escaping Parser<T>) -> Parser<[T]> {
    return {
        (input: Substring) -> ParserResult<[T]> in
        var results : [T] = []
        var remaining = input
        while true {
            switch p(remaining) {
                case .success(let successVal, let successRemaining):
                    results.append(successVal)
                    remaining = successRemaining
                case .failure(let failMessage, _):
                    return results.count == 0 ? .failure(failMessage, input) : .success(results, remaining)
            }
        }
    }
}

// spaces
public var spaces : Parser<String> = {cs in String(cs)} <^> many(char(" "))

// between
public func between<T, U, V>(open: Parser<T>, close: Parser<U>, contents: Parser<V>) -> Parser<V> {
    return {
        (input: Substring) -> ParserResult<V> in
        return ParserResult<V>.failure("NYI", input)
    }
}

// satisfy
public func satisfy(_ f: @escaping (Character) -> Bool) -> Parser<Character> {
    return { (input: Substring) -> ParserResult<Character> in
        if let firstChar = input.first {
            if f(firstChar) {
                return ParserResult.success(firstChar, input.dropFirst())
            }
        }
        return ParserResult.failure("Expectation not satisfied'", input)
    }
}
    
// pmap
infix operator <^>
public func <^> <T1, T2> (_ f: @escaping (T1) -> T2, _ p: @escaping Parser<T1>) -> Parser<T2> {
    return {
            (input: Substring) -> ParserResult<T2> in
                switch p(input) {
                    case ParserResult<T1>.success(let val, let remaining):
                        return ParserResult<T2>.success(f(val), remaining)
                    case ParserResult<T1>.failure(let message, _):
                        return ParserResult<T2>.failure(message, input)
                }
    }
}


// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *>
public func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> {
    return {
        (input: Substring) -> ParserResult<T2> in
            switch p1(input) {
                case ParserResult<T1>.success(_, let remaining):
                    return p2(remaining)
                case ParserResult<T1>.failure(let message, _):
                    return ParserResult<T2>.failure(message, input)
            }
    }
}

public func char(_ c: Character) -> Parser<Character> {
    return {
        (input: Substring) -> ParserResult<Character> in
            if let firstChar = input.first {
                if firstChar == c {
                    return ParserResult.success(firstChar, input.dropFirst())
                }
            }
            return ParserResult.failure("Expected '\(c)'", input)
    }
}

public func string(_ s: String) -> Parser<String> {
    return {
        (input: Substring) -> ParserResult<String> in
            if input.hasPrefix(s) {
                return ParserResult.success(s, input.dropFirst(s.count))
            }
            return ParserResult.failure("Expected '\(s)'", input)
    }
}

