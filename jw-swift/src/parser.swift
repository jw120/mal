// DIY parser combinators

enum ParserResult<T : Equatable> : Equatable {
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

typealias Parser<T : Equatable> = (Substring) -> ParserResult<T>

func parse<T>(_ p: Parser<T>, _ s: String) -> () {
    switch p(Substring.init(s)) {
        case ParserResult.success(let match, let remainder):
            print("Success found '\(match)' with remainder '\(remainder)'")
        case ParserResult.failure(let message, let input):
            print("Failed with error '\(message)'  parsing '\(input)'")
    }
}

// Create a parser that tries each of the given parsers in sequence and return the first successful match
func choice<T>(_ parsers: Parser<T>...) -> Parser<T> {
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

// many1

// between

// <$>

// Combine two parsers into one that matches them in sequence, discarding the first result
infix operator *>
func *> <T1, T2> (_ p1: @escaping Parser<T1>, _ p2: @escaping Parser<T2>) -> Parser<T2> {
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

func char(_ c: Character) -> Parser<Character> {
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

func string(_ s: String) -> Parser<String> {
    return {
        (input: Substring) -> ParserResult<String> in
            if input.hasPrefix(s) {
                return ParserResult.success(s, input.dropFirst(s.count))
            }
            return ParserResult.failure("Expected '\(s)'", input)
    }
}


//
// Unit tests
//

func test<T>(_ description: String, parser: Parser<T>, input: String, expected: ParserResult<T>) {
    let result = parser(Substring.init(input))
    if result == expected {
        print("OK: \(description)")
    } else {
        print("Failed: \(description), received \(result) expected \(expected)")
    }
}

// char
test("char z present", parser: char("z"), input: "zoo", expected: ParserResult.success("z", "oo"))
test("char z missing", parser: char("z"), input: "abc", expected: ParserResult.failure("Expected 'z'", "abc"))
test("char z empty", parser: char("z"), input: "", expected: ParserResult.failure("Expected 'z'", ""))
test("char a present", parser: char("a"), input: "azoo", expected: ParserResult.success("a", "zoo"))
test("char a missing", parser: char("a"), input: "zabc", expected: ParserResult.failure("Expected 'a'", "zabc"))
test("char a empty", parser: char("a"), input: "", expected: ParserResult.failure("Expected 'a'", ""))

// string
test("string abc present", parser: string("abc"), input: "abcde", expected: ParserResult.success("abc", "de"))
test("string abc missing", parser: string("abc"), input: "ab", expected: ParserResult.failure("Expected 'abc'", "ab"))
test("string abc empty", parser: string("abc"), input: "", expected: ParserResult.failure("Expected 'abc'", ""))

// choice
let s3 : Parser<String> = choice(string("abc"), string("def"), string("axe"))
test("choice s3 1st present", parser: s3, input: "abcde", expected: ParserResult.success("abc", "de"))
test("choice s3 2nd present", parser: s3, input: "defgh", expected: ParserResult.success("def", "gh"))
test("choice s3 3rd present", parser: s3, input: "axe!!", expected: ParserResult.success("axe", "!!"))
test("choice s3 missing", parser: s3, input: "pqr", expected: ParserResult.failure("Expected one of multiple choices", "pqr"))

// *>
test("*> matching", parser: string("abc") *> string("def"), input: "abcdefgh", expected: ParserResult.success("def", "gh"))
test("*> failing 1st", parser: string("abc") *> string("def"), input: "abdefgh", expected: ParserResult.failure("Expected 'abc'", "abdefgh"))
test("*> failing 2nd", parser: string("abc") *> string("def"), input: "abceh", expected: ParserResult.failure("Expected 'def'", "eh"))
