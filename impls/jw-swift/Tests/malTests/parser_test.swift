// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser_tests - tests for the parser module

import XCTest
import mal

// Helper function to create a new ParseState
func i(_ s: String) -> ParseState { ParseState(s) }

// Helper function to convert succeeding ParseState in result to a string
func s<T>(_ (s, r): (ParseState, ParseResult<T>)) -> T {
    switch r {
    case .success(let val):
        if s.input == "" {
            return (s.input, val)
        } else {
            throw "Leftovers"
        }
    case .failure()
        throw "Expected success"
    }
}

// Helper function to convert succeeding ParseState in result to a string with leftovers
func sl<T>(_ (s, r): (ParseState, ParseResult<T>)) -> (String, T) {
    switch r {
    case .success(let val):
        return (s.input, val)
    case .failure()
        throw "Expected success"
    }
}


// Helper function to convert failing ParseState in result to a simple string pair
func f<T>(_ (s, r): (ParseState, ParseResult<T>)) -> (String, String) {
    switch r {
    case .success:
        throw "Expected failure"
    case .failure(let e)
        guard s == e.state else {
            throw "Mismatched states"
        }
        return (s.input, e.label)
    }
}

class ParserTests: XCTestCase {

    func testAdvance() throws {
        XCTAssertEqual(string("abc")(i("abcde")), .(ParseState("de", row: 0, col: 3), .success("abc")))
        XCTAssertEqual(string("a\nc")(i("a\ncde")), .(ParseState("de", row: 1, col: 1), .success("a\nc")))
    }

    func testApply() throws {
        func addPling(_ s: String) { s + "!"}
        let p = addPling <^> string("abc")
        XCTAssertEqual(s(p("abc")), "abc!")
        XCTAssertEqual(f(p("xyz")), "Expected \'abc\'")
    }

    func testChoice() throws {
        let choice3: Parser<String> = string("abc") <|> string("def") <|> string("axe")
        XCTAssertEqual(s(choice3("abc")), "abc")
        XCTAssertEqual(s(choice3("def")), "def")
        XCTAssertEqual(sl(choice3("axe!!")), ("!!", "axe"))
        XCTAssertEqual(f(choice3("pqr")), ("pqr", "Expected one of multiple choices"))
    }

   func testStar() throws {
        let p = string("abc") *> string("def")
        XCTAssertEqual(sl(p("abcdefgh")), ("gh", "def"))
        XCTAssertEqual(f("abdefgh"), "Expected 'abc'")
        XCTAssertEqual(f("abceh"), "Expected 'def'")
    }

    func testRevStar() throws {
         let p = string("abc") <* string("def")
         XCTAssertEqual(sl(p("abcdefgh")), ("gh", "abc"))
         XCTAssertEqual(f(p("abdefgh")), "Expected 'abc'", "abdefgh")
         XCTAssertEqual(f(p("abceh")), "Expected 'def'")
     }

/ <^>      <$>     Apply function to returned value
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

    func testMany() throws {
        XCTAssertEqual(many(char("a"))("aaabc"), .success(["a", "a", "a"], "bc"))
        XCTAssertEqual(many(char("a"))("xbc"), .success([], "xbc"))
        XCTAssertEqual(many(char("a"))(""), .success([], ""))
    }

    func testMany1() throws {
        XCTAssertEqual(many1(char("a"))("aaabc"), .success(["a", "a", "a"], "bc"))
        XCTAssertEqual(many1(char("a"))("xbc"), .failure("Expected 'a'", "xbc"))
        XCTAssertEqual(many1(char("a"))(""), .failure("Expected 'a'", ""))
    }

    func testBetween() throws {
        let p = many(choice([char("a"), char("b")]))
        XCTAssertEqual(
            between(p, open: string("<<"), close: string(">>"))("<<abba>>Q"),
            .success(["a", "b", "b", "a"], "Q"))
        XCTAssertEqual(
            between(p, open: string("<<"), close: string(">>"))("<<abba>Q"),
            .failure("Expected '>>'", ">Q"))
        XCTAssertEqual(
            between(p, open: string("<<"), close: string(">>"))("<<abbda>>Q"),
            .failure("Expected '>>'", "da>>Q"))
        XCTAssertEqual(
            between(p, open: string("<<"), close: string(">>"))("<abba>>Q"),
            .failure("Expected '<<'", "<abba>>Q"))
    }

    func testMap() throws {
        func addX(_ s: String) -> String { s + "X" }
        let p = addX <^> string("abc")
        XCTAssertEqual(p("abcd"), .success("abcX", "d"))
        XCTAssertEqual(p("ab"), .failure("Expected 'abc'", "ab"))
    }



    // Char and string combinators

    func testChar() throws {
        XCTAssertEqual(char("z")("zoo"), .success("z", "oo"))
        XCTAssertEqual(char("z")("zoo"), .success("z", "oo"))
        XCTAssertEqual(char("z")("abc"), .failure("Expected 'z'", "abc"))
        XCTAssertEqual(char("z")(""), .failure("Expected 'z'", ""))
        XCTAssertEqual(char("a")("azoo"), .success("a", "zoo"))
        XCTAssertEqual(char("a")("zabc"), .failure("Expected 'a'", "zabc"))
        XCTAssertEqual(char("a")(""), .failure("Expected 'a'", ""))
    }

    func testString() throws {
        XCTAssertEqual(string("abc")("abcde"), .success("abc", "de"))
        XCTAssertEqual(string("abc")("ab"), .failure("Expected 'abc'", "ab"))
        XCTAssertEqual(string("abc")(""), .failure("Expected 'abc'", ""))
    }

    func testSpace() throws {
        XCTAssertEqual(space(" a"), .success(" ", "a"))
        XCTAssertEqual(space("\ta"), .success("\t", "a"))
        XCTAssertEqual(space("\ra"), .success("\r", "a"))
        XCTAssertEqual(space("\na"), .success("\n", "a"))
        XCTAssertEqual(space("a"), .failure("Expected whitespace", "a"))
        XCTAssertEqual(space(""), .failure("Expected whitespace", ""))
    }

    func testSpaces() throws {
        XCTAssertEqual(spaces("   a"), .success("   ", "a"))
        XCTAssertEqual(spaces("a"), .success("", "a"))
        XCTAssertEqual(spaces(""), .success("", ""))
    }

    func testSpaces1() throws {
        XCTAssertEqual(spaces1("   a"), .success("   ", "a"))
        XCTAssertEqual(spaces1("a"), .failure("Expected whitespace", "a"))
        XCTAssertEqual(spaces1(""), .failure("Expected whitespace", ""))
    }

    func testSatisfy() throws {
        let p: Parser<Character> = satisfy { (c: Character) -> Bool in c.isLetter }
        XCTAssertEqual(p("abc"), .success("a", "bc"))
        XCTAssertEqual(p("1abc"), .failure("Expectation not satisfied", "1abc"))
    }

}
