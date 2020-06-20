// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser_tests - tests for the parser module

import XCTest
import mal

struct HelperError: Error { }

// Helper function to create a new ParseState
func i(_ s: String) -> ParseState { ParseState(Substring(s)) }

// Helper function to convert succeeding output in to a tuple
func s<T>(_ output: (ParseState, ParseResult<T>)) -> (String, ParseResult<T>) {
    let (state, result) = output
    return (String(state.input), result)
}

// Helper function to convert failing ParseState to a string tuple
func f<T>(_ output: (ParseState, ParseResult<T>)) -> (String, String) {
    let (state, result) = output
    switch result {
    case .success(let val):
        return (String(state.input), "Unexpected success '\(val)'")
    case .failure(let e):
        return (String(state.input), e.label)
    }
}

class ParserTests: XCTestCase {

    func testAdvance() throws {
        let (state1, result1) = string("abc")(i("abcde"))
        XCTAssertEqual(state1, ParseState("de", row: 0, col: 3))
        XCTAssertEqual(result1, .success("abc"))
        let (state2, result2) = string("a\nc")(i("a\ncde"))
        XCTAssertEqual(state2, ParseState("de", row: 1, col: 1))
        XCTAssertEqual(result2, .success("a\nc"))

    }

    func testApply() throws {
        func addPling(_ s: String) -> String { s + "!"}
        let p = addPling <^> string("abc")
        let (str1, res1) = s(p(i("abcQQ")))
        XCTAssertEqual(str1, "QQ")
        XCTAssertEqual(res1, .success("abc!"))
        let (str2, err2) = f(p(i("xyzQQ")))
        XCTAssertEqual(str2, "xyzQQ")
        XCTAssertEqual(err2, "Expected \'abc\'")
    }

    func testChoice() throws {
        let p: Parser<String> = string("abc") <|> string("def") <|> string("axe")
        let (str1, res1) = s(p(i("abcZZ")))
        XCTAssertEqual(str1, "ZZ")
        XCTAssertEqual(res1, .success("abc"))
        let (str2, res2) = s(p(i("defZZZ")))
        XCTAssertEqual(str2, "ZZZ")
        XCTAssertEqual(res2, .success("def"))
        let (str3, res3) = s(p(i("axe!!")))
        XCTAssertEqual(str3, "!!")
        XCTAssertEqual(res3, .success("axe"))
        let (str4, err4) = f(p(i("ABC")))
        XCTAssertEqual(str4, "ABC")
        XCTAssertEqual(err4, "Expected one of multiple alternatives")
    }

   func testStar() throws {
        let p = string("abc") *> string("def")
        let (str1, res1) = s(p(i("abcdefg")))
        XCTAssertEqual(str1, "g")
        XCTAssertEqual(res1, .success("def"))
        let (str2, err2) = f(p(i("abXdef")))
        XCTAssertEqual(str2, "abXdef")
        XCTAssertEqual(err2, "Expected 'abc'")
        let (str3, err3) = f(p(i("abcdeX")))
        XCTAssertEqual(str3, "deX")
        XCTAssertEqual(err3, "Expected 'def'")
    }

    func testRevStar() throws {
         let p = string("abc") <* string("def")
         let (str1, res1) = s(p(i("abcdefg")))
         XCTAssertEqual(str1, "g")
         XCTAssertEqual(res1, .success("abc"))
         let (str2, err2) = f(p(i("abXdef")))
         XCTAssertEqual(str2, "abXdef")
         XCTAssertEqual(err2, "Expected 'abc'")
         let (str3, err3) = f(p(i("abcdeX")))
         XCTAssertEqual(str3, "deX")
         XCTAssertEqual(err3, "Expected 'def'")
     }

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

/*

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

 */

}
