// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser_tests - tests for the parser module

import XCTest

class ParserTests: XCTestCase {

    // Combinators

    func testChoice() throws {
        let s3: Parser<String> = choice([string("abc"), string("def"), string("axe")])
        XCTAssertEqual(s3("abcde"), .success("abc", "de"))
        XCTAssertEqual(s3("defgh"), .success("def", "gh"))
        XCTAssertEqual(s3("axe!!"), .success("axe", "!!"))
        XCTAssertEqual(s3("pqr"), .failure("Expected one of multiple choices", "pqr"))
    }

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

    func testStar() throws {
        let p = string("abc") *> string("def")
        XCTAssertEqual(p("abcdefgh"), .success("def", "gh"))
        XCTAssertEqual(p("abdefgh"), .failure("Expected 'abc'", "abdefgh"))
        XCTAssertEqual(p("abceh"), .failure("Expected 'def'", "eh"))
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
