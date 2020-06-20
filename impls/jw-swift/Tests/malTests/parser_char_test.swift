// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// parser_tests - tests for the parser module

import mal
import XCTest

public class ParserCharTests: XCTestCase {
    public public func testChar() throws {
        let p = char("z")
        let (str1, res1) = s(p(i("zq")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success("z"))
        let (str2, res2) = f(p(i("q")))
        XCTAssertEqual(str2, "q")
        XCTAssertEqual(res2, "Expected 'z'")
        let (str3, res3) = f(p(i("")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected 'z'")
    }

    public func testAnyChar() throws {
        let (str1, res1) = s(anyChar(i("zq")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success("z"))
        let (str2, res2) = f(anyChar(i("")))
        XCTAssertEqual(str2, "")
        XCTAssertEqual(res2, "Expected any character")
    }

    public func testString() throws {
        let p = string("abc")
        let (str1, res1) = s(p(i("abccq")))
        XCTAssertEqual(str1, "cq")
        XCTAssertEqual(res1, .success("abc"))
        let (str2, res2) = f(p(i("abq")))
        XCTAssertEqual(str2, "abq")
        XCTAssertEqual(res2, "Expected 'abc'")
        let (str3, res3) = f(p(i("")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected 'abc'")
    }

    public func testSatisfy() throws {
        let p = satisfy { c in c == "a" }
        let (str1, res1) = s(p(i("aq")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success("a"))
        let (str2, res2) = f(p(i("q")))
        XCTAssertEqual(str2, "q")
        XCTAssertEqual(res2, "Expectation not satisfied")
        let (str3, res3) = f(p(i("")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expectation not satisfied")
    }

    public func testEol() throws {
        let (str1, res1) = s(eol(i("\nQ")))
        XCTAssertEqual(str1, "Q")
        switch res1 { // workaround as void is not Equatable so cant use XCTAssertEqual
        case .success:
            break
        case .failure:
            XCTFail("Expected success")
        }
        let (str2, msg2) = f(eol(i("Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(msg2, "Expected end of line")
        let (str3, msg3) = f(eol(i("")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(msg3, "Expected end of line")
    }

    public func testLexeme() throws {
        let p = lexeme(string("abc"), spaceConsumer: many(char(" ") <|> char(",")))
        let (str1, res1) = s(p(i("abc  ,,Q")))
        XCTAssertEqual(str1, "Q")
        XCTAssertEqual(res1, .success("abc"))
        let (str2, res2) = f(p(i("q")))
        XCTAssertEqual(str2, "q")
        XCTAssertEqual(res2, "Expected 'abc'")
    }
}
