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

    func testCombine() throws {
        func g(_ s : String) -> (String) -> String { {
                (t: String) -> String in s + t + s
        } }
        let p = g <^> string("abc") <*> string("def")
        let (str1, res1) = s(p(i("abcdefg")))
        XCTAssertEqual(str1, "g")
        XCTAssertEqual(res1, .success("abcdefabc"))
        let (str2, err2) = f(p(i("abXdef")))
        XCTAssertEqual(str2, "abXdef")
        XCTAssertEqual(err2, "Expected 'abc'")
        let (str3, err3) = f(p(i("abcdeX")))
        XCTAssertEqual(str3, "deX")
        XCTAssertEqual(err3, "Expected 'def'")
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

    func testChoiceNoBackTracking() throws {
        // Failure of first alternative consumes input and makes second fail
        // and gives error message from second parser which consumed more input
        let p1 = (char("a") *> char("b")) <|> char("a")
        let (str1, res1) = f(p1(i("ac")))
        XCTAssertEqual(str1, "c")
        XCTAssertEqual(res1, "Expected 'b'") //

        // Failure of both alternatives gives second parser's error message if it consumed more
        let p2 = (char("a") *> char("b")) <|> (char("x") *> char("y") *> char("z"))
        let (str2, res2) = f(p2(i("axyQ")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, "Expected 'z'")
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

    func testSuccess() throws {
        let p = 23 <^ string("zoo")
        let (str1, res1) = s(p(i("zoom")))
        XCTAssertEqual(str1, "m")
        XCTAssertEqual(res1, .success(23))
        let (str2, err2) = f(p(i("park")))
        XCTAssertEqual(str2, "park")
        XCTAssertEqual(err2, "Expected 'zoo'")
    }

    func testFailure() throws {
        let p = "special" <! string("zoo")
        let (str1, res1) = s(p(i("zoom")))
        XCTAssertEqual(str1, "m")
        XCTAssertEqual(res1, .success("zoo"))
        let (str2, err2) = f(p(i("park")))
        XCTAssertEqual(str2, "park")
        XCTAssertEqual(err2, "special")
    }

  func testMany() throws {
        let p = many(char("a"))
        let (str1, res1) = s(p(i("aaab")))
        XCTAssertEqual(str1, "b")
        XCTAssertEqual(res1, .success(["a", "a", "a"]))
        let (str2, res2) = s(p(i("b")))
        XCTAssertEqual(str2, "b")
        XCTAssertEqual(res2, .success([]))
    }

    func testMany1() throws {
         let p = many1(char("a"))
         let (str1, res1) = s(p(i("aaab")))
         XCTAssertEqual(str1, "b")
         XCTAssertEqual(res1, .success(["a", "a", "a"]))
         let (str2, res2) = f(p(i("b")))
         XCTAssertEqual(str2, "b")
         XCTAssertEqual(res2, "Expected 'a'")
     }

    func testManyTill() throws {
        let p = manyTill(anyChar, char("]"))
        let (str1, res1) = s(p(i("abc]Q")))
        XCTAssertEqual(str1, "Q")
        XCTAssertEqual(res1, .success(["a", "b", "c"]))
        let (str2, res2) = s(p(i("]Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, .success([]))
        let (str3, res3) = s(p(i("abc")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, .success(["a", "b", "c"]))
        let (str4, res4) = s(p(i("")))
        XCTAssertEqual(str4, "")
        XCTAssertEqual(res4, .success([]))

    }

    func testOptional() throws {
        let p = optional(char("a"))
        let (str1, res1) = s(p(i("aQ")))
        XCTAssertEqual(str1, "Q")
        XCTAssertEqual(res1, .success(.some("a")))
        let (str2, res2) = s(p(i("Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, .success(nil))

    }

    func testEof() throws {
        let (str1, res1) = s(eof(i("")))
        XCTAssertEqual(str1, "")
        switch res1 { // workaround as void is not Equatable so cant use XCTAssertEqual
        case .success:
            break
        case .failure:
            XCTFail("Expected success")
        }
        let (str2, msg2) = f(eof(i("Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(msg2, "Expected EOF")
    }

    func testChar() throws {
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

    func testAnyChar() throws {
        let (str1, res1) = s(anyChar(i("zq")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success("z"))
        let (str2, res2) = f(anyChar(i("")))
        XCTAssertEqual(str2, "")
        XCTAssertEqual(res2, "Expected any character")
    }

    func testString() throws {
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

    func testSatisfy() throws {
        let p = satisfy { c in c == "a"}
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

    func testEol() throws {
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

    func testLexeme() throws {
        let p = lexeme(string("abc"), spaceConsumer: many(char(" ") <|> char(",")))
        let (str1, res1) = s(p(i("abc  ,,Q")))
        XCTAssertEqual(str1, "Q")
        XCTAssertEqual(res1, .success("abc"))
        let (str2, res2) = f(p(i("q")))
        XCTAssertEqual(str2, "q")
        XCTAssertEqual(res2, "Expected 'abc'")
    }

}
