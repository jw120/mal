// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader_tests - tests for the reader module

import mal
import XCTest

public class ReaderTests: XCTestCase {
    public func testExpr() throws {
        let (str1, res1) = s(expr(i("123q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.int(123)))
        let (str2, res2) = s(expr(i("(2)qq")))
        XCTAssertEqual(str2, "qq")
        XCTAssertEqual(res2, .success(.list([.int(2)])))
        let (str3, res3) = s(expr(i("'23")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, .success(.list([.sym("quote"), .int(23)])))
        let (str0, msg0) = f(expr(i("")))
        XCTAssertEqual(str0, "")
        XCTAssertEqual(msg0, "Expected one of multiple alternatives")
    }

    public func testInt() throws {
        let (str1, res1) = s(int(i("123  ,q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.int(123)))
        let (str2, res2) = f(int(i("q")))
        XCTAssertEqual(str2, "q")
        XCTAssertEqual(res2, "Expected a digit")
        let (str3, res3) = f(int(i("")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected a digit")
        let (str4, res4) = s(int(i("-12Q")))
        XCTAssertEqual(str4, "Q")
        XCTAssertEqual(res4, .success(.int(-12)))
    }

    public func testList() throws {
        let (str1, res1) = s(list(i("(2 3)q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.list([.int(2), .int(3)])))
        let (str2, res2) = s(list(i("()Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, .success(.list([])))
        let (str3, res3) = f(list(i("(2")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected ')'")
        let (str4, res4) = f(list(i("")))
        XCTAssertEqual(str4, "")
        XCTAssertEqual(res4, "Expected '('")
    }

    public func testVector() throws {
        let (str1, res1) = s(vector(i("[2 3,4]q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.vec([.int(2), .int(3), .int(4)])))
        let (str2, res2) = s(vector(i("[]Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, .success(.vec([])))
        let (str3, res3) = f(vector(i("[2")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected ']'")
        let (str4, res4) = f(vector(i("")))
        XCTAssertEqual(str4, "")
        XCTAssertEqual(res4, "Expected '['")
    }

    public func testHashmap() throws {
        let (str1, res1) = s(hashmap(i("{2 3}q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.hashmap([.int(2), .int(3)])))
        let (str2, res2) = s(hashmap(i("{}Q")))
        XCTAssertEqual(str2, "Q")
        XCTAssertEqual(res2, .success(.hashmap([])))
        let (str3, res3) = f(hashmap(i("{2 3")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, "Expected '}'")
        let (str4, res4) = f(hashmap(i("")))
        XCTAssertEqual(str4, "")
        XCTAssertEqual(res4, "Expected '{'")
    }

    public func testSymbol() throws {
        let (str1, res1) = s(sym(i("abc q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.sym("abc")))
        let (str2, _) = f(sym(i("()")))
        XCTAssertEqual(str2, "()")
        let (str3, res3) = s(sym(i("true")))
        XCTAssertEqual(str3, "")
        XCTAssertEqual(res3, .success(.bool(true)))
        let (str4, res4) = s(sym(i("false")))
        XCTAssertEqual(str4, "")
        XCTAssertEqual(res4, .success(.bool(false)))
        let (str5, res5) = s(sym(i("nil")))
        XCTAssertEqual(str5, "")
        XCTAssertEqual(res5, .success(.null))
    }

    public func testString() throws {
        let (str1, res1) = s(str(i("\"abc\" q")))
        XCTAssertEqual(str1, "q")
        XCTAssertEqual(res1, .success(.str("abc")))
        let (str2, _) = f(str(i("()")))
        XCTAssertEqual(str2, "()")
        let (str3, _) = f(str(i("")))
        XCTAssertEqual(str3, "")
        let (str4, _) = f(str(i("\"")))
        XCTAssertEqual(str4, "")
    }
}
