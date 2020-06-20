// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader_tests - tests for the reader module

import XCTest
import mal

class ReaderTests: XCTestCase {

/*

    func testReadStr() throws {
        XCTAssertEqual(read_str("22"), ReadResult.value(.int(22)))
        XCTAssert(read_str("(").isErr())
        XCTAssertFalse(read_str("2").isErr())
        XCTAssertEqual(read_str("  "), ReadResult.nothing)
        XCTAssertEqual(read_str(""), ReadResult.nothing)
    }

    func testExpr() throws {
        XCTAssertEqual(expr("23QQ"), .success(.int(23), "QQ"))
        XCTAssertEqual(expr("(2 3)Q"), .success(.list([.int(2), .int(3)]), "Q"))
        XCTAssertEqual(expr("(+ 2 3)"), .success(.list([.sym("+"), .int(2), .int(3)]), ""))
        XCTAssertEqual(expr("[0,1]"), .success(.vec([.int(0), .int(1)]), ""))
    }

    func testInt() throws {
        XCTAssertEqual(int("42"), .success(.int(42), ""))
        XCTAssertEqual(int("-42"), .success(.int(-42), ""))
        XCTAssertEqual(int("Q2"), .failure("Expected an integer", "Q2"))
        XCTAssertEqual(int(""), .failure("Expected an integer", ""))
    }

    func testList() throws {
//        XCTAssertEqual((spaces *> int)("  23"), .success(.int(23), ""))
//        XCTAssertEqual(expr(Substring("  23")), .success(.int(23), ""))
//        XCTAssertEqual(many(spaces *> int)("  23"), .success([.int(23)], ""))
//        XCTAssertEqual(many(spaces *> int)("  23 -45"), .success([.int(23), .int(-45)], ""))
//        XCTAssertEqual(many(expr)("  23 45"), .success([.int(23), .int(45)], ""))

        XCTAssertEqual(list("(2 3)Q"), .success(.list([.int(2), .int(3)]), "Q"))
        XCTAssertEqual(list("( 2   3 4 )Q"), .success(.list([.int(2), .int(3), .int(4)]), "Q"))
        XCTAssertEqual(list("()Z"), .success(.list([]), "Z"))
        XCTAssertEqual(list("(2 (3 4) 5)Z"), .success(.list([.int(2), .list([.int(3), .int(4)]), .int(5)]), "Z"))
    }

    func testVec() throws {
        XCTAssertEqual(vector("[2 3]Q"), .success(.vec([.int(2), .int(3)]), "Q"))
        XCTAssertEqual(vector("[ 2   3,4 ]Q"), .success(.vec([.int(2), .int(3), .int(4)]), "Q"))
        XCTAssertEqual(vector("[]Z"), .success(.vec([]), "Z"))
        XCTAssertEqual(vector("[2 (3 4) 5]Z"), .success(.vec([.int(2), .list([.int(3), .int(4)]), .int(5)]), "Z"))
    }

    func testHash() throws {
        XCTAssertEqual(hashmap("{2 3}Q"), .success(.hashmap([.int(2), .int(3)]), "Q"))
        XCTAssertEqual(hashmap("{}Z"), .success(.hashmap([]), "Z"))
        XCTAssertEqual(hashmap("{2 {3 4}}Z"), .success(.hashmap([.int(2), .hashmap([.int(3), .int(4)])]), "Z"))
    }

    func testSym() {
        XCTAssertEqual(symbol("true"), .success(.bool(true), ""))
        XCTAssertEqual(symbol("false"), .success(.bool(false), ""))
        XCTAssertEqual(symbol("nil"), .success(.null, ""))
        XCTAssertEqual(symbol("+"), .success(.sym("+"), ""))
        XCTAssertEqual(symbol("-"), .success(.sym("-"), ""))
        XCTAssertEqual(symbol("abc"), .success(.sym("abc"), ""))
    }

*/

}
