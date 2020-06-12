//
//  tests.swift
//  tests
//
//  Created by Joe Watson on 2020/06/12.
//  Copyright © 2020 Joe Watson. All rights reserved.
//

import XCTest

class tests: XCTestCase {

    func testChar() throws {
        XCTAssertEqual(char("z")("zoo"), ParserResult.success("z", "oo"))
        XCTAssertEqual(char("z")("zoo"), ParserResult.success("z", "oo"))
        XCTAssertEqual(char("z")("abc"), ParserResult.failure("Expected 'z'", "abc"))
        XCTAssertEqual(char("z")(""), ParserResult.failure("Expected 'z'", ""))
        XCTAssertEqual(char("a")("azoo"), ParserResult.success("a", "zoo"))
        XCTAssertEqual(char("a")("zabc"), ParserResult.failure("Expected 'a'", "zabc"))
        XCTAssertEqual(char("a")(""), ParserResult.failure("Expected 'a'", ""))
    }

    func testString() throws {
        XCTAssertEqual(string("abc")("abcde"), ParserResult.success("abc", "de"))
        XCTAssertEqual(string("abc")("ab"), ParserResult.failure("Expected 'abc'", "ab"))
        XCTAssertEqual(string("abc")(""), ParserResult.failure("Expected 'abc'", ""))
    }

    func testChoice() throws {
        let s3 : Parser<String> = choice(string("abc"), string("def"), string("axe"))
        XCTAssertEqual(s3("abcde"), ParserResult.success("abc", "de"))
        XCTAssertEqual(s3("defgh"), ParserResult.success("def", "gh"))
        XCTAssertEqual(s3("axe!!"), ParserResult.success("axe", "!!"))
        XCTAssertEqual(s3("pqr"), ParserResult.failure("Expected one of multiple choices", "pqr"))
    }

    func testMany() throws {
        XCTAssertEqual(many(char("a"))("aaabc"), ParserResult.success(["a", "a", "a"], "bc"))
        XCTAssertEqual(many(char("a"))("xbc"), ParserResult.success([], "xbc"))
        XCTAssertEqual(many(char("a"))(""), ParserResult.success([], ""))
    }

    func testMany1() throws {
        XCTAssertEqual(many1(char("a"))("aaabc"), ParserResult.success(["a", "a", "a"], "bc"))
        XCTAssertEqual(many1(char("a"))("xbc"), ParserResult.failure("Expected 'a'", "xbc"))
        XCTAssertEqual(many1(char("a"))(""), ParserResult.failure("Expected 'a'", ""))
    }

    
    func testStar() throws {
        let p = string("abc") *> string("def")
        XCTAssertEqual(p("abcdefgh"), ParserResult.success("def", "gh"))
        XCTAssertEqual(p("abdefgh"), ParserResult.failure("Expected 'abc'", "abdefgh"))
        XCTAssertEqual(p("abceh"), ParserResult.failure("Expected 'def'", "eh"))
    }
}
