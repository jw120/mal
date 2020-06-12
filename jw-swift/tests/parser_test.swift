//
//  tests.swift
//  tests
//
//  Created by Joe Watson on 2020/06/12.
//  Copyright © 2020 Joe Watson. All rights reserved.
//

import XCTest

// Helper function to test parser
func testParser<T>(_ description: String, parser: Parser<T>, input: String, expected: ParserResult<T>) {
    let result = parser(Substring.init(input))
    XCTAssertEqual(result, expected, description)
}

class tests: XCTestCase {

    func testChar() throws {
        testParser("char z present", parser: char("z"), input: "zoo", expected: ParserResult.success("z", "oo"))
        testParser("char z missing", parser: char("z"), input: "abc", expected: ParserResult.failure("Expected 'z'", "abc"))
        testParser("char z empty", parser: char("z"), input: "", expected: ParserResult.failure("Expected 'z'", ""))
        testParser("char a present", parser: char("a"), input: "azoo", expected: ParserResult.success("a", "zoo"))
        testParser("char a missing", parser: char("a"), input: "zabc", expected: ParserResult.failure("Expected 'a'", "zabc"))
        testParser("char a empty", parser: char("a"), input: "", expected: ParserResult.failure("Expected 'a'", ""))
    }

    func testString() throws {
        testParser("string abc present", parser: string("abc"), input: "abcde", expected: ParserResult.success("abc", "de"))
        testParser("string abc missing", parser: string("abc"), input: "ab", expected: ParserResult.failure("Expected 'abc'", "ab"))
        testParser("string abc empty", parser: string("abc"), input: "", expected: ParserResult.failure("Expected 'abc'", ""))
    }

    func testChoice() throws {
        let s3 : Parser<String> = choice(string("abc"), string("def"), string("axe"))
        testParser("choice s3 1st present", parser: s3, input: "abcde", expected: ParserResult.success("abc", "de"))
        testParser("choice s3 2nd present", parser: s3, input: "defgh", expected: ParserResult.success("def", "gh"))
        testParser("choice s3 3rd present", parser: s3, input: "axe!!", expected: ParserResult.success("axe", "!!"))
        testParser("choice s3 missing", parser: s3, input: "pqr", expected: ParserResult.failure("Expected one of multiple choices", "pqr"))
    }

    func testStar() throws {
        testParser("*> matching", parser: string("abc") *> string("def"), input: "abcdefgh", expected: ParserResult.success("def", "gh"))
        testParser("*> failing 1st", parser: string("abc") *> string("def"), input: "abdefgh", expected: ParserResult.failure("Expected 'abc'", "abdefgh"))
        testParser("*> failing 2nd", parser: string("abc") *> string("def"), input: "abceh", expected: ParserResult.failure("Expected 'def'", "eh"))
    }
}
