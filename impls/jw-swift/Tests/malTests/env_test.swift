// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// env_tests - tests for the env module

import malLib
import XCTest

public class EnvTests: XCTestCase {
    public func testEnv() throws {
        let a = Env()
        a.set("x", .int(7))
        a.set("y", .int(8))
        let b = Env(outer: a)
        b.set("y", .int(4))
        b.set("z", .int(6))
        XCTAssertEqual(try a.get("x"), .int(7))
        XCTAssertEqual(try a.get("y"), .int(8))
        XCTAssertThrowsError(try a.get("z"))
        XCTAssertEqual(try b.get("x"), .int(7))
        XCTAssertEqual(try b.get("y"), .int(4))
        XCTAssertEqual(try b.get("z"), .int(6))
        XCTAssertEqual(a.find("x"), a)
        XCTAssertEqual(a.find("y"), a)
        XCTAssertNil(a.find("z"))
        XCTAssertEqual(b.find("x"), a)
        XCTAssertEqual(b.find("y"), b)
        XCTAssertEqual(b.find("z"), b)
    }
}
