// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer_tests - tests for the printer module

import malLib
import XCTest

public class PrinterTests: XCTestCase {
    public func testPrStr() throws {
        XCTAssertEqual(Mal.int(23).pr_str(), "23")
        XCTAssertEqual(Mal.list([]).pr_str(), "()")
        XCTAssertEqual(Mal.list([.int(2)]).pr_str(), "(2)")
        XCTAssertEqual(Mal.list([.int(2), .int(3)]).pr_str(), "(2 3)")
    }
}
