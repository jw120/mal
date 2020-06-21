// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer_tests - tests for the printer module

import malLib
import XCTest

public class PrinterTests: XCTestCase {
    public func testPrStr() throws {
        XCTAssertEqual(pr_str(.int(23)), "23")
        XCTAssertEqual(pr_str(.list([])), "()")
        XCTAssertEqual(pr_str(.list([.int(2)])), "(2)")
        XCTAssertEqual(pr_str(.list([.int(2), .int(3)])), "(2 3)")
    }
}
