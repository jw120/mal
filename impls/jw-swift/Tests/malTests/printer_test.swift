// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer_tests - tests for the printer module

import malLib
import XCTest

public class PrinterTests: XCTestCase {
    public func testPrStr() throws {
        XCTAssertEqual(.int(23).print(), "23")
        XCTAssertEqual(.list([]).print(), "()")
        XCTAssertEqual(.list([.int(2)])), "(2)")
        XCTAssertEqual(.list([.int(2), .int(3)]).print(), "(2 3)")
    }
}
