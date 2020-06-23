// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer_tests - tests for the printer module

import malLib
import XCTest

public class PrinterTests: XCTestCase {
    public func testPrStr() throws {
        XCTAssertEqual(Mal.int(23).print(), "23")
        XCTAssertEqual(Mal.list([]).print(), "()")
        XCTAssertEqual(Mal.list([.int(2)]).print(), "(2)")
        XCTAssertEqual(Mal.list([.int(2), .int(3)]).print(), "(2 3)")
    }
}
