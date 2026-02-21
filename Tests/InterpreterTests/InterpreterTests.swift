import Driver
import Foundation
import FrontEnd
import IR
import XCTest
import Utils

@testable import Interpreter

final class InterpreterRunTests: XCTestCase {

  func testEmptyMain() throws {
    let p =
      """
        public fun main() { }
      """.asSourceFile();
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

  func testLocalVariables() throws {
    let p =
      """
        public fun main() {
          let x = 2 as Int32
          let y = 4 as Int64
        }
      """.asSourceFile()
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

  func testCopyingBuiltin() throws {
    let p =
      """
        public fun main() {
          let x = 2;
          var y = x;
        }
      """.asSourceFile()
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

}
