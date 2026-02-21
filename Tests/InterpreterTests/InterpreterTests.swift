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
  
  func testConditionals() throws {
    let p =
      """
        public fun main() {
          var n = 0
          if true {
            &n = 1
          } else {
            &n = 2
          }

          if false {
            &n = 3
          }else{
            &n = 4
          }
        }
      """.asSourceFile()
    try p.runOnInterpreterAsMainWithHostedStandardLibrary()
  }

}
