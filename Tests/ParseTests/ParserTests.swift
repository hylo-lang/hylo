import XCTest
import Compiler

final class ParserTests: XCTestCase {

  func testInfix() {
    let parser = Parser()

    with(parser.parseExpr("a+ b +c"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      XCTAssert(expr is CallExpr)
    })

    with(parser.parseExpr("a+ b +c"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      XCTAssert(expr?[as: CallExpr.self]?.callee is UnresolvedMemberExpr)
    })

    with(parser.parseExpr("a+ b"), { (expr, diags) in
      XCTAssertFalse(diags.isEmpty)
    })
  }

  func testPostfix() {
    let parser = Parser()

    with(parser.parseExpr("a++"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      XCTAssert(expr is CallExpr)
    })

    with(parser.parseExpr("a<"), { (expr, diags) in
      XCTAssertFalse(diags.isEmpty)
    })
  }

  func testTypeArgs() {
    let parser = Parser()

    with(parser.parseExpr("a< b>"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      XCTAssert(expr is SpecializedDeclRefExpr)
    })

    with(parser.parseExpr("a < b > c"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      XCTAssert(expr is CallExpr)
    })

    with(parser.parseExpr("a<>"), { (expr, diags) in
      XCTAssertFalse(diags.isEmpty)
    })

    with(parser.parseExpr("a< b >c"), { (expr, diags) in
      XCTAssertFalse(diags.isEmpty)
    })
  }

  func testFunDeclRef() {
    let parser = Parser()

    with(parser.parseExpr("a"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      assuming(expr, is: UnresolvedDeclRefExpr.self, { expr in
        XCTAssertEqual(expr.ident.base, "a")
        XCTAssertEqual(expr.ident.labels, [])
        XCTAssertNil(expr.ident.notation)
      })
    })

    with(parser.parseExpr("a(_:b:)"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      assuming(expr, is: UnresolvedDeclRefExpr.self, { expr in
        XCTAssertEqual(expr.ident.base, "a")
        XCTAssertEqual(expr.ident.labels, ["_", "b"])
        XCTAssertNil(expr.ident.notation)
      })
    })

    with(parser.parseExpr("(prefix>>)"), { (expr, diags) in
      XCTAssert(diags.isEmpty)
      assuming(expr, is: TupleExpr.self, { expr in
        assuming(expr.elems.first?.value, is: UnresolvedDeclRefExpr.self, { expr in
          XCTAssertEqual(expr.ident.base, ">>")
          XCTAssertEqual(expr.ident.labels, [])
          XCTAssertEqual(expr.ident.notation, .prefix)
        })
      })
    })
  }

}

extension Node {

  subscript<T>(as _: T.Type) -> T? {
    return self as? T
  }

}

func with<T>(_ value: T, _ action: (T) throws -> ()) rethrows {
  try action(value)
}

func assuming<T, U>(_ value: T, is: U.Type, _ action: (U) throws -> ()) rethrows {
  if let v = value as? U {
    try action(v)
  } else {
    XCTFail("\(value) does not have type '\(U.self)'")
  }
}
