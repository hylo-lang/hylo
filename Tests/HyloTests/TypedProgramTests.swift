import FrontEnd
import XCTest

extension AST {
  fileprivate struct ASTWalker<DeclT: SingleEntityDecl>: ASTWalkObserver {
    let targetName: String
    var result: DeclT.ID? = nil

    init(targetName: String) {
      self.targetName = targetName
    }

    mutating func willEnter(_ id: AnyNodeID, in ast: AST) -> Bool {
      if let d = DeclT.ID(id), ast[d].baseName == targetName {
        result = d
        return false
      }
      return true
    }
  }

  /// Searches for any occurrence of a specific declaration in the AST with the given name.
  fileprivate func lookupSimplyByName<DeclT: SingleEntityDecl>(name: String, type: DeclT.Type)
    -> DeclT.ID?
  {
    var walker = ASTWalker<DeclT>(targetName: name)
    for m in modules {
      walk(m, notifying: &walker)
    }
    return walker.result
  }
}

final class TypedProgramTests: XCTestCase {
  func testUnqualifiedLookup() throws {
    // Given a source file with a type and a function
    let source = SourceFile.diagnosableLiteral(
      """
      type A {}
      type Foo {
        fun add(x: A) {
          
        }
      }
      """)

    var ast = AST()

    let moduleId = try checkNoDiagnostic { d in
      return try ast.makeModule(
        "Main", sourceCode: [source], builtinModuleAccess: false, diagnostics: &d)
    }

    let typedProgram = try checkNoDiagnostic { d in
      return try TypedProgram(annotating: ScopedProgram(ast), reportingDiagnosticsTo: &d)
    }

    let fooId = try XCTUnwrap(ast.lookupSimplyByName(name: "Foo", type: ProductTypeDecl.self))

    // When performing an unqualified lookup from the type's scope, the type should be found
    let fooResult = typedProgram.lookup(unqualified: "Foo", in: AnyScopeID(fooId))
    XCTAssertEqual(fooResult.count, 1)
    XCTAssertEqual(try XCTUnwrap(fooResult.first), AnyDeclID(fooId))

    // When performing an unqualified lookup from the module's scope, the type should be found
    let fooResult2 = typedProgram.lookup(unqualified: "Foo", in: AnyScopeID(moduleId))
    XCTAssertEqual(fooResult2.count, 1)
    XCTAssertEqual(try XCTUnwrap(fooResult2.first), AnyDeclID(fooId))
  }

  func testQualifiedLookupWithinAType() throws {
    let source = SourceFile.diagnosableLiteral(
      """
      type A1 {}
      type A2 {}
      type Foo {
        typealias B = A1
        typealias C = A2
      }
      """)

    var ast = AST()

    let moduleId = try checkNoDiagnostic { d in
      return try ast.makeModule(
        "Main", sourceCode: [source], builtinModuleAccess: false, diagnostics: &d)
    }

    let typedProgram = try checkNoDiagnostic { d in
      return try TypedProgram(annotating: ScopedProgram(ast), reportingDiagnosticsTo: &d)
    }

    let fooId = try XCTUnwrap(ast.lookupSimplyByName(name: "Foo", type: ProductTypeDecl.self))

    // When looking up B from Foo's nominal scope, B should be found
    let bResult = typedProgram.lookup(
      "B", memberOf: ^ProductType(fooId, ast: ast), exposedTo: AnyScopeID(moduleId))
    XCTAssertEqual(bResult.count, 1)

    let bExpected = ast.lookupSimplyByName(name: "B", type: TypeAliasDecl.self)!
    XCTAssertEqual(try XCTUnwrap(bResult.first), AnyDeclID(bExpected))
  }

  func testQualifiedLookupWithinATypeAlias() throws {
    let source = SourceFile.diagnosableLiteral(
      """
      type A1 {
        type Inner {}
      }
      type Foo {
        typealias B = A1
      }
      """)

    var ast = AST()

    let moduleId = try checkNoDiagnostic { d in
      return try ast.makeModule(
        "Main", sourceCode: [source], builtinModuleAccess: false, diagnostics: &d)
    }

    let typedProgram = try checkNoDiagnostic { d in
      return try TypedProgram(annotating: ScopedProgram(ast), reportingDiagnosticsTo: &d)
    }

    let typeAliasB = ast.lookupSimplyByName(name: "B", type: TypeAliasDecl.self)!

    // when looking up an identifier (Inner) within the nominal scope of a type alias (B), it should be looked up in the aliased type (A1)
    let typeAliasTypeBType = MetatypeType(typedProgram[typeAliasB].type)!.instance
    let innerResult = typedProgram.lookup(
      "Inner", memberOf: typeAliasTypeBType, exposedTo: AnyScopeID(moduleId))
    XCTAssertEqual(innerResult.count, 1)

    let innerExpected = ast.lookupSimplyByName(name: "Inner", type: ProductTypeDecl.self)!
    XCTAssertEqual(try XCTUnwrap(innerResult.first), AnyDeclID(innerExpected))
  }
}
