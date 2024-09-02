import Durian
import FrontEnd
import TestUtils
import Utils
import XCTest

@testable import FrontEnd

final class ParserTests: XCTestCase {

  // MARK: Unit tests

  func testParse() throws {
    let input = SourceFile.diagnosableLiteral(
      """
      public fun main() {
        print("Hello, World!")
      }
      """)

    var a = AST()
    try checkNoDiagnostic { d in
      _ = try a.loadModule("Main", parsing: [input], reportingDiagnosticsTo: &d)
    }
  }

  func testSourceFile() throws {
    let input = SourceFile.diagnosableLiteral(
      """
        ;;
        import Foo

        @ffi("_val_bar")
        fun _bar(x: Builtin.i64) -> Builtin.i64

        let x = "Hello!"
        public let y = 0;
      """)

    var a = AST()
    let m = try checkNoDiagnostic { d in
      try a.loadModule("Main", parsing: [input], reportingDiagnosticsTo: &d)
    }
    XCTAssertEqual(a[a[m].sources.first!].decls.count, 4)
  }

  // MARK: Declarations

  func testModuleMember() throws {
    let input: SourceFile = "public operator infix| : disjunction"
    let (declID, ast) = try input.parse(with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? OperatorDecl)
    XCTAssertEqual(decl.name.value, "|")
    XCTAssertEqual(decl.precedenceGroup?.value, .disjunction)
  }

  func testImportDecl() throws {
    let input: SourceFile = "import Foo"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseImportDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.baseName, "Foo")
  }

  func testNamespaceDecl() throws {
    let input: SourceFile = """
      namespace A {
        ;;
        let x = "Hello!"
        public let y = 0;
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseNamespaceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testNamespaceMember() throws {
    let input: SourceFile = "fun foo() {}"
    let (declID, ast) = try input.parse(inContext: .namespaceBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertNotNil(declID)
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
  }

  func testNamespaceMemberPrivate() throws {
    let input: SourceFile = "private fun foo() {}"
    let (declID, ast) = try input.parse(inContext: .namespaceBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier.value, .private)
  }

  func testNamespaceMemberPublic() throws {
    let input: SourceFile = "public fun foo() {}"
    let (declID, ast) = try input.parse(inContext: .namespaceBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier.value, .public)
  }

  func testTypeAliasDecl() throws {
    let input: SourceFile = "typealias A = B"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTypeAliasDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "A")
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
  }

  func testTypeAliasDeclWithGenericClause() throws {
    let input: SourceFile = "typealias A<T> = B<T>"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTypeAliasDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
  }

  func testProductTypeDecl() throws {
    let input: SourceFile = """
      type A {
        var x: Int; var y: Int
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)
  }

  func testProductTypeDeclWithGenericClause() throws {
    let input: SourceFile = """
      type A<T, U> {
              var x: Int; var y: Int
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
  }

  func testProductTypeDeclWithConformances() throws {
    let input: SourceFile = """
      type A: Foo, Bar {
        var x: Int; var y: Int
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testProductTypeDeclWithGenericClauseAndConformances() throws {
    let input: SourceFile = """
      type A<T>: Foo {
        var x: Int
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.conformances)
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
  }

  func testProductTypeMember() throws {
    let input: SourceFile = "var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertNotNil(declID)
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
  }

  func testProductTypeMemberPrivate() throws {
    let input: SourceFile = "private var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier.value, .private)
  }

  func testProductTypeMemberPublic() throws {
    let input: SourceFile = "public var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier.value, .public)
  }

  func testProductTypeMemberStatic() throws {
    let input: SourceFile = "static var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier.value, .private)  // implicitly declared as private
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testProductTypeMemberPrivateStatic() throws {
    let input: SourceFile = "private static var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier.value, .private)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testProductTypeMemberPublicStatic() throws {
    let input: SourceFile = "public static var x: Int"
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testTraitDecl() throws {
    let input: SourceFile = """
      trait A {
        type B
        property b: B { let }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTraitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)  // 2 explicit decls + 1 implicit `Self` parameter
  }

  func testTraitDeclWithRefinements() throws {
    let input: SourceFile = """
      trait A: Foo {
        type B
        property b: B { let }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTraitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.bounds.count, 1)
  }

  func testMethodBundleRequirement() throws {
    let input: SourceFile = """
      fun foo() -> T {
        let
        inout
      }
      """
    let (declID, ast) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? MethodDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testSubscriptRequirement() throws {
    let input: SourceFile = """
      subscript foo(): T {
        let
        inout
      }
      """
    let (declID, ast) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? SubscriptDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testPropertyRequirement() throws {
    let input: SourceFile = "property foo: T { let }"
    let (declID, _) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    XCTAssertNotNil(declID)
  }

  func testAssociatedTypeDecl() throws {
    let input: SourceFile = "type Foo"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "Foo")
  }

  func testAssociatedTypeDeclWithConformances() throws {
    let input: SourceFile = "type Foo: Bar, Ham"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testAssociatedTypeDeclWithWhereClause() throws {
    let input: SourceFile = "type Foo where Foo.Bar == Ham"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testAssociatedTypeDeclWithWithDefault() throws {
    let input: SourceFile = "type Foo = X"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testAssociatedValueDecl() throws {
    let input: SourceFile = "value foo"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "foo")
  }

  func testAssociatedValueDeclWithWhereClause() throws {
    let input: SourceFile = "value foo where @value foo > bar"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testAssociatedValueDeclWithDefault() throws {
    let input: SourceFile = "value foo = 42"
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody,
      with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testConformanceDecl() throws {
    let input: SourceFile = """
      conformance A: Foo {
        public fun bar() {}
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseConformanceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testConformanceDeclWithWhereClause() throws {
    let input: SourceFile = """
      conformance A: Foo where A.Bar == Ham {
        public fun bar() {}
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseConformanceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionDecl() throws {
    let input: SourceFile = """
      extension A {
        type B {}; property z: Int { x }
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseExtensionDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)
  }

  func testExtensionDeclWithWhereClause() throws {
    let input: SourceFile = """
      extension A where Foo: Bar {
        type B {}; property z: Int { x }
        fun foo() -> Int { x.copy() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseExtensionDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionMemberPrivate() throws {
    let input: SourceFile = "private static fun forty_two() -> Int { 42 }"
    let (declID, ast) = try input.parse(inContext: .extensionBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier.value, .private)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testExtensionMemberPublic() throws {
    let input: SourceFile = "public static fun forty_two() -> Int { 42 }"
    let (declID, ast) = try input.parse(inContext: .extensionBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testBindingDecl() throws {
    let input: SourceFile = "let (foo, bar)"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseBindingDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(ast[decl.pattern].introducer.value, .let)
  }

  func testBindingDeclWithInitializer() throws {
    let input: SourceFile = "let (foo, bar) = (true, ham())"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseBindingDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.initializer)
  }

  func testMemberwiseInitDecl() throws {
    let input: SourceFile = "memberwise init"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseMemberwiseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .memberwiseInit)
  }

  func testInitDecl() throws {
    let input: SourceFile = "init() {}"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.body)
  }

  func testInitDeclGeneric() throws {
    let input: SourceFile = "init<T>() {}"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDecl() throws {
    let input: SourceFile = "fun foo() {}"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDeclWithCaptureList() throws {
    let input: SourceFile = "fun foo[let x = 42]() {}"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.explicitCaptures.count, 1)
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDeclWithExprBody() throws {
    let input: SourceFile = "fun id<T: Movable>(_ x: T) -> T { x }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testPostifxFunctionDecl() throws {
    let input: SourceFile = "fun postfix+ () -> T { x }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.notation?.value, .postfix)
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDeclWithAutoclosure() throws {
    let input: SourceFile = "fun foo<E>(value: @autoclosure ([E]() -> Int)) { &value() }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertNotNil(decl)
    XCTAssertEqual(decl.parameters.count, 1)
    let p = ast[decl.parameters[0]]
    let e = try XCTUnwrap(ast[p.annotation])
    XCTAssert(e.isAutoclosure)
  }

  func testMethodBundle() throws {
    let input: SourceFile = """
      fun foo() {
        let  { self.copy() }
        sink { self }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? MethodDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testFunctionDeclSignature() throws {
    let input: SourceFile = "()"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 0)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithParameters() throws {
    let input: SourceFile = "(_ foo: Foo, bar: Bar = .default)"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 2)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithEffect() throws {
    let input: SourceFile = "(_ foo: Foo) inout"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .inout)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithOutput() throws {
    let input: SourceFile = "(_ foo: Foo) -> C"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertEqual(signature.output?.kind, NodeKind(NameExpr.self))
  }

  func testFunctionDeclSignatureWithOutputAndEffect() throws {
    let input: SourceFile = "(_ foo: Foo) sink -> C"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .sink)
    XCTAssertEqual(signature.output?.kind, .init(NameExpr.self))
  }

  func testFunctionDeclIdentifier() throws {
    let input: SourceFile = "fun foo"
    let identifier = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclHead(in:)).element)
    XCTAssertEqual(identifier.stem.value, "foo")
    XCTAssertNil(identifier.notation)
  }

  func testFunctionDeclOperator() throws {
    let input: SourceFile = "fun postfix++"
    let identifier = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclHead(in:)).element)
    XCTAssertEqual(identifier.stem.value, "++")
    XCTAssertEqual(identifier.notation?.value, .postfix)
  }

  func testFunctionDeclBodyBlock() throws {
    let input: SourceFile = "{}"
    let (body, _) = try apply(Parser.functionBody, on: input)
    if case .block = body {
    } else {
      XCTFail()
    }
  }

  func testFunctionDeclBodyExpr() throws {
    let input: SourceFile = "{ 0x2a }"
    let (body, _) = try apply(Parser.functionBody, on: input)
    if case .expr = body {
    } else {
      XCTFail()
    }
  }

  func testMethodDeclBody() throws {
    let input: SourceFile = """
      {
        let  { self.copy() }
        sink { self }
      }
      """
    let (body, _) = try apply(Parser.methodDeclBody, on: input)
    let impls = try XCTUnwrap(body)
    XCTAssertEqual(impls.count, 2)
  }

  func testMethodImplBlock() throws {
    let input: SourceFile = "let { }"
    let (declID, ast) = try apply(Parser.methodImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .block = decl.body {
    } else {
      XCTFail()
    }
  }

  func testMethodImplExpr() throws {
    let input: SourceFile = "let { foo }"
    let (declID, ast) = try apply(Parser.methodImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testPropertyDecl() throws {
    let input: SourceFile = "property foo: T { T() }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parsePropertyDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssert(decl.isProperty)
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertEqual(decl.parameters.count, 0)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDecl() throws {
    let input: SourceFile = "subscript foo(): T { T() }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertEqual(decl.parameters.count, 0)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclAnonymous() throws {
    let input: SourceFile = "subscript (): T { T() }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertNil(decl.identifier)
    XCTAssertEqual(decl.parameters.count, 0)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithCaptureList() throws {
    let input: SourceFile = "subscript foo[let x = 42](): T { T() }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertEqual(decl.explicitCaptures.count, 1)
    XCTAssertEqual(decl.parameters.count, 0)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithBlockBody() throws {
    let input: SourceFile = "subscript foo<T: Foo>(_ x: T): T { yield x }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertEqual(decl.parameters.count, 1)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithExprBody() throws {
    let input: SourceFile = "subscript foo<T: Foo>(_ x: T): T { x }"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertEqual(decl.parameters.count, 1)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptBundle() throws {
    let input: SourceFile = """
      subscript foo(): T {
        let  { T() }
        sink { T() }
      }
      """
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertFalse(decl.isProperty)
    XCTAssertEqual(decl.parameters.count, 0)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testSubscriptDeclSignature() throws {
    let input: SourceFile = "(): T"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseSubscriptDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 0)
  }

  func testSubscriptDeclSignatureWithParameters() throws {
    let input: SourceFile = "(_ foo: Foo, bar: Bar = .default): T"
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseSubscriptDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 2)
  }

  func testSubscriptDeclBodyBlock() throws {
    let input: SourceFile = "{ yield x }"
    let (body, ast) = try input.parse(
      inContext: .subscriptBody,
      with: { (state) in try Parser.parseSubscriptDeclBody(in: &state, asNonStaticMember: true) })

    XCTAssertEqual(body?.count, 1)
    if let impl = ast[body?.first] {
      XCTAssertEqual(impl.introducer.value, .let)
      if case .block = impl.body {
      } else {
        XCTFail()
      }
    }
  }

  func testSubscriptDeclBodyExpr() throws {
    let input: SourceFile = "{ 0x2a }"
    let (body, ast) = try input.parse(
      inContext: .subscriptBody,
      with: { (state) in try Parser.parseSubscriptDeclBody(in: &state, asNonStaticMember: true) })

    XCTAssertEqual(body?.count, 1)
    if let impl = ast[body?.first] {
      XCTAssertEqual(impl.introducer.value, .let)
      if case .expr = impl.body {
      } else {
        XCTFail()
      }
    }
  }

  func testSubscriptDeclBodyBundle() throws {
    let input: SourceFile = """
      {
        let  { self.copy() }
        sink { self }
      }
      """
    let (body, _) = try input.parse(
      inContext: .subscriptBody,
      with: { (state) in try Parser.parseSubscriptDeclBody(in: &state, asNonStaticMember: true) })

    XCTAssertEqual(body?.count, 2)
  }

  func testParameterDecl() throws {
    let input: SourceFile = "_ foo"
    let (d, ast) = try input.parse(with: Parser.parseParameterDecl(in:))
    XCTAssertEqual(ast[d]?.baseName, "foo")
  }

  func testParameterDeclWithAnnotation() throws {
    let input: SourceFile = "_ foo: T"
    let (d, ast) = try input.parse(with: Parser.parseParameterDecl(in:))
    XCTAssertNotNil(ast[d]?.annotation)
  }

  func testParameterDeclWithDefault() throws {
    let input: SourceFile = "_ foo: T = T()"
    let (d, ast) = try input.parse(with: Parser.parseParameterDecl(in:))
    XCTAssertNotNil(ast[d]?.defaultValue)
  }

  func testParameterDeclImplicit() throws {
    let input: SourceFile = "_ foo?: T"
    let (d, ast) = try input.parse(with: Parser.parseParameterDecl(in:))
    let tree = try XCTUnwrap(ast[d])
    XCTAssert(tree.isImplicit)
  }

  func testParameterInterfaceLabelAndName() throws {
    let input: SourceFile = "for name"
    let tree = try XCTUnwrap(input.parse(with: Parser.parseParameterInterface(in:)).element)
    XCTAssertEqual(tree.label?.value, "for")
    XCTAssertEqual(tree.name.value, "name")
    XCTAssertNil(tree.implicitMarker)
  }

  func testParameterInterfaceUnderscoreAndName() throws {
    let input: SourceFile = "_ name"
    let tree = try XCTUnwrap(input.parse(with: Parser.parseParameterInterface(in:)).element)
    XCTAssertNil(tree.label)
    XCTAssertEqual(tree.name.value, "name")
    XCTAssertNil(tree.implicitMarker)
  }

  func testParameterInterfaceOnlyName() throws {
    let input: SourceFile = "name"
    let tree = try XCTUnwrap(input.parse(with: Parser.parseParameterInterface(in:)).element)
    XCTAssertEqual(tree.label?.value, "name")
    XCTAssertEqual(tree.name.value, "name")
    XCTAssertNil(tree.implicitMarker)
  }

  func testParameterInterfaceImplicit() throws {
    let input: SourceFile = "in context?"
    let tree = try XCTUnwrap(input.parse(with: Parser.parseParameterInterface(in:)).element)
    XCTAssertEqual(tree.label?.value, "in")
    XCTAssertEqual(tree.name.value, "context")
    XCTAssertNotNil(tree.implicitMarker)
  }

  func testOperatorDecl() throws {
    let input: SourceFile = "operator infix+"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseOperatorDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.notation.value, .infix)
    XCTAssertNil(decl.precedenceGroup)
  }

  func testOperatorDeclWithPrecedenceGroup() throws {
    let input: SourceFile = "operator infix+ : addition"
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseOperatorDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.notation.value, .infix)
    XCTAssertEqual(decl.precedenceGroup?.value, .addition)
  }

  func testGenericClause() throws {
    let input: SourceFile = "<T>"
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 1)
  }

  func testGenericClauseWithMultipleParameters() throws {
    let input: SourceFile = "<T, n: Int>"
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 2)
  }

  func testGenericClauseWithMultipleParametersSansHint() throws {
    let input: SourceFile = "<T, n: Int>"
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 2)
  }

  func testCaptureList() throws {
    let input: SourceFile = "[let x = a, var y = true]"
    let list = try XCTUnwrap(try apply(Parser.captureList, on: input).element)
    XCTAssertEqual(list.count, 2)
  }

  func testGenericClauseWithWhereClause() throws {
    let input: SourceFile = "<T: Foo where T.Bar == {}>"
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertNotNil(clause.value.whereClause)
  }

  func testGenericParameter() throws {
    let input: SourceFile = "T"
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.baseName, "T")
  }

  func testGenericParameterWithTypeIntroducer() throws {
    let input: SourceFile = "type T"
    let (d, a) = try apply(Parser.genericParameter, on: input)
    let n = try XCTUnwrap(a[d])
    XCTAssertEqual(n.baseName, "T")
    XCTAssertEqual(n.introducer?.value, .type)
  }

  func testGenericParameterWithValueIntroducer() throws {
    let input: SourceFile = "value T"
    let (d, a) = try apply(Parser.genericParameter, on: input)
    let n = try XCTUnwrap(a[d])
    XCTAssertEqual(n.baseName, "T")
    XCTAssertEqual(n.introducer?.value, .value)
  }

  func testGenericParameterWithConformances() throws {
    let input: SourceFile = "T: Foo & Bar"
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.conformances.count, 2)
  }

  func testGenericParameterWithDefault() throws {
    let input: SourceFile = "T = U"
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testGenericParameterWithConformancesAndDefault() throws {
    let input: SourceFile = "T: Int = 0o52"
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.conformances.count, 1)
    XCTAssertNotNil(decl.defaultValue)
  }

  func testConformanceList() throws {
    let input: SourceFile = ": Foo, Bar, Ham"
    let list = try XCTUnwrap(try apply(Parser.conformanceList, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Value expressions

  func testExpr() throws {
    let input: SourceFile = "(foo().bar[] + 42, ham++, !baz)"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(TupleExpr.self))
  }

  func testInfixExpr() throws {
    let input: SourceFile = "foo == 2 & true"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let sequence = try XCTUnwrap(ast[exprID] as? SequenceExpr)
    XCTAssertEqual(sequence.head.kind, .init(NameExpr.self))
    XCTAssertEqual(sequence.tail.count, 2)
    if sequence.tail.count == 2 {
      XCTAssertEqual(ast[sequence.tail[0].operator].name.value.stem, "==")
      XCTAssertEqual(ast[sequence.tail[0].operator].name.value.notation, .infix)
      XCTAssertEqual(sequence.tail[0].operand.kind, .init(IntegerLiteralExpr.self))
    }
  }

  func testCastExprUp() throws {
    let input: SourceFile = "foo as T"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.direction, .up)
  }

  func testCastExprDown() throws {
    let input: SourceFile = "foo as! T"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.direction, .down)
  }

  func testCastExprBuiltinPointerConversion() throws {
    let input: SourceFile = "foo as* T"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.direction, .pointerConversion)
  }

  func testInoutExpr() throws {
    let input: SourceFile = "&foo.bar.ham"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(InoutExpr.self))
  }

  func testPrefixExpr() throws {
    let input: SourceFile = "+foo"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let call = try XCTUnwrap(ast[exprID] as? FunctionCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .prefix)

    if case .explicit(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testPostfixExpr() throws {
    let input: SourceFile = "foo+"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let call = try XCTUnwrap(ast[exprID] as? FunctionCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .postfix)

    if case .explicit(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testMethodCallOnIntegerLiteral() throws {
    // See #1037
    let input: SourceFile = "1.copy()"
    let (e, ast) = try input.parse(with: Parser.parseExpr(in:))
    let call = try XCTUnwrap(ast[e] as? FunctionCallExpr)
    XCTAssertEqual(input[ast[call.callee].site], "1.copy")
  }

  // MARK: Compound expressions

  func testLabeledMemberExpr() throws {
    let input: SourceFile = "a.b.c"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    var expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "c")

    if case .explicit(let domainID) = expr.domain {
      expr = try XCTUnwrap(ast[domainID] as? NameExpr)
      XCTAssertEqual(expr.name.value.stem, "b")
    } else {
      XCTFail()
    }
  }

  func testIndexedMemberExpr() throws {
    let input: SourceFile = "foo.12"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleMemberExpr)
    XCTAssertEqual(expr.index.value, 12)

    let parentExpr = try XCTUnwrap(ast[expr.tuple] as? NameExpr)
    XCTAssertEqual(parentExpr.name.value.stem, "foo")
  }

  func testStaticValueMemberExpr() throws {
    let input: SourceFile = "{ A, B }.meta"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "meta")

    if case .explicit(let domainID) = expr.domain {
      XCTAssertEqual(domainID.kind, .init(TupleTypeExpr.self))
    } else {
      XCTFail()
    }
  }

  func testConformanceLensExpr() throws {
    let input: SourceFile = "{ T, U }::Baz"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ConformanceLensExpr)
    XCTAssertEqual(expr.subject.kind, .init(TupleTypeExpr.self))
    XCTAssertEqual(expr.lens.kind, .init(NameExpr.self))
  }

  func testConformanceLensExprWithMember() throws {
    let input: SourceFile = "T::P.A"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "A")

    if case .explicit(let domain) = expr.domain {
      let d = try XCTUnwrap(ast[domain] as? ConformanceLensExpr)
      XCTAssertEqual(d.subject.kind, .init(NameExpr.self))
      XCTAssertEqual(d.lens.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testFunctionCallExprWithoutArguments() throws {
    let input: SourceFile = "foo()"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? FunctionCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testFunctionCallExpr() throws {
    let input: SourceFile = "foo(42, label: true)"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? FunctionCallExpr)
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "label")
    }
  }

  func testFunctionCallExprNewlineBeforeLParen() throws {
    let input: SourceFile = "foo \n (42, label: true)"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(NameExpr.self))
  }

  func testFunctionCallExprNewlineAfterLParen() throws {
    let input: SourceFile = "foo ( \n 42, label: true)"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(FunctionCallExpr.self))
  }

  func testSubscriptCallExprWithoutArguments() throws {
    let input: SourceFile = "foo[]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SubscriptCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testSubscriptCallExpr() throws {
    let input: SourceFile = "foo[42, label: true]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SubscriptCallExpr)
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "label")
    }
  }

  // MARK: Primary expressions

  func testTrueLiteral() throws {
    let input: SourceFile = "true"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BooleanLiteralExpr)
    XCTAssertEqual(expr.value, true)
  }

  func testFalseLiteral() throws {
    let input: SourceFile = "false"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BooleanLiteralExpr)
    XCTAssertEqual(expr.value, false)
  }

  func testDecimalLiteral() throws {
    let input: SourceFile = "4_2_"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "42")
  }

  func testBinaryLiteral() throws {
    let input: SourceFile = "0b10_10_10"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0b101010")
  }

  func testOctalLiteral() throws {
    let input: SourceFile = "0o5_2_"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0o52")
  }

  func testHexadecimalLiteral() throws {
    let input: SourceFile = "0x2_a_"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0x2a")
  }

  func testFloatingPointLiteral() throws {
    let literals: [(String, String)] = [
      ("3.14159", "3.14159"),
      ("0.0", "0.0"),
      ("001.00", "001.00"),
      ("0.1_2__34__", "0.1234"),
      ("1e1_000", "1e1000"),
      ("1.12e+123", "1.12e+123"),
      ("1.12e+123_456", "1.12e+123456"),
      ("3.45E-6", "3.45E-6"),
      ("1.1e2", "1.1e2"),
      ("1.1e+2", "1.1e+2"),
      ("1.1e-2", "1.1e-2"),
      ("1e2", "1e2"),
      ("1e+2", "1e+2"),
      ("1e-2", "1e-2"),
    ]

    for (lit, expected) in literals {
      let input = SourceFile(stringLiteral: lit)
      let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
      let expr = try XCTUnwrap(ast[exprID] as? FloatLiteralExpr)
      XCTAssertEqual(expr.value, expected)
    }
  }

  func testStringLiteral() throws {
    let input: SourceFile = #""Hylo""#
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? StringLiteralExpr)
    XCTAssertEqual(expr.value, "Hylo")
  }

  func testPragmaLiteralExpr() throws {
    let input: SourceFile = "#file"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? PragmaLiteralExpr)
    XCTAssertEqual(expr.kind, .file)
  }

  func testSpawnExprInline() throws {
    let input: SourceFile = "spawn foo"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(SpawnExpr.self))
  }

  func testSpawnExprInlineWithCaptureList() throws {
    let input: SourceFile = "spawn[let x = a] foo"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SpawnExpr)
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
  }

  func testSpawnExprInlineWithEffect() throws {
    let input: SourceFile = "spawn[var x = a] inout foo"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SpawnExpr)
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
    XCTAssertNotNil(ast[expr.decl].receiverEffect)
  }

  func testSpawnExprBlock() throws {
    let input: SourceFile = "spawn -> T { return foo }"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(SpawnExpr.self))
  }

  func testRemoteTypeExpr() throws {
    let input: SourceFile = "remote let T"
    let (e, ast) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(e?.kind, .init(RemoteTypeExpr.self))
    let syntax = try XCTUnwrap(ast[e] as? RemoteTypeExpr)
    XCTAssertEqual(syntax.convention.value, .let)
  }

  func testCaptureExpr() throws {
    let input: SourceFile = "[let x.y]"
    let (e, ast) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(e?.kind, .init(CaptureExpr.self))
    let syntax = try XCTUnwrap(ast[e] as? CaptureExpr)
    XCTAssertEqual(syntax.access.value, .let)
  }

  func testBufferLiteral() throws {
    let input: SourceFile = "[]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssert(expr.elements.isEmpty)
  }

  func testBufferLiteralWithOneElement() throws {
    let input: SourceFile = "[a]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 1)
  }

  func testBufferLiteralWithMultipleElements() throws {
    let input: SourceFile = "[a, b, c]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testBufferLiteralWithTrailingComma() throws {
    let input: SourceFile = "[a, b, c,]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testMapLiteral() throws {
    let input: SourceFile = "[:]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssert(expr.elements.isEmpty)
  }

  func testMapLiteralWithMultipleElements() throws {
    let input: SourceFile = "[a: 0, b: 1, c: 2]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testMapLiteralWithTrailingComma() throws {
    let input: SourceFile = "[a: 0, b: 1, c: 2,]"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testPrimaryDeclRefSansHint() throws {
    let input: SourceFile = "foo<T, size: 42>"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "foo")
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "size")
    }
  }

  func testImplicitMemberRef() throws {
    let input: SourceFile = ".foo"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "foo")
    XCTAssertEqual(expr.domain, .implicit)
  }

  func testLambdaExpr() throws {
    let input: SourceFile = "fun (x) { x.foo() }"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaExpr)
    XCTAssertTrue(ast[expr.decl].isInExprContext)
  }

  func testMatchExpr() throws {
    let input: SourceFile = """
      match foo {
        let (x, y) where x == y { 0 }
        _ { 1 }
      }
      """
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MatchExpr)
    XCTAssertEqual(expr.cases.count, 2)
  }

  func testParenthesizedExpr() throws {
    let input: SourceFile = "(42)"
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(IntegerLiteralExpr.self))
  }

  func testTupleExpr() throws {
    let input: SourceFile = "()"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleExprWithOneElement() throws {
    let input: SourceFile = "(42,)"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleExprWithMultipleElements() throws {
    let input: SourceFile = "((n, m), number: 42)"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testTupleTypeExpr() throws {
    let input: SourceFile = "{}"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleTypeExprWithOneElement() throws {
    let input: SourceFile = "{ T }"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleTypeExprWithMultipleElements() throws {
    let input: SourceFile = "{ { T, U }, number: V }"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testArrowOrParenthesizedTypeExpr() throws {
    let input: SourceFile = "((A) -> (B)) -> C"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ArrowTypeExpr)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testArrowTypeExpr() throws {
    let input: SourceFile = "[{ A, B }] (T, by: U) inout -> T"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ArrowTypeExpr)
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.environment?.kind, .init(TupleTypeExpr.self))
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testTypeErasedArrowTypeExpr() throws {
    let input: SourceFile = "(T, by: U) inout -> T"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ArrowTypeExpr)
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
    XCTAssertNil(expr.environment)
  }

  func testThinArrowTypeExpr() throws {
    let input: SourceFile = "[] () -> Int"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ArrowTypeExpr)
    XCTAssertNil(expr.receiverEffect)
    XCTAssertEqual(expr.environment?.kind, .init(TupleTypeExpr.self))
    XCTAssert(expr.parameters.isEmpty)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testExistentialTypeExpr() throws {
    let input: SourceFile = "any T & U where T.Key == U.Value"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ExistentialTypeExpr)
    XCTAssertEqual(expr.traits.count, 2)
    XCTAssertEqual(expr.whereClause?.value.constraints.count, 1)
  }

  func testParameterTypeExpr() throws {
    let input: SourceFile = "sink T"
    let (e, ast) = try input.parse(with: Parser.parseParameterTypeExpr(in:))
    let tree = try XCTUnwrap(ast[e])
    XCTAssertEqual(tree.convention.value, .sink)
  }

  func testImplicitLetParameterTypeExpr() throws {
    let input: SourceFile = "T"
    let (e, ast) = try input.parse(with: Parser.parseParameterTypeExpr(in:))
    let tree = try XCTUnwrap(ast[e])
    XCTAssertEqual(tree.convention.value, .let)
  }

  func testStaticArgumentList() throws {
    let input: SourceFile = "foo<T, size: 40 + two()>"
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.arguments.count, 2)
  }

  func testWhereClause() throws {
    let input: SourceFile = "where T == B, U : A"
    let clause = try XCTUnwrap(try apply(Parser.whereClause, on: input).element)
    XCTAssertEqual(clause.value.constraints.count, 2)
  }

  func testWhereClauseEqualityConstraint() throws {
    let input: SourceFile = "T == { U, V }"
    let constraint = try XCTUnwrap(try apply(Parser.typeConstraint, on: input).element)
    if case .equality(let lhs, let rhs) = constraint.value {
      XCTAssertEqual(lhs.kind, .init(NameExpr.self))
      XCTAssertEqual(rhs.kind, .init(TupleTypeExpr.self))
    } else {
      XCTFail()
    }
  }

  func testWhereClauseConformanceConstraint() throws {
    let input: SourceFile = "T : U & V"
    let constraint = try XCTUnwrap(try apply(Parser.typeConstraint, on: input).element)
    if case .bound(let lhs, _) = constraint.value {
      XCTAssertEqual(lhs.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testWhereClauseValueConstraint() throws {
    let input: SourceFile = "@value x > 2"
    let constraint = try XCTUnwrap(try apply(Parser.valueConstraint, on: input).element)
    if case .value(let exprID) = constraint.value {
      XCTAssertEqual(exprID.kind, .init(SequenceExpr.self))
    } else {
      XCTFail()
    }
  }

  func testBoundComposition() throws {
    let input: SourceFile = "T & U & V"
    let list = try XCTUnwrap(try apply(Parser.boundComposition, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Patterns

  func testBindingPattern() throws {
    let input: SourceFile = "let (first: foo, second: (bar, _))"
    let (p, ast) = try input.parse(with: Parser.parseBindingPattern(in:))
    let pattern = try XCTUnwrap(ast[p])
    XCTAssertEqual(pattern.introducer.value, .let)

    let names = ast.names(in: p!)
    XCTAssertEqual(names.count, 2)
    if names.count == 2 {
      XCTAssertEqual(names[0].subfield, [0])
      XCTAssertEqual(ast[ast[names[0].pattern].decl].baseName, "foo")
      XCTAssertEqual(names[1].subfield, [1, 0])
      XCTAssertEqual(ast[ast[names[1].pattern].decl].baseName, "bar")
    }
  }

  func testLetSugarBindingPattern() throws {
    let input: SourceFile = "_ = foo"
    let (p, ast) = try input.parse(with: Parser.parseBindingPattern(in:))
    let pattern = try XCTUnwrap(ast[p])
    XCTAssertEqual(pattern.introducer.value, .let)
  }

  func testBindingPatternWithAnnotation() throws {
    let input: SourceFile = "inout x: T)"
    let (p, ast) = try input.parse(with: Parser.parseBindingPattern(in:))
    let pattern = try XCTUnwrap(ast[p])
    XCTAssertNotNil(pattern.annotation)
  }

  func testExprPattern() throws {
    let input: SourceFile = "foo"
    let (patternID, ast) = try apply(Parser.exprPattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID] as? ExprPattern)
    XCTAssertEqual(pattern.expr.kind, .init(NameExpr.self))
  }

  func testNamePattern() throws {
    let input: SourceFile = "foo"
    let (patternID, ast) = try apply(Parser.namePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(ast[pattern.decl].baseName, "foo")
  }

  func testOptionPattern() throws {
    let input: SourceFile = "let foo?"
    let (p, ast) = try input.parse(with: Parser.parseBindingPattern(in:))
    let b = try XCTUnwrap(p)
    let o = try XCTUnwrap(OptionPattern.ID(ast[b].subpattern))
    XCTAssertEqual(ast[ast[ast[o].name].decl].baseName, "foo")
  }

  func testTuplePattern() throws {
    let input: SourceFile = "()"
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 0)
  }

  func testTuplePatternWithOneElement() throws {
    let input: SourceFile = "(_)"
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 1)

    if pattern.elements.count == 1 {
      XCTAssertNil(pattern.elements[0].label)
    }
  }

  func testTuplePatternWithMultipleElements() throws {
    let input: SourceFile = "((n, m), number: 42)"
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 2)
  }

  func testWildcardPattern() throws {
    let input: SourceFile = "_"
    XCTAssertNotNil(try apply(Parser.wildcardPattern, on: input))
  }

  // MARK: Statements

  func testAssignStmt() throws {
    let input: SourceFile = "foo = bar"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    XCTAssert(ast[stmtID] is AssignStmt)
  }

  func testBraceStmtEmpty() throws {
    let input: SourceFile = "{}"
    XCTAssertNotNil(try apply(Parser.braceStmt, on: input).element)
  }

  func testBraceStmtWithSemicolons() throws {
    let input: SourceFile = "{; ;;}"
    let (stmtID, ast) = try apply(Parser.braceStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.stmts.count, 0)
  }

  func testBraceStmtWithElements() throws {
    let input: SourceFile = """
      {
        var x = 0; var y = 1
        print(x + y)
      }
      """
    let (stmtID, ast) = try apply(Parser.braceStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.stmts.count, 3)
  }

  func testDiscardStmt() throws {
    let input: SourceFile = "_ = foo()"
    XCTAssertNotNil(try apply(Parser.discardStmt, on: input).element)
  }

  func testDoWhileStmt() throws {
    let input: SourceFile = "do {} while true"
    XCTAssertNotNil(try apply(Parser.doWhileStmt, on: input).element)
  }

  func testWhileStmt() throws {
    let input: SourceFile = "while true {}"
    let (stmtID, ast) = try apply(Parser.whileStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.condition.count, 1)
  }

  func testWhileStmtWithMultipleConditions() throws {
    let input: SourceFile = "while let x = foo(), x > 0 {}"
    let (stmtID, ast) = try apply(Parser.whileStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.condition.count, 2)
  }

  func testForStmt() throws {
    let input: SourceFile = "for let x in array {}"
    XCTAssertNotNil(try apply(Parser.forStmt, on: input))
  }

  func testForStmtWithFilter() throws {
    let input: SourceFile = "for let x in array where x > 2 {}"
    let (stmtID, ast) = try apply(Parser.forStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertNotNil(stmt.filter)
  }

  func testReturnStmt() throws {
    let input: SourceFile = "return"
    XCTAssertNotNil(try apply(Parser.returnStmt, on: input, context: .functionBody))
  }

  func testReturnStmtWithValue() throws {
    let input: SourceFile = "return 42"
    let (stmtID, ast) = try apply(Parser.returnStmt, on: input, context: .functionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertNotNil(stmt.value)
  }

  func testYieldStmt() throws {
    let input: SourceFile = "yield &foo.bar"
    XCTAssertNotNil(try apply(Parser.yieldStmt, on: input, context: .subscriptBody))
  }

  func testBreak() throws {
    let input: SourceFile = "break"
    XCTAssertNotNil(try apply(Parser.breakStmt, on: input, context: .loopBody))
  }

  func testContinue() throws {
    let input: SourceFile = "continue"
    XCTAssertNotNil(try apply(Parser.continueStmt, on: input, context: .loopBody))
  }

  func testConditionalBinding() throws {
    let input: SourceFile = "var x = foo() else { return }"
    let (s, a) = try apply(Parser.bindingStmt, on: input, context: .functionBody)
    let n = try XCTUnwrap(a[s.flatMap(ConditionalBindingStmt.ID.init(_:))])
    let m = try XCTUnwrap(a[n.fallback].stmts.first)
    XCTAssert(m.kind == ReturnStmt.self)
  }

  func testDeclStmt() throws {
    let input: SourceFile = "typealias X = A"
    XCTAssertNotNil(try apply(Parser.declStmt, on: input))
  }

  func testExprStmt() throws {
    let input: SourceFile = "foo()"
    XCTAssertNotNil(try apply(Parser.exprStmt, on: input))
  }

  func testSimpleConditionalControl() throws {
    let input: SourceFile = "#if os(macOs) foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .operatingSystem("macOs"))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlTrue() throws {
    let input: SourceFile = "#if true foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .`true`)
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 1)
  }

  func testConditionalControlFalse() throws {
    let input: SourceFile = "#if false awgr() #else foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .`false`)
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 1)
  }

  func testConditionalControlOs() throws {
    let input: SourceFile =
      "#if os(macOs) foo() #elseif os(Linux) bar() #elseif os(Windows) bazz() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .operatingSystem("macOs"))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 1)
    
    let stmt2 = try XCTUnwrap(ast[stmt.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt2.condition, .operatingSystem("Linux"))
    XCTAssertEqual(stmt2.stmts.count, 1)
    XCTAssertEqual(stmt2.fallback.count, 1)
    
    let stmt3 = try XCTUnwrap(ast[stmt2.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt3.condition, .operatingSystem("Windows"))
    XCTAssertEqual(stmt3.stmts.count, 1)
    XCTAssertEqual(stmt3.fallback.count, 1)
  }

  func testConditionalControlArch() throws {
    let input: SourceFile =
      "#if arch(x86_64) foo() #elseif arch(i386) bar() #elseif arch(arm64) bazz() #elseif arch(arm) fizz() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .architecture("x86_64"))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 1)
    
    let stmt2 = try XCTUnwrap(ast[stmt.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt2.condition, .architecture("i386"))
    XCTAssertEqual(stmt2.stmts.count, 1)
    XCTAssertEqual(stmt2.fallback.count, 1)
    
    let stmt3 = try XCTUnwrap(ast[stmt2.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt3.condition, .architecture("arm64"))
    XCTAssertEqual(stmt3.stmts.count, 1)
    XCTAssertEqual(stmt3.fallback.count, 1)
    
    let stmt4 = try XCTUnwrap(ast[stmt3.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt4.condition, .architecture("arm"))
    XCTAssertEqual(stmt4.stmts.count, 1)
    XCTAssertEqual(stmt4.fallback.count, 1)
  }

  func testConditionalControlFeature() throws {
    let input: SourceFile =
      "#if feature(useLibC) foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .feature("useLibC"))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlCompiler() throws {
    let input: SourceFile = "#if compiler(hc) foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.condition, .compiler("hc"))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // Body not parsed
  }

  func testConditionalControlCompilerVersionGreater() throws {
    let input: SourceFile = "#if compiler_version(>= 0.1) foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .compilerVersion(
        comparison: .greaterOrEqual(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // Body not parsed
  }

  func testConditionalControlCompilerVersionLess() throws {
    let input: SourceFile = "#if compiler_version(< 100.1.2) foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .compilerVersion(
        comparison: .less(SemanticVersion(major: 100, minor: 1, patch: 2))))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // Body not parsed
  }

  func testConditionalControlHyloVersionGreater() throws {
    let input: SourceFile = "#if hylo_version(>= 0.1) foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .greaterOrEqual(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // Body not parsed
  }

  func testConditionalControlHyloVersionLess() throws {
    let input: SourceFile = "#if hylo_version(< 100.1.2) foo() #else awgr() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(
        comparison: .less(SemanticVersion(major: 100, minor: 1, patch: 2))))
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // Body not parsed
  }

  func testConditionalControlParsingInsideDisabledBlocks() throws {
    let input: SourceFile = "#if false < parse error > #endif"
    do {
      let _ = try apply(Parser.stmt, on: input)
      XCTFail("parse error expected")
    } catch {
      // all good
    }
  }

  func testConditionalControlParsingInsideVersionBlocks() throws {
    let input: SourceFile = "#if hylo_version(< 0.1) <don't show parse error here> #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .less(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 0)  // Body not parsed
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlParsingSkipsParsingOverNestedBlocks() throws {
    let input: SourceFile =
      "#if hylo_version(< 0.1) <don't show parse error here> #if hylo_version(< 0.1) <don't show parse error here> #endif #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .less(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 0)  // Body not parsed
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlParsingSkipsParsingOverNestedBlocks2() throws {
    let input: SourceFile =
      "#if hylo_version(< 0.1) <don't show parse error here> #if hylo_version(< 0.1) <don't show parse error here> #endif #elseif os(bla) #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .less(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 0)  // Body not parsed
    XCTAssertEqual(stmt.fallback.count, 1)
    
    let stmt2 = try XCTUnwrap(ast[stmt.fallback[0]] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt2.condition, .operatingSystem("bla"))
    XCTAssertEqual(stmt2.stmts.count, 0)
    XCTAssertEqual(stmt2.fallback.count, 0)
  }

  func testConditionalControlParsingSkipsParsingOverNestedBlocksInElse() throws {
    let input: SourceFile =
      "#if hylo_version(>= 0.1) #else <don't show parse error here> #if hylo_version(< 0.1) <don't show parse error here> #endif #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .greaterOrEqual(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 0)  // Body not parsed
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlParsingSkipsParsingOverNestedBlocksInElse2() throws {
    let input: SourceFile =
      "#if hylo_version(>= 0.1) #elseif compiler_version(>= 0.1) <don't show parse error here> #if hylo_version(< 0.1) <don't show parse error here> #endif #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .hyloVersion(comparison: .greaterOrEqual(SemanticVersion(major: 0, minor: 1, patch: 0))))
    XCTAssertEqual(stmt.stmts.count, 0)
    XCTAssertEqual(stmt.fallback.count, 0)
  }

  func testConditionalControlChecksParsing() throws {
    let input: SourceFile = "#if os(abracadabra) <expecting error here> #endif"
    do {
      let _ = try apply(Parser.stmt, on: input)
      XCTFail("parse error expected")
    } catch {
      // all good
    }
  }

  func testConditionalControlNotOperatorOnFalse() throws {
    let input: SourceFile = "#if !os(abracadabra) foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    // We should expand to the body of the #if
    XCTAssertEqual(stmt.expansion(for: ConditionalCompilationFactors()).count, 1)
  }

  func testConditionalControlNotOperatorOnTrue() throws {
    let input: SourceFile = "#if !true foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    // We should expand to nothing.
    XCTAssertEqual(stmt.expansion(for: ConditionalCompilationFactors()).count, 0)
  }

  func testConditionalControlNotNot() throws {
    let input: SourceFile = "#if ! !true foo() #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    // We should expand to the body.
    XCTAssertEqual(stmt.expansion(for: ConditionalCompilationFactors()).count, 1)
  }

  func testConditionalControlSkipParsingAfterNot() throws {
    let input: SourceFile = "#if !compiler_version(< 0.1) foo() #else <won't parse> #endif"
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(stmt.stmts.count, 1)
    XCTAssertEqual(stmt.fallback.count, 0)  // don't parse the #else part
  }

  func testConditionalControlInfix() throws {
    let input = SourceFile.diagnosableLiteral(
      """
      #if os(macOS) || os(Linux) && hylo_version(< 1.0.0) || os(Windows)
      do_something()
      #endif
      """)
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID] as? ConditionalCompilationStmt)
    XCTAssertEqual(
      stmt.condition,
      .or(
        .or(
          .operatingSystem("macOS"),
          .and(
            .operatingSystem("Linux"),
            .hyloVersion(comparison: .less(SemanticVersion(major: 1, minor: 0, patch: 0))))),
        .operatingSystem("Windows")))
  }

  // MARK: Operators

  func testTakeOperator() throws {
    let input: SourceFile = "+ & == | < <= > >="
    var context = ParserState(ast: AST(), space: 0, lexer: Lexer(tokenizing: input))
    XCTAssertEqual(context.takeOperator()?.value, "+")
    XCTAssertEqual(context.takeOperator()?.value, "&")
    XCTAssertEqual(context.takeOperator()?.value, "==")
    XCTAssertEqual(context.takeOperator()?.value, "|")
    XCTAssertEqual(context.takeOperator()?.value, "<")
    XCTAssertEqual(context.takeOperator()?.value, "<=")
    XCTAssertEqual(context.takeOperator()?.value, ">")
    XCTAssertEqual(context.takeOperator()?.value, ">=")
  }

  // MARK: Attributes

  func testAttribute() throws {
    let input: SourceFile = "@attr"
    let attribute = try XCTUnwrap(input.parse(with: Parser.parseAttribute).element)
    XCTAssertEqual(attribute.value.name.value, "@attr")
    XCTAssertEqual(attribute.value.arguments.count, 0)
  }

  func testAttributeWithArguments() throws {
    let input: SourceFile = #"@attr(8, "Hylo")"#
    let attribute = try XCTUnwrap(input.parse(with: Parser.parseAttribute).element)
    XCTAssertEqual(attribute.value.name.value, "@attr")
    XCTAssertEqual(attribute.value.arguments.count, 2)
  }

  func testAttributeWithExpressionArguments() throws {
    let input: SourceFile = #"@attr(MemoryLayout<Int64>.alignment())"#
    let attribute = try XCTUnwrap(input.parse(with: Parser.parseAttribute).element)
    XCTAssertEqual(attribute.value.name.value, "@attr")
    XCTAssertEqual(attribute.value.arguments.count, 1)
  }

  // MARK: Helpers

  /// Applies `combinator` on `input`, optionally setting `context` in the parser state.
  func apply<C: Combinator>(
    _ combinator: C,
    on input: SourceFile,
    context: ParserState.Context? = nil
  ) throws -> (element: C.Element?, ast: AST) where C.Context == ParserState {
    try input.parse(inContext: context, with: combinator.parse(_:))
  }

}

extension SourceFile {

  /// Parses `self` with `parser`, optionally setting `context` in the parser state.
  fileprivate func parse<Element>(
    inContext context: ParserState.Context? = nil,
    with parser: (inout ParserState) throws -> Element
  ) rethrows -> (element: Element, ast: AST) {
    var tree = AST()
    let k = tree.createNodeSpace()
    var s = ParserState(ast: tree, space: k, lexer: Lexer(tokenizing: self))
    if let c = context {
      s.contexts.append(c)
    }
    let element = try parser(&s)
    return (element, s.ast)
  }

  /// Parses `self` with `parser`, optionally setting `context` in the parser state.
  fileprivate func parseWithDeclPrologue<Element>(
    inContext context: ParserState.Context? = nil,
    with parser: (DeclPrologue, inout ParserState) throws -> Element?
  ) rethrows -> (element: Element?, ast: AST) {
    try parse(
      inContext: context,
      with: { (state) in
        try Parser.parseDeclPrologue(in: &state, then: parser)
      })
  }

}
