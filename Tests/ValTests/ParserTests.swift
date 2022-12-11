import Durian
import XCTest

@testable import Compiler

final class ParserTests: XCTestCase {

  func testParser() throws {
    // Locate the test cases.
    let testCaseDirectory = try XCTUnwrap(
      Bundle.module.url(forResource: "TestCases/Parsing", withExtension: nil), "No test cases")

    // Execute the test cases.
    try TestCase.executeAll(
      in: testCaseDirectory,
      { (tc) in
        // Parse the input.
        var ast = AST()
        let module = try ast.insert(wellFormed: ModuleDecl(name: tc.name))

        let diagnostics: [Diagnostic]
        do {
          diagnostics = try Parser.parse(tc.source, into: module, in: &ast).diagnostics
        } catch let error as DiagnosedError {
          diagnostics = error.diagnostics
        }

        // Process the test annotations.
        var diagnosticChecker = DiagnosticChecker(testCaseName: tc.name, diagnostics: diagnostics)
        for annotation in tc.annotations {
          switch annotation.command {
          case "diagnostic":
            diagnosticChecker.handle(annotation)
          default:
            XCTFail("\(tc.name): unexpected test command: '\(annotation.command)'")
          }
        }

        diagnosticChecker.finalize()
      })
  }

  // MARK: Unit tests

  func testParse() throws {
    let input = SourceFile(
      contents: """
        public fun main() {
          print("Hello, World!")
        }
        """)

    var program = AST()
    let module = try program.insert(wellFormed: ModuleDecl(name: "Main"))

    let (decls, diagnostics) = try Parser.parse(input, into: module, in: &program)
    XCTAssertNotNil(decls)
    XCTAssertEqual(diagnostics.count, 0)
  }

  func testSourceFile() throws {
    let input = SourceFile(
      contents: """
          ;;
          import Foo

          @_lowered_name("_val_bar")
          fun _bar(x: Builtin.i64) -> Builtin.i64

          let x = "Hello!"
          public let y = 0;
        """)

    let (id, ast) = try input.parse(with: Parser.parseSourceFile)
    XCTAssertEqual(ast[id].decls.count, 4)
  }

  // MARK: Declarations

  func testModuleMember() throws {
    let input = SourceFile(contents: "public operator infix| : disjunction")
    let (declID, ast) = try input.parse(with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? OperatorDecl)
    XCTAssertEqual(decl.name.value, "|")
    XCTAssertEqual(decl.precedenceGroup?.value, .disjunction)
  }

  func testImportDecl() throws {
    let input = SourceFile(contents: "import Foo")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseImportDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.name, "Foo")
  }

  func testNamespaceDecl() throws {
    let input = SourceFile(
      contents: """
        namespace A {
          ;;
          let x = "Hello!"
          public let y = 0;
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseNamespaceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testNamespaceMember() throws {
    let input = SourceFile(contents: "fun foo() {}")
    let (declID, _) = try input.parse(inContext: .namespaceBody, with: Parser.parseDecl)
    XCTAssertNotNil(declID)
  }

  func testNamespaceMemberPublic() throws {
    let input = SourceFile(contents: "public fun foo() {}")
    let (declID, ast) = try input.parse(inContext: .namespaceBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
  }

  func testTypeAliasDecl() throws {
    let input = SourceFile(contents: "typealias A = B")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTypeAliasDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "A")
  }

  func testTypeAliasDeclWithGenericClause() throws {
    let input = SourceFile(contents: "typealias A<T> = B<T>")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTypeAliasDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
  }

  func testProductTypeDecl() throws {
    let input = SourceFile(
      contents: """
        type A {
          var x: Int; var y: Int
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 4)  // 3 explicit decls + 1 implicit memberwise init
  }

  func testProductTypeDeclWithGenericClause() throws {
    let input = SourceFile(
      contents: """
        type A<T, U> {
                var x: Int; var y: Int
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
  }

  func testProductTypeDeclWithConformances() throws {
    let input = SourceFile(
      contents: """
        type A: Foo, Bar {
          var x: Int; var y: Int
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testProductTypeDeclWithGenericClauseAndConformances() throws {
    let input = SourceFile(
      contents: """
        type A<T>: Foo {
          var x: Int
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseProductTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.conformances)
  }

  func testProductTypeMember() throws {
    let input = SourceFile(contents: "var x: Int")
    let (declID, _) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    XCTAssertNotNil(declID)
  }

  func testProductTypeMemberPublic() throws {
    let input = SourceFile(contents: "public var x: Int")
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
  }

  func testProductTypeMemberStatic() throws {
    let input = SourceFile(contents: "static var x: Int")
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testProductTypeMemberPublicStatic() throws {
    let input = SourceFile(contents: "public static var x: Int")
    let (declID, ast) = try input.parse(inContext: .productBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testTraitDecl() throws {
    let input = SourceFile(
      contents: """
        trait A {
          type B
          property b: B { let }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTraitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testTraitDeclWithRefinements() throws {
    let input = SourceFile(
      contents: """
        trait A: Foo {
          type B
          property b: B { let }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseTraitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.refinements.count, 1)
  }

  func testMethodBundleRequirement() throws {
    let input = SourceFile(
      contents: """
        fun foo() -> T {
          let
          inout
        }
        """)
    let (declID, ast) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? MethodDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testSubscriptRequirement() throws {
    let input = SourceFile(
      contents: """
        subscript foo(): T {
          let
          inout
        }
        """)
    let (declID, ast) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? SubscriptDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testPropertyRequirement() throws {
    let input = SourceFile(contents: "property foo: T { let }")
    let (declID, _) = try input.parse(inContext: .traitBody, with: Parser.parseDecl)
    XCTAssertNotNil(declID)
  }

  func testAssociatedTypeDecl() throws {
    let input = SourceFile(contents: "type Foo")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "Foo")
  }

  func testAssociatedTypeDeclWithConformances() throws {
    let input = SourceFile(contents: "type Foo: Bar, Ham")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testAssociatedTypeDeclWithWhereClause() throws {
    let input = SourceFile(contents: "type Foo where Foo.Bar == Ham")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testAssociatedTypeDeclWithWithDefault() throws {
    let input = SourceFile(contents: "type Foo = X")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedTypeDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testAssociatedValueDecl() throws {
    let input = SourceFile(contents: "value foo")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "foo")
  }

  func testAssociatedValueDeclWithWhereClause() throws {
    let input = SourceFile(contents: "value foo where @value foo > bar")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  /*
  func testAssociatedValueDeclWithWhereClauseSansHint() throws {
    let input = SourceFile(contents: "value foo where foo > bar")
    let (declID, ast) = try input.parseWithDeclPrologue(       inContext: .traitBody,       with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }
   */

  func testAssociatedValueDeclWithDefault() throws {
    let input = SourceFile(contents: "value foo = 42")
    let (declID, ast) = try input.parseWithDeclPrologue(
      inContext: .traitBody, with: Parser.parseAssociatedValueDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testConformanceDecl() throws {
    let input = SourceFile(
      contents: """
        conformance A: Foo {
          public fun bar() {}
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseConformanceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testConformanceDeclWithWhereClause() throws {
    let input = SourceFile(
      contents: """
        conformance A: Foo where A.Bar == Ham {
          public fun bar() {}
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseConformanceDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionDecl() throws {
    let input = SourceFile(
      contents: """
        extension A {
          type B {}; property z: Int { x }
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseExtensionDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)
  }

  func testExtensionDeclWithWhereClause() throws {
    let input = SourceFile(
      contents: """
        extension A where Foo: Bar {
          type B {}; property z: Int { x }
          fun foo() -> Int { x.copy() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseExtensionDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionMember() throws {
    let input = SourceFile(contents: "public static fun forty_two() -> Int { 42 }")
    let (declID, ast) = try input.parse(inContext: .extensionBody, with: Parser.parseDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testBindingDecl() throws {
    let input = SourceFile(contents: "let (foo, bar)")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseBindingDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(ast[decl.pattern].introducer.value, .let)
  }

  func testBindingDeclWithInitializer() throws {
    let input = SourceFile(contents: "let (foo, bar) = (true, ham())")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseBindingDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.initializer)
  }

  func testMemberwiseInitDecl() throws {
    let input = SourceFile(contents: "memberwise init")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseMemberwiseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .memberwiseInit)
  }

  func testInitDecl() throws {
    let input = SourceFile(contents: "init() {}")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.body)
  }

  func testInitDeclGeneric() throws {
    let input = SourceFile(contents: "init<T>() {}")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseInitDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDecl() throws {
    let input = SourceFile(contents: "fun foo() {}")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDeclWithCaptureList() throws {
    let input = SourceFile(contents: "fun foo[let x = 42]() {}")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.explicitCaptures.count, 1)
    XCTAssertNotNil(decl.body)
  }

  func testFunctionDeclWithExprBody() throws {
    let input = SourceFile(contents: "fun id<T: Sinkable>(_ x: T) -> T { x }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testPostifxFunctionDecl() throws {
    let input = SourceFile(contents: "postfix fun +() -> T { x }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? FunctionDecl)
    XCTAssertEqual(decl.notation?.value, .postfix)
    XCTAssertNotNil(decl.body)
  }

  func testMethodBundle() throws {
    let input = SourceFile(
      contents: """
        fun foo() {
          let  { self.copy() }
          sink { self }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseFunctionOrMethodDecl)
    let decl = try XCTUnwrap(ast[declID] as? MethodDecl)
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testFunctionDeclSignature() throws {
    let input = SourceFile(contents: "()")
    let signature = try XCTUnwrap(input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 0)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithParameters() throws {
    let input = SourceFile(contents: "(_ foo: Foo, bar: Bar = .default)")
    let signature = try XCTUnwrap(input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 2)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithEffect() throws {
    let input = SourceFile(contents: "(_ foo: Foo) inout")
    let signature = try XCTUnwrap(input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .inout)
    XCTAssertNil(signature.output)
  }

  func testFunctionDeclSignatureWithOutput() throws {
    let input = SourceFile(contents: "(_ foo: Foo) -> C")
    let signature = try XCTUnwrap(input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertEqual(signature.output?.kind, NodeKind(NameExpr.self))
  }

  func testFunctionDeclSignatureWithOutputAndEffect() throws {
    let input = SourceFile(contents: "(_ foo: Foo) sink -> C")
    let signature = try XCTUnwrap(input.parse(with: Parser.parseFunctionDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .sink)
    XCTAssertEqual(signature.output?.kind, .init(NameExpr.self))
  }

  func testFunctionDeclIdentifier() throws {
    let input = SourceFile(contents: "fun foo")
    let identifier = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclIntroducerAndIdentifier(in:)).element)
    XCTAssertEqual(identifier.stem.value, "foo")
    XCTAssertNil(identifier.notation)
  }

  func testFunctionDeclOperator() throws {
    let input = SourceFile(contents: "postfix fun ++")
    let identifier = try XCTUnwrap(
      input.parse(with: Parser.parseFunctionDeclIntroducerAndIdentifier(in:)).element)
    XCTAssertEqual(identifier.stem.value, "++")
    XCTAssertEqual(identifier.notation?.value, .postfix)
  }

  func testFunctionDeclBodyBlock() throws {
    let input = SourceFile(contents: "{}")
    let (body, _) = try apply(Parser.functionDeclBody, on: input)
    if case .block = body {
    } else {
      XCTFail()
    }
  }

  func testFunctionDeclBodyExpr() throws {
    let input = SourceFile(contents: "{ 0x2a }")
    let (body, _) = try apply(Parser.functionDeclBody, on: input)
    if case .expr = body {
    } else {
      XCTFail()
    }
  }

  func testMethodDeclBody() throws {
    let input = SourceFile(
      contents: """
        {
          let  { self.copy() }
          sink { self }
        }
        """)
    let (body, _) = try apply(Parser.methodDeclBody, on: input)
    let impls = try XCTUnwrap(body)
    XCTAssertEqual(impls.count, 2)
  }

  func testMethodImplBlock() throws {
    let input = SourceFile(contents: "let { }")
    let (declID, ast) = try apply(Parser.methodImplDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .block = decl.body {
    } else {
      XCTFail()
    }
  }

  func testMethodImplExpr() throws {
    let input = SourceFile(contents: "let { foo }")
    let (declID, ast) = try apply(Parser.methodImplDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testPropertyDecl() throws {
    let input = SourceFile(contents: "property foo: T { T() }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parsePropertyDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNil(decl.parameters)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDecl() throws {
    let input = SourceFile(contents: "subscript foo(): T { T() }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNotNil(decl.parameters)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclAnonymous() throws {
    let input = SourceFile(contents: "subscript (): T { T() }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNil(decl.identifier)
    XCTAssertNotNil(decl.parameters)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithCaptureList() throws {
    let input = SourceFile(contents: "subscript foo[let x = 42](): T { T() }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.explicitCaptures.count, 1)
    XCTAssertNotNil(decl.parameters)
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithBlockBody() throws {
    let input = SourceFile(contents: "subscript foo<T: Foo>(_ x: T): T { yield x }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptDeclWithExprBody() throws {
    let input = SourceFile(contents: "subscript foo<T: Foo>(_ x: T): T { x }")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.impls.count, 1)
  }

  func testSubscriptBundle() throws {
    let input = SourceFile(
      contents: """
        subscript foo(): T {
          let  { T() }
          sink { T() }
        }
        """)
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseSubscriptDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.impls.count, 2)
  }

  func testSubscriptDeclSignature() throws {
    let input = SourceFile(contents: "(): T")
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseSubscriptDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 0)
  }

  func testSubscriptDeclSignatureWithParameters() throws {
    let input = SourceFile(contents: "(_ foo: Foo, bar: Bar = .default): T")
    let signature = try XCTUnwrap(
      input.parse(with: Parser.parseSubscriptDeclSignature(in:)).element)
    XCTAssertEqual(signature.parameters.count, 2)
  }

  func testSubscriptDeclBodyBlock() throws {
    let input = SourceFile(contents: "{ yield x }")
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
    let input = SourceFile(contents: "{ 0x2a }")
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
    let input = SourceFile(
      contents: """
        {
          let  { self.copy() }
          sink { self }
        }
        """)
    let (body, _) = try input.parse(
      inContext: .subscriptBody,
      with: { (state) in try Parser.parseSubscriptDeclBody(in: &state, asNonStaticMember: true) })

    XCTAssertEqual(body?.count, 2)
  }

  func testParameterDecl() throws {
    let input = SourceFile(contents: "_ foo")
    let (declID, ast) = try apply(Parser.parameterDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.name, "foo")
  }

  func testParameterDeclWithAnnotation() throws {
    let input = SourceFile(contents: "_ foo: T")
    let (declID, ast) = try apply(Parser.parameterDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.annotation)
  }

  func testParameterDeclWithDefault() throws {
    let input = SourceFile(contents: "_ foo: T = T()")
    let (declID, ast) = try apply(Parser.parameterDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testParameterIntefaceLabelAndName() throws {
    let input = SourceFile(contents: "for name")
    let interface = try XCTUnwrap(try apply(Parser.parameterInterface, on: input).element)
    XCTAssertEqual(interface.label?.value, "for")
    XCTAssertEqual(interface.name.value, "name")
  }

  func testParameterIntefaceUnderscoreAndName() throws {
    let input = SourceFile(contents: "_ name")
    let interface = try XCTUnwrap(try apply(Parser.parameterInterface, on: input).element)
    XCTAssertNil(interface.label)
    XCTAssertEqual(interface.name.value, "name")
  }

  func testParameterIntefaceOnlyName() throws {
    let input = SourceFile(contents: "name")
    let interface = try XCTUnwrap(try apply(Parser.parameterInterface, on: input).element)
    XCTAssertEqual(interface.label?.value, "name")
    XCTAssertEqual(interface.name.value, "name")
  }

  func testOperatorDecl() throws {
    let input = SourceFile(contents: "operator infix+")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseOperatorDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.notation.value, .infix)
    XCTAssertNil(decl.precedenceGroup)
  }

  func testOperatorDeclWithPredecenceGroup() throws {
    let input = SourceFile(contents: "operator infix+ : addition")
    let (declID, ast) = try input.parseWithDeclPrologue(with: Parser.parseOperatorDecl)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.notation.value, .infix)
    XCTAssertEqual(decl.precedenceGroup?.value, .addition)
  }

  func testGenericClause() throws {
    let input = SourceFile(contents: "<T>")
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 1)
  }

  func testGenericClauseWithMultipleParameters() throws {
    let input = SourceFile(contents: "<T, n: Int>")
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 2)
  }

  func testGenericClauseWithMultipleParametersSansHint() throws {
    let input = SourceFile(contents: "<T, n: Int>")
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertEqual(clause.value.parameters.count, 2)
  }

  func testCaptureList() throws {
    let input = SourceFile(contents: "[let x = a, var y = true]")
    let list = try XCTUnwrap(try apply(Parser.captureList, on: input).element)
    XCTAssertEqual(list.count, 2)
  }

  func testGenericClauseWithWhereClause() throws {
    let input = SourceFile(contents: "<T: Foo where T.Bar == {}>")
    let clause = try XCTUnwrap(try apply(Parser.genericClause, on: input).element)
    XCTAssertNotNil(clause.value.whereClause)
  }

  func testGenericParameter() throws {
    let input = SourceFile(contents: "T")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.name, "T")
  }

  func testGenericParameterWithConformances() throws {
    let input = SourceFile(contents: "T: Foo & Bar")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.conformances.count, 2)
  }

  func testGenericParameterWithDefault() throws {
    let input = SourceFile(contents: "T = U")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testGenericParameterWithConformancesAndDefault() throws {
    let input = SourceFile(contents: "T: Int = 0o52")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.conformances.count, 1)
    XCTAssertNotNil(decl.defaultValue)
  }

  func testConformanceList() throws {
    let input = SourceFile(contents: ": Foo, Bar, Ham")
    let list = try XCTUnwrap(try apply(Parser.conformanceList, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Value expressions

  func testExpr() throws {
    let input = SourceFile(contents: "(foo().bar[] + 42, ham++, !baz)")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(TupleExpr.self))
  }

  func testInfixExpr() throws {
    let input = SourceFile(contents: "foo == 2 & true")
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
    let input = SourceFile(contents: "foo as T")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.kind, .up)
  }

  func testCastExprDown() throws {
    let input = SourceFile(contents: "foo as! T")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.kind, .down)
  }

  func testCastExprBuiltinPointerConversion() throws {
    let input = SourceFile(contents: "foo as!! T")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.kind, .builtinPointerConversion)
  }

  func testInoutExpr() throws {
    let input = SourceFile(contents: "&foo")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(InoutExpr.self))
  }

  func testPrefixExpr() throws {
    let input = SourceFile(contents: "+foo")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let call = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .prefix)

    if case .expr(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testPostfixExpr() throws {
    let input = SourceFile(contents: "foo+")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let call = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .postfix)

    if case .expr(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  // MARK: Compound expressions

  func testLabeledMemberExpr() throws {
    let input = SourceFile(contents: "a.b.c")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    var expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "c")

    if case .expr(let domainID) = expr.domain {
      expr = try XCTUnwrap(ast[domainID] as? NameExpr)
      XCTAssertEqual(expr.name.value.stem, "b")
    } else {
      XCTFail()
    }
  }

  func testIndexedMemberExpr() throws {
    let input = SourceFile(contents: "foo.12")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleMemberExpr)
    XCTAssertEqual(expr.index, 12)

    let parentExpr = try XCTUnwrap(ast[expr.tuple] as? NameExpr)
    XCTAssertEqual(parentExpr.name.value.stem, "foo")
  }

  func testStaticValueMemberExpr() throws {
    let input = SourceFile(contents: "{ A, B }.meta")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "meta")

    if case .expr(let domainID) = expr.domain {
      XCTAssertEqual(domainID.kind, .init(TupleTypeExpr.self))
    } else {
      XCTFail()
    }
  }

  func testConformanceLensTypeExpr() throws {
    let input = SourceFile(contents: "{ T, U }::Baz")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ConformanceLensTypeExpr)
    XCTAssertEqual(expr.subject.kind, .init(TupleTypeExpr.self))
    XCTAssertEqual(expr.lens.kind, .init(NameExpr.self))
  }

  func testConformanceLensExprWithMember() throws {
    let input = SourceFile(contents: "T::P.A")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "A")

    if case .expr(let domain) = expr.domain {
      let d = try XCTUnwrap(ast[domain] as? ConformanceLensTypeExpr)
      XCTAssertEqual(d.subject.kind, .init(NameExpr.self))
      XCTAssertEqual(d.lens.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testFunctionCallExprWithoutArguments() throws {
    let input = SourceFile(contents: "foo()")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testFunctionCallExpr() throws {
    let input = SourceFile(contents: "foo(42, label: true)")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "label")
    }
  }

  func testFunctionCallExprNewlineBeforeLParen() throws {
    let input = SourceFile(contents: "foo \n (42, label: true)")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(NameExpr.self))
  }

  func testFunctionCallExprNewlineAfterLParen() throws {
    let input = SourceFile(contents: "foo ( \n 42, label: true)")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(FunCallExpr.self))
  }

  func testSubscriptCallExprWithoutArguments() throws {
    let input = SourceFile(contents: "foo[]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SubscriptCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testSubscriptCallExpr() throws {
    let input = SourceFile(contents: "foo[42, label: true]")
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
    let input = SourceFile(contents: "true")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BooleanLiteralExpr)
    XCTAssertEqual(expr.value, true)
  }

  func testFalseLiteral() throws {
    let input = SourceFile(contents: "false")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BooleanLiteralExpr)
    XCTAssertEqual(expr.value, false)
  }

  func testDecimalLiteral() throws {
    let input = SourceFile(contents: "4_2_")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "42")
  }

  func testBinaryLiteral() throws {
    let input = SourceFile(contents: "0b10_10_10")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0b101010")
  }

  func testOctalLiteral() throws {
    let input = SourceFile(contents: "0o5_2_")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0o52")
  }

  func testHexadecimalLiteral() throws {
    let input = SourceFile(contents: "0x2_a_")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? IntegerLiteralExpr)
    XCTAssertEqual(expr.value, "0x2a")
  }

  func testFloatingPointLiteral() throws {
    let input = SourceFile(contents: "4.2e+1")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? FloatLiteralExpr)
    XCTAssertEqual(expr.value, "4.2e+1")
  }

  func testStringLiteral() throws {
    let input = SourceFile(contents: #""Val""#)
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? StringLiteralExpr)
    XCTAssertEqual(expr.value, "Val")
  }

  func testNilLiteralExpr() throws {
    let input = SourceFile(contents: "nil")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(NilLiteralExpr.self))
  }

  func testSpawnExprInline() throws {
    let input = SourceFile(contents: "spawn foo")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(SpawnExpr.self))
  }

  func testSpawnExprInlineWithCaptureList() throws {
    let input = SourceFile(contents: "spawn[let x = a] foo")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SpawnExpr)
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
  }

  func testSpawnExprInlineWithEffect() throws {
    let input = SourceFile(contents: "spawn[var x = a] inout foo")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? SpawnExpr)
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
    XCTAssertNotNil(ast[expr.decl].receiverEffect)
  }

  func testSpawnExprBlock() throws {
    let input = SourceFile(contents: "spawn -> T { return foo }")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(SpawnExpr.self))
  }

  func testBufferLiteral() throws {
    let input = SourceFile(contents: "[]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssert(expr.elements.isEmpty)
  }

  func testBufferLiteralWithOneElement() throws {
    let input = SourceFile(contents: "[a]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 1)
  }

  func testBufferLiteralWithMultipleElements() throws {
    let input = SourceFile(contents: "[a, b, c]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testMapLiteral() throws {
    let input = SourceFile(contents: "[:]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssert(expr.elements.isEmpty)
  }

  func testMapLiteralWithMultipleElements() throws {
    let input = SourceFile(contents: "[a: 0, b: 1, c: 2]")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testPrimaryDeclRefSansHint() throws {
    let input = SourceFile(contents: "foo<T, size: 42>")
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
    let input = SourceFile(contents: ".foo")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "foo")
    XCTAssertEqual(expr.domain, .implicit)
  }

  func testLambdaExpr() throws {
    let input = SourceFile(contents: "fun (x) { x.foo() }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaExpr)
    XCTAssertTrue(ast[expr.decl].isInExprContext)
  }

  func testMatchExpr() throws {
    let input = SourceFile(
      contents: """
        match foo {
          let (x, y) where x == y { 0 }
          _ { 1 }
        }
        """)
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? MatchExpr)
    XCTAssertEqual(expr.cases.count, 2)
  }

  func testMatchCaseBlock() throws {
    let input = SourceFile(contents: "let (x, 0x2a) { }")
    let (caseID, ast) = try apply(Parser.matchCase, on: input)
    let case_ = try XCTUnwrap(ast[caseID])
    if case .block = case_.body {
    } else {
      XCTFail()
    }
  }

  func testMatchCaseExpr() throws {
    let input = SourceFile(contents: "let (x, 0x2a) { x }")
    let (caseID, ast) = try apply(Parser.matchCase, on: input)
    let case_ = try XCTUnwrap(ast[caseID])
    if case .expr = case_.body {
    } else {
      XCTFail()
    }
  }

  func testMatchCaseWithCondition() throws {
    let input = SourceFile(contents: "let (x, y) where x > y { }")
    let (caseID, ast) = try apply(Parser.matchCase, on: input)
    let case_ = try XCTUnwrap(ast[caseID])
    XCTAssertNotNil(case_.condition)
  }

  func testConditionalExpr() throws {
    let input = SourceFile(contents: "if true { }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)
    XCTAssertEqual(expr.condition.count, 1)
  }

  func testConditionalExprWithMultipleConditions() throws {
    let input = SourceFile(contents: "if let x = foo, x > 0 { }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)
    XCTAssertEqual(expr.condition.count, 2)
  }

  func testConditionalExprBlockThenNoElse() throws {
    let input = SourceFile(contents: "if true { }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)

    if case .block = expr.success {
    } else {
      XCTFail()
    }

    XCTAssertNil(expr.failure)
  }

  func testConditionalExprBlockThenBlockElse() throws {
    let input = SourceFile(contents: "if true { } else { }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)

    if case .block = expr.success {
    } else {
      XCTFail()
    }

    if case .block = expr.failure {
    } else {
      XCTFail()
    }
  }

  func testConditionalExprExprThenExprElse() throws {
    let input = SourceFile(contents: "if true { 1 } else { 2 }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)

    if case .expr = expr.success {
    } else {
      XCTFail()
    }

    if case .expr = expr.failure {
    } else {
      XCTFail()
    }
  }

  func testConditionalExprExprElseIfElse() throws {
    let input = SourceFile(contents: "if true { 1 } else if false { 2 } else { 3 }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? CondExpr)

    if case .expr(let elseID) = expr.failure {
      XCTAssertEqual(elseID.kind, .init(CondExpr.self))
    } else {
      XCTFail()
    }
  }

  func testParenthesizedExpr() throws {
    let input = SourceFile(contents: "(42)")
    let (exprID, _) = try input.parse(with: Parser.parseExpr(in:))
    XCTAssertEqual(exprID?.kind, .init(IntegerLiteralExpr.self))
  }

  func testTupleExpr() throws {
    let input = SourceFile(contents: "()")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleExprWithOneElement() throws {
    let input = SourceFile(contents: "(42,)")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleExprWithMultipleElements() throws {
    let input = SourceFile(contents: "((n, m), number: 42)")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleExpr)
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testTupleTypeExpr() throws {
    let input = SourceFile(contents: "{}")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleTypeExprWithOneElement() throws {
    let input = SourceFile(contents: "{ T }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleTypeExprWithMultipleElements() throws {
    let input = SourceFile(contents: "{ { T, U }, number: V }")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? TupleTypeExpr)
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testLambdaOrParenthesizedTypeExpr() throws {
    let input = SourceFile(contents: "((A) -> (B)) -> C")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaTypeExpr)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testLambdaTypeExpr() throws {
    let input = SourceFile(contents: "[{ A, B }] (T, by: U) inout -> T")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaTypeExpr)
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.environment?.kind, .init(TupleTypeExpr.self))
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testTypeErasedLambdaTypeExpr() throws {
    let input = SourceFile(contents: "(T, by: U) inout -> T")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaTypeExpr)
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
    XCTAssertNil(expr.environment)
  }

  func testThinLambdaTypeExpr() throws {
    let input = SourceFile(contents: "[] () -> Int")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? LambdaTypeExpr)
    XCTAssertNil(expr.receiverEffect)
    XCTAssertEqual(expr.environment?.kind, .init(TupleTypeExpr.self))
    XCTAssert(expr.parameters.isEmpty)
    XCTAssertEqual(expr.output.kind, .init(NameExpr.self))
  }

  func testExistentialTypeExpr() throws {
    let input = SourceFile(contents: "any T & U where T.Key == U.Value")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? ExistentialTypeExpr)
    XCTAssertEqual(expr.traits.count, 2)
    XCTAssertEqual(expr.whereClause?.value.constraints.count, 1)
  }

  func testParameterTypeExpr() throws {
    let input = SourceFile(contents: "sink T")
    let (exprID, ast) = try apply(Parser.parameterTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.convention.value, .sink)
  }

  func testImplicitLetParameterTypeExpr() throws {
    let input = SourceFile(contents: "T")
    let (exprID, ast) = try apply(Parser.parameterTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.convention.value, .let)
  }

  func testStaticArgumentList() throws {
    let input = SourceFile(contents: "foo<T, size: 40 + two()>")
    let (exprID, ast) = try input.parse(with: Parser.parseExpr(in:))
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.arguments.count, 2)
  }

  func testWhereClause() throws {
    let input = SourceFile(contents: "where T == B, U : A")
    let clause = try XCTUnwrap(try apply(Parser.whereClause, on: input).element)
    XCTAssertEqual(clause.value.constraints.count, 2)
  }

  func testWhereClauseEqualityConstraint() throws {
    let input = SourceFile(contents: "T == { U, V }")
    let constraint = try XCTUnwrap(try apply(Parser.typeConstraint, on: input).element)
    if case .equality(let lhs, let rhs) = constraint.value {
      XCTAssertEqual(lhs.kind, .init(NameExpr.self))
      XCTAssertEqual(rhs.kind, .init(TupleTypeExpr.self))
    } else {
      XCTFail()
    }
  }

  func testWhereClauseConformanceConstraint() throws {
    let input = SourceFile(contents: "T : U & V")
    let constraint = try XCTUnwrap(try apply(Parser.typeConstraint, on: input).element)
    if case .conformance(let lhs, _) = constraint.value {
      XCTAssertEqual(lhs.kind, .init(NameExpr.self))
    } else {
      XCTFail()
    }
  }

  func testWhereClauseValueConstraint() throws {
    let input = SourceFile(contents: "@value x > 2")
    let constraint = try XCTUnwrap(try apply(Parser.valueConstraint, on: input).element)
    if case .value(let exprID) = constraint.value {
      XCTAssertEqual(exprID.kind, .init(SequenceExpr.self))
    } else {
      XCTFail()
    }
  }

  /*
  func testWhereClauseValueConstraintSansHint() throws {
    let input = SourceFile(contents: "x > 2")
    let constraint = try XCTUnwrap(try apply(Parser.valueConstraint, on: input).element)
    if case .value(let exprID) = constraint.value {
      XCTAssertEqual(exprID.kind, .init(SequenceExpr.self))
    } else {
      XCTFail()
    }
  }
   */

  func testTraitComposition() throws {
    let input = SourceFile(contents: "T & U & V")
    let list = try XCTUnwrap(try apply(Parser.traitComposition, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Patterns

  func testBindingPattern() throws {
    let input = SourceFile(contents: "let (first: foo, second: (bar, _))")
    let (patternID, ast) = try apply(Parser.bindingPattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.introducer.value, .let)

    let names = ast.names(in: patternID!)
    XCTAssertEqual(names.count, 2)
    if names.count == 2 {
      XCTAssertEqual(names[0].path, [0])
      XCTAssertEqual(ast[ast[names[0].pattern].decl].name, "foo")
      XCTAssertEqual(names[1].path, [1, 0])
      XCTAssertEqual(ast[ast[names[1].pattern].decl].name, "bar")
    }
  }

  func testBindingPatternWithAnnotation() throws {
    let input = SourceFile(contents: "inout x: T)")
    let (patternID, ast) = try apply(Parser.bindingPattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertNotNil(pattern.annotation)
  }

  func testExprPattern() throws {
    let input = SourceFile(contents: "foo")
    let (patternID, ast) = try apply(Parser.exprPattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID] as? ExprPattern)
    XCTAssertEqual(pattern.expr.kind, .init(NameExpr.self))
  }

  func testNamePattern() throws {
    let input = SourceFile(contents: "foo")
    let (patternID, ast) = try apply(Parser.namePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(ast[pattern.decl].name, "foo")
  }

  func testTuplePattern() throws {
    let input = SourceFile(contents: "()")
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 0)
  }

  func testTuplePatternWithOneElement() throws {
    let input = SourceFile(contents: "(_)")
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 1)

    if pattern.elements.count == 1 {
      XCTAssertNil(pattern.elements[0].label)
    }
  }

  func testTuplePatternWithMultipleElements() throws {
    let input = SourceFile(contents: "((n, m), number: 42)")
    let (patternID, ast) = try apply(Parser.tuplePattern, on: input)
    let pattern = try XCTUnwrap(ast[patternID])
    XCTAssertEqual(pattern.elements.count, 2)
  }

  func testWildcardPattern() throws {
    let input = SourceFile(contents: "_")
    XCTAssertNotNil(try apply(Parser.wildcardPattern, on: input))
  }

  // MARK: Statements

  func testAssignStmt() throws {
    let input = SourceFile(contents: "foo = bar")
    let (stmtID, ast) = try apply(Parser.stmt, on: input)
    XCTAssert(ast[stmtID] is AssignStmt)
  }

  func testBraceStmtEmpty() throws {
    let input = SourceFile(contents: "{}")
    XCTAssertNotNil(try apply(Parser.braceStmt, on: input).element)
  }

  func testBraceStmtWithSemicolons() throws {
    let input = SourceFile(contents: "{; ;;}")
    let (stmtID, ast) = try apply(Parser.braceStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.stmts.count, 0)
  }

  func testBraceStmtWithElements() throws {
    let input = SourceFile(
      contents: """
        {
          var x = 0; var y = 1
          print(x + y)
        }
        """)
    let (stmtID, ast) = try apply(Parser.braceStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.stmts.count, 3)
  }

  func testDiscardStmt() throws {
    let input = SourceFile(contents: "_ = foo()")
    XCTAssertNotNil(try apply(Parser.discardStmt, on: input).element)
  }

  func testDoWhileStmt() throws {
    let input = SourceFile(contents: "do {} while true")
    XCTAssertNotNil(try apply(Parser.doWhileStmt, on: input).element)
  }

  func testWhileStmt() throws {
    let input = SourceFile(contents: "while true {}")
    let (stmtID, ast) = try apply(Parser.whileStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.condition.count, 1)
  }

  func testWhileStmtWithMultipleConditions() throws {
    let input = SourceFile(contents: "while let x = foo(), x > 0 {}")
    let (stmtID, ast) = try apply(Parser.whileStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertEqual(stmt.condition.count, 2)
  }

  func testForStmt() throws {
    let input = SourceFile(contents: "for let x in array {}")
    XCTAssertNotNil(try apply(Parser.forStmt, on: input))
  }

  func testForStmtWithFilter() throws {
    let input = SourceFile(contents: "for let x in array where x > 2 {}")
    let (stmtID, ast) = try apply(Parser.forStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertNotNil(stmt.filter)
  }

  func testReturnStmt() throws {
    let input = SourceFile(contents: "return")
    XCTAssertNotNil(try apply(Parser.returnStmt, on: input, context: .functionBody))
  }

  func testReturnStmtWithValue() throws {
    let input = SourceFile(contents: "return 42")
    let (stmtID, ast) = try apply(Parser.returnStmt, on: input, context: .functionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertNotNil(stmt.value)
  }

  func testYieldStmt() throws {
    let input = SourceFile(contents: "yield &foo.bar")
    XCTAssertNotNil(try apply(Parser.yieldStmt, on: input, context: .subscriptBody))
  }

  func testBreak() throws {
    let input = SourceFile(contents: "break")
    XCTAssertNotNil(try apply(Parser.breakStmt, on: input, context: .loopBody))
  }

  func testContinue() throws {
    let input = SourceFile(contents: "continue")
    XCTAssertNotNil(try apply(Parser.continueStmt, on: input, context: .loopBody))
  }

  func testConditionalBinding() throws {
    let input = SourceFile(contents: "var x = foo() else return")
    let (stmtID, ast) = try apply(Parser.conditionalBindingStmt, on: input, context: .functionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    if case .exit = stmt.fallback {
    } else {
      XCTFail()
    }
  }

  func testConditionalBindingBlock() throws {
    let input = SourceFile(contents: "var x = foo() else { bar(); return }")
    let (stmtID, ast) = try apply(Parser.conditionalBindingStmt, on: input, context: .functionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    if case .exit = stmt.fallback {
    } else {
      XCTFail()
    }
  }

  func testConditionalBindingExpr() throws {
    let input = SourceFile(contents: "var x = foo() else fatal_error()")
    let (stmtID, ast) = try apply(Parser.conditionalBindingStmt, on: input)
    let stmt = try XCTUnwrap(ast[stmtID])
    if case .expr = stmt.fallback {
    } else {
      XCTFail()
    }
  }

  func testConditionalBindingFallback() throws {
    let input = SourceFile(contents: "return")
    XCTAssertNotNil(try apply(Parser.conditionalBindingStmt, on: input, context: .functionBody))
  }

  func testDeclStmt() throws {
    let input = SourceFile(contents: "typealias X = A")
    XCTAssertNotNil(try apply(Parser.declStmt, on: input))
  }

  func testExprStmt() throws {
    let input = SourceFile(contents: "foo()")
    XCTAssertNotNil(try apply(Parser.exprStmt, on: input))
  }

  // MARK: Operators

  func testTakeOperator() throws {
    let input = SourceFile(contents: "+ & == | < <= > >=")
    var context = ParserState(ast: AST(), lexer: Lexer(tokenizing: input))
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

  func testDeclAttribute() throws {
    let input = SourceFile(contents: "@attr")
    let attribute = try XCTUnwrap(input.parse(with: Parser.parseDeclAttribute).element)
    XCTAssertEqual(attribute.value.name.value, "@attr")
    XCTAssertEqual(attribute.value.arguments.count, 0)
  }

  func testDeclAttributeWithArguments() throws {
    let input = SourceFile(contents: #"@attr(8, "Val")"#)
    let attribute = try XCTUnwrap(input.parse(with: Parser.parseDeclAttribute).element)
    XCTAssertEqual(attribute.value.name.value, "@attr")
    XCTAssertEqual(attribute.value.arguments.count, 2)
  }

  // MARK: Helpers

  /// Applies `combinator` on `input`, optionally setting `context` in the parser state.
  func apply<C: Combinator>(
    _ combinator: C, on input: SourceFile, context: ParserState.Context? = nil
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
    var state = ParserState(ast: AST(), lexer: Lexer(tokenizing: self))
    if let c = context {
      state.contexts.append(c)
    }

    let element = try parser(&state)
    return (element, state.ast)
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
