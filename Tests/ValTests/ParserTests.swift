import XCTest
import ParserCombinators

@testable import Compiler

final class ParserTests: XCTestCase {

  // MARK: Declarations

  func testImportDecl() throws {
    let input = SourceFile(contents: "import Foo")
    let (declID, ast) = try apply(Parser.importDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.name, "Foo")
  }

  func testNamespaceDecl() throws {
    let input = SourceFile(contents: """
    namespace A {
      ;;
      let x = "Hello!"
      public let y = 0;
    }
    """)
    let (declID, ast) = try apply(Parser.namespaceDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testNamespaceMember() throws {
    let input = SourceFile(contents: "fun foo() {}")
    XCTAssertNotNil(try apply(Parser.namespaceMember, on: input))
  }

  func testNamespaceMemberPublic() throws {
    let input = SourceFile(contents: "public fun foo() {}")
    let (declID, ast) = try apply(Parser.namespaceMember, on: input)
    let decl = try XCTUnwrap(ast[declID] as? FunDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
  }

  func testTypeAliasDecl() throws {
    let input = SourceFile(contents: "typealias A = B")
    XCTAssertNotNil(try apply(Parser.typeAliasDecl, on: input))
  }

  func testTypeAliasDeclWithGenericClause() throws {
    let input = SourceFile(contents: "typealias A<T> = B<T>")
    let (declID, ast) = try apply(Parser.typeAliasDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
  }

  func testProductTypeDecl() throws {
    let input = SourceFile(contents: """
    type A {
      var x: Int; var y: Int
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.productTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)
  }

  func testProductTypeDeclWithGenericClause() throws {
    let input = SourceFile(contents: """
    type A<T, U> {
            var x: Int; var y: Int
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.productTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
  }

  func testProductTypeDeclWithConformances() throws {
    let input = SourceFile(contents: """
    type A: Foo, Bar {
      var x: Int; var y: Int
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.productTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testProductTypeDeclWithGenericClauseAndConformances() throws {
    let input = SourceFile(contents: """
    type A<T>: Foo {
      var x: Int
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.productTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.conformances)
  }

  func testProductTypeMember() throws {
    let input = SourceFile(contents: "var x: Int")
    XCTAssertNotNil(try apply(Parser.productTypeMember, on: input, flags: .parsingProductBody))
  }

  func testProductTypeMemberPublic() throws {
    let input = SourceFile(contents: "public var x: Int")
    let (declID, ast) = try apply(Parser.productTypeMember, on: input, flags: .parsingProductBody)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
  }

  func testProductTypeMemberStatic() throws {
    let input = SourceFile(contents: "static var x: Int")
    let (declID, ast) = try apply(Parser.productTypeMember, on: input, flags: .parsingProductBody)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testProductTypeMemberPublicStatic() throws {
    let input = SourceFile(contents: "public static var x: Int")
    let (declID, ast) = try apply(Parser.productTypeMember, on: input, flags: .parsingProductBody)
    let decl = try XCTUnwrap(ast[declID] as? BindingDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testTraitDecl() throws {
    let input = SourceFile(contents: """
    trait A {
      type B
      property b: B
    }
    """)
    let (declID, ast) = try apply(Parser.traitDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testTraitDeclWithRefinements() throws {
    let input = SourceFile(contents: """
    trait A: Foo {
      type B
      property b: B
    }
    """)
    let (declID, ast) = try apply(Parser.traitDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.refinements.count, 1)
  }

  func testMethodBundleRequirement() throws {
    let input = SourceFile(contents: """
    fun foo() -> T {
      let
      inout
    }
    """)
    let (declID, ast) = try apply(Parser.traitMember, on: input, flags: .parsingTraitBody)
    let decl = try XCTUnwrap(ast[declID] as? FunDecl)
    if case .bundle(let impls) = decl.body {
      XCTAssertEqual(impls.count, 2)
    } else {
      XCTFail()
    }
  }

  func testSubscriptRequirement() throws {
    let input = SourceFile(contents: """
    subscript foo(): T {
      let
      inout
    }
    """)
    let (declID, ast) = try apply(Parser.traitMember, on: input, flags: .parsingTraitBody)
    let decl = try XCTUnwrap(ast[declID] as? SubscriptDecl)
    if case .bundle(let impls) = decl.body {
      XCTAssertEqual(impls.count, 2)
    } else {
      XCTFail()
    }
  }

  func testPropertyRequirement() throws {
    let input = SourceFile(contents: "property foo: T")
    XCTAssertNotNil(try apply(Parser.traitMember, on: input, flags: .parsingTraitBody))
  }

  func testAssociatedTypeDecl() throws {
    let input = SourceFile(contents: "type Foo")
    let (declID, ast) = try apply(Parser.associatedTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "Foo")
  }

  func testAssociatedTypeDeclWithConformances() throws {
    let input = SourceFile(contents: "type Foo: Bar, Ham")
    let (declID, ast) = try apply(Parser.associatedTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.conformances)
  }

  func testAssociatedTypeDeclWithWhereClause() throws {
    let input = SourceFile(contents: "type Foo where Foo.Bar == Ham")
    let (declID, ast) = try apply(Parser.associatedTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testAssociatedTypeDeclWithWithDefault() throws {
    let input = SourceFile(contents: "type Foo = X")
    let (declID, ast) = try apply(Parser.associatedTypeDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testAssociatedValueDecl() throws {
    let input = SourceFile(contents: "value foo")
    let (declID, ast) = try apply(Parser.associatedValueDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier.value, "foo")
  }

  func testAssociatedValueDeclWithWhereClause() throws {
    let input = SourceFile(contents: "value foo where @value foo > bar")
    let (declID, ast) = try apply(Parser.associatedValueDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testAssociatedValueDeclWithDefault() throws {
    let input = SourceFile(contents: "value foo = 42")
    let (declID, ast) = try apply(Parser.associatedValueDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.defaultValue)
  }

  func testConformanceDecl() throws {
    let input = SourceFile(contents: """
    conformance A: Foo {
      public fun bar() {}
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.conformanceDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 2)
  }

  func testConformanceDeclWithWhereClause() throws {
    let input = SourceFile(contents: """
    conformance A: Foo where A.Bar == Ham {
      public fun bar() {}
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.conformanceDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionDecl() throws {
    let input = SourceFile(contents: """
    extension A {
      type B {}; property z: Int { x }
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.extensionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.members.count, 3)
  }

  func testExtensionDeclWithWhereClause() throws {
    let input = SourceFile(contents: """
    extension A where Foo: Bar {
      type B {}; property z: Int { x }
      fun foo() -> Int { x.copy() }
    }
    """)
    let (declID, ast) = try apply(Parser.extensionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.whereClause)
  }

  func testExtensionMember() throws {
    let input = SourceFile(contents: "public static fun forty_two() -> Int { 42 }")
    let (declID, ast) = try apply(Parser.extensionMember, on: input, flags: .parsingExtensionBody)
    let decl = try XCTUnwrap(ast[declID] as? FunDecl)
    XCTAssertEqual(decl.accessModifier?.value, .public)
    XCTAssertEqual(decl.memberModifier?.value, .static)
  }

  func testBindingDecl() throws {
    let input = SourceFile(contents: "let (foo, bar)")
    XCTAssertNotNil(try apply(Parser.bindingDecl, on: input))
  }

  func testBindingDeclWithInitializer() throws {
    let input = SourceFile(contents: "let (foo, bar) = (true, ham())")
    let (declID, ast) = try apply(Parser.bindingDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNotNil(decl.initializer)
  }

  func testDeinitDecl() throws {
    let input = SourceFile(contents: "deinit {}")
    XCTAssertNotNil(try apply(Parser.deinitDecl, on: input))
  }

  func testMemberwiseInitDecl() throws {
    let input = SourceFile(contents: "memberwise init")
    let (declID, ast) = try apply(Parser.initDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .memberwiseInit)
  }

  func testInitDecl() throws {
    let input = SourceFile(contents: "init() {}")
    let (declID, ast) = try apply(Parser.initDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.body)
  }

  func testInitDeclGeneric() throws {
    let input = SourceFile(contents: "init<T>() {}")
    let (declID, ast) = try apply(Parser.initDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .`init`)
    XCTAssertNotNil(decl.genericClause)
    XCTAssertNotNil(decl.body)
  }

  func testFunDecl() throws {
    let input = SourceFile(contents: "fun foo() {}")
    let (declID, ast) = try apply(Parser.functionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.introducer.value, .fun)
    XCTAssertNotNil(decl.body)
  }

  func testFunDeclWithCaptureList() throws {
    let input = SourceFile(contents: "fun foo[let x = 42]() {}")
    let (declID, ast) = try apply(Parser.functionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.explicitCaptures.count, 1)
    XCTAssertNotNil(decl.body)
  }

  func testFunDeclWithExprBody() throws {
    let input = SourceFile(contents: "fun id<T: Sinkable>(_ x: T) -> T { x }")
    let (declID, ast) = try apply(Parser.functionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testMethodBundle() throws {
    let input = SourceFile(contents: """
    fun foo() {
      let  { self.copy() }
      sink { self }
    }
    """)
    let (declID, ast) = try apply(Parser.functionDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .bundle = decl.body {
    } else {
      XCTFail()
    }
  }

  func testFunctionSignature() throws {
    let input = SourceFile(contents: "()")
    let signature = try XCTUnwrap(try apply(Parser.functionSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 0)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionSignatureWithParameters() throws {
    let input = SourceFile(contents: "(_ foo: Foo, bar: Bar = .default)")
    let signature = try XCTUnwrap(try apply(Parser.functionSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 2)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertNil(signature.output)
  }

  func testFunctionSignatureWithEffect() throws {
    let input = SourceFile(contents: "(_ foo: Foo) inout")
    let signature = try XCTUnwrap(try apply(Parser.functionSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .inout)
    XCTAssertNil(signature.output)
  }

  func testFunctionSignatureWithOutput() throws {
    let input = SourceFile(contents: "(_ foo: Foo) -> C")
    let signature = try XCTUnwrap(try apply(Parser.functionSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertNil(signature.receiverEffect)
    XCTAssertEqual(signature.output?.kind, .nameTypeExpr)
  }

  func testFunctionSignatureWithOutputAndEffect() throws {
    let input = SourceFile(contents: "(_ foo: Foo) sink -> C")
    let signature = try XCTUnwrap(try apply(Parser.functionSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .sink)
    XCTAssertEqual(signature.output?.kind, .nameTypeExpr)
  }

  func testNamedFunctionDeclIdentifier() throws {
    let input = SourceFile(contents: "fun foo")
    let identifier = try XCTUnwrap(try apply(Parser.functionDeclIdentifier, on: input).element)
    XCTAssertEqual(identifier.introducer.value, .fun)
    XCTAssertEqual(identifier.stem?.value, "foo")
    XCTAssertNil(identifier.notation)
  }

  func testOperatorFunctionDeclIdentifier() throws {
    let input = SourceFile(contents: "postfix fun ++")
    let identifier = try XCTUnwrap(try apply(Parser.functionDeclIdentifier, on: input).element)
    XCTAssertEqual(identifier.introducer.value, .fun)
    XCTAssertEqual(identifier.stem?.value, "++")
    XCTAssertEqual(identifier.notation?.value, .postfix)
  }

  func testFunctionBodyBlock() throws {
    let input = SourceFile(contents: "{}")
    let (body, _) = try apply(Parser.functionBody, on: input)
    if case .block = body {
    } else {
      XCTFail()
    }
  }

  func testFunctionBodyExpr() throws {
    let input = SourceFile(contents: "{ 0x2a }")
    let (body, _) = try apply(Parser.functionBody, on: input)
    if case .expr = body {
    } else {
      XCTFail()
    }
  }

  func testFunctionBodyBundle() throws {
    let input = SourceFile(contents: """
    {
      let  { self.copy() }
      sink { self }
    }
    """)
    let (body, _) = try apply(Parser.functionBody, on: input)
    if case .bundle(let impls) = body {
      XCTAssertEqual(impls.count, 2)
    } else {
      XCTFail()
    }
  }

  func testMethodImplBlock() throws {
    let input = SourceFile(contents: "let { }")
    let (declID, ast) = try apply(Parser.methodImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .block = decl.body {
    } else {
      XCTFail()
    }
  }

  func testMethodImplExpr() throws {
    let input = SourceFile(contents: "let { foo }")
    let (declID, ast) = try apply(Parser.methodImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testPropertyDecl() throws {
    let input = SourceFile(contents: "property foo: T {}")
    let (declID, ast) = try apply(Parser.propertyDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNil(decl.parameters)
    XCTAssertNotNil(decl.body)
  }

  func testSubscriptDecl() throws {
    let input = SourceFile(contents: "subscript foo(): T {}")
    let (declID, ast) = try apply(Parser.subscriptDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.identifier?.value, "foo")
    XCTAssertNotNil(decl.parameters)
    XCTAssertNotNil(decl.body)
  }

  func testSubscriptDeclAnonymous() throws {
    let input = SourceFile(contents: "subscript (): T {}")
    let (declID, ast) = try apply(Parser.subscriptDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertNil(decl.identifier)
  }

  func testSubscriptDeclWithCaptureList() throws {
    let input = SourceFile(contents: "subscript foo[let x = 42](): T {}")
    let (declID, ast) = try apply(Parser.subscriptDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.explicitCaptures.count, 1)
  }

  func testSubscriptDeclWithExprBody() throws {
    let input = SourceFile(contents: "subscript foo<T: Foo>(_ x: T): T { x }")
    let (declID, ast) = try apply(Parser.subscriptDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
  }

  func testSubscriptBundle() throws {
    let input = SourceFile(contents: """
    subscript foo(): T {
      let  { T() }
      sink { T() }
    }
    """)
    let (declID, ast) = try apply(Parser.subscriptDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .bundle = decl.body {
    } else {
      XCTFail()
    }
  }

  func testSubscriptSignature() throws {
    let input = SourceFile(contents: "(): T")
    let signature = try XCTUnwrap(try apply(Parser.subscriptSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 0)
    XCTAssertNil(signature.receiverEffect)
  }

  func testSubscriptSignatureWithParameters() throws {
    let input = SourceFile(contents: "(_ foo: Foo, bar: Bar = .default): T")
    let signature = try XCTUnwrap(try apply(Parser.subscriptSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 2)
    XCTAssertNil(signature.receiverEffect)
  }

  func testSubscriptSignatureWithEffect() throws {
    let input = SourceFile(contents: "(_ foo: Foo) yielded : T")
    let signature = try XCTUnwrap(try apply(Parser.subscriptSignature, on: input).element)
    XCTAssertEqual(signature.parameters.count, 1)
    XCTAssertEqual(signature.receiverEffect?.value, .yielded)
  }

  func testSubscriptBodyBlock() throws {
    let input = SourceFile(contents: "{}")
    let (body, _) = try apply(Parser.subscriptBody, on: input)
    if case .block = body {
    } else {
      XCTFail()
    }
  }

  func testSubscriptBodyExpr() throws {
    let input = SourceFile(contents: "{ 0x2a }")
    let (body, _) = try apply(Parser.subscriptBody, on: input)
    if case .expr = body {
    } else {
      XCTFail()
    }
  }

  func testSubscriptBodyBundle() throws {
    let input = SourceFile(contents: """
    {
      let  { self.copy() }
      sink { self }
    }
    """)
    let (body, _) = try apply(Parser.subscriptBody, on: input)
    if case .bundle(let impls) = body {
      XCTAssertEqual(impls.count, 2)
    } else {
      XCTFail()
    }
  }

  func testSubscriptImplBlock() throws {
    let input = SourceFile(contents: "let { }")
    let (declID, ast) = try apply(Parser.subscriptImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .block = decl.body {
    } else {
      XCTFail()
    }
  }

  func testSubscriptImplExpr() throws {
    let input = SourceFile(contents: "let { foo }")
    let (declID, ast) = try apply(Parser.subscriptImpl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    if case .expr = decl.body {
    } else {
      XCTFail()
    }
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
    let (declID, ast) = try apply(Parser.operatorDecl, on: input)
    let decl = try XCTUnwrap(ast[declID])
    XCTAssertEqual(decl.notation.value, .infix)
    XCTAssertNil(decl.precedenceGroup)
  }

  func testOperatorDeclWithPredecenceGroup() throws {
    let input = SourceFile(contents: "operator infix+ : addition")
    let (declID, ast) = try apply(Parser.operatorDecl, on: input)
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
    let input = SourceFile(contents: "<T, @value n: Int>")
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

  func testGenericTypeParameter() throws {
    let input = SourceFile(contents: "T")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .type(let typeDeclID) = declID {
      XCTAssertEqual(ast[typeDeclID].name, "T")
    } else {
      XCTFail()
    }
  }

  func testGenericTypeParameterWithIntroducer() throws {
    let input = SourceFile(contents: "@type T")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .type(let typeDeclID) = declID {
      XCTAssertEqual(ast[typeDeclID].name, "T")
    } else {
      XCTFail()
    }
  }

  func testGenericTypeParameterWithConformances() throws {
    let input = SourceFile(contents: "T: Foo & Bar")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .type(let typeDeclID) = declID {
      XCTAssertEqual(ast[typeDeclID].conformances.count, 2)
    } else {
      XCTFail()
    }
  }

  func testGenericTypeParameterWithDefault() throws {
    let input = SourceFile(contents: "T = U")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .type(let typeDeclID) = declID {
      XCTAssertNotNil(ast[typeDeclID].defaultValue)
    } else {
      XCTFail()
    }
  }

  func testGenericTypeParameterWithConformancesANdDefault() throws {
    let input = SourceFile(contents: "T: Foo & Bar = U")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .type(let typeDeclID) = declID {
      XCTAssertEqual(ast[typeDeclID].conformances.count, 2)
      XCTAssertNotNil(ast[typeDeclID].defaultValue)
    } else {
      XCTFail()
    }
  }

  func testGenericValueParameter() throws {
    let input = SourceFile(contents: "@value n: Int")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .value(let valueDeclID) = declID {
      XCTAssertEqual(ast[valueDeclID].name, "n")
    } else {
      XCTFail()
    }
  }

  func testGenericValueParameterWithDefault() throws {
    let input = SourceFile(contents: "@value n: Int = 0o52")
    let (declID, ast) = try apply(Parser.genericParameter, on: input)
    if case .value(let valueDeclID) = declID {
      XCTAssertEqual(ast[valueDeclID].name, "n")
      XCTAssertNotNil(ast[valueDeclID].defaultValue)
    } else {
      XCTFail()
    }
  }

  func testConformanceList() throws {
    let input = SourceFile(contents: ": Foo, Bar, Ham")
    let list = try XCTUnwrap(try apply(Parser.conformanceList, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Value expressions

  func testExpr() throws {
    let input = SourceFile(contents: "(foo().bar[] + 42, ham++, !baz)")
    let (exprID, _) = try apply(Parser.infixExpr, on: input)
    XCTAssertEqual(exprID?.kind, .tupleExpr)
  }

  func testInfixExpr() throws {
    let input = SourceFile(contents: "foo == 2 & true")
    let (exprID, ast) = try apply(Parser.infixExpr, on: input)
    let sequence = try XCTUnwrap(ast[exprID] as? SequenceExpr)
    if case .unfolded(let head, let tail) = sequence {
      XCTAssertEqual(head.kind, .nameExpr)
      XCTAssertEqual(tail.count, 2)
      if tail.count == 2 {
        XCTAssertEqual(tail[0].operator.value, "==")
        XCTAssertEqual(tail[0].rhs.kind, .integerLiteralExpr)
      }
    } else {
      XCTFail()
    }
  }

  func testCastExprUp() throws {
    let input = SourceFile(contents: "foo as T")
    let (exprID, ast) = try apply(Parser.infixExpr, on: input)
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.direction, .up)
  }

  func testCastExprDown() throws {
    let input = SourceFile(contents: "foo as! T")
    let (exprID, ast) = try apply(Parser.infixExpr, on: input)
    let cast = try XCTUnwrap(ast[exprID] as? CastExpr)
    XCTAssertEqual(cast.direction, .down)
  }

  func testAsyncExprInline() throws {
    let input = SourceFile(contents: "async foo")
    XCTAssertNotNil(try apply(Parser.asyncExpr, on: input))
  }

  func testAsyncExprInlineWithCaptureList() throws {
    let input = SourceFile(contents: "async[let x = a] foo")
    let (exprID, ast) = try apply(Parser.asyncExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
  }

  func testAsyncExprInlineWithEffect() throws {
    let input = SourceFile(contents: "async[var x = a] inout foo")
    let (exprID, ast) = try apply(Parser.asyncExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(ast[expr.decl].explicitCaptures.count, 1)
    XCTAssertNotNil(ast[expr.decl].receiverEffect)
  }

  func testAsyncExprBlock() throws {
    let input = SourceFile(contents: "async -> T { return foo }")
    XCTAssertNotNil(try apply(Parser.asyncExpr, on: input))
  }

  func testAwaitExpr() throws {
    let input = SourceFile(contents: "await foo")
    let (exprID, ast) = try apply(Parser.awaitExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.operand.kind, .nameExpr)
  }

  func testAwaitAwaitExpr() throws {
    let input = SourceFile(contents: "await await foo")
    let (exprID, ast) = try apply(Parser.awaitExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.operand.kind, .awaitExpr)
  }

  func testPrefixExpr() throws {
    let input = SourceFile(contents: "+foo")
    let (exprID, ast) = try apply(Parser.prefixExpr, on: input)
    let call = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .prefix)

    if case .expr(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .nameExpr)
    } else {
      XCTFail()
    }
  }

  func testPostfixExpr() throws {
    let input = SourceFile(contents: "foo+")
    let (exprID, ast) = try apply(Parser.postfixExpr, on: input)
    let call = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(call.arguments.count, 0)

    let callee = try XCTUnwrap(ast[call.callee] as? NameExpr)
    XCTAssertEqual(callee.name.value.stem, "+")
    XCTAssertEqual(callee.name.value.notation, .postfix)

    if case .expr(let receiverID) = callee.domain {
      XCTAssertEqual(receiverID.kind, .nameExpr)
    } else {
      XCTFail()
    }
  }

  // MARK: Compound expressions

  func testLabeledMemberExpr() throws {
    let input = SourceFile(contents: "a.b.c")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
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
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? TupleMemberExpr)
    XCTAssertEqual(expr.index, 12)

    let parentExpr = try XCTUnwrap(ast[expr.tuple] as? NameExpr)
    XCTAssertEqual(parentExpr.name.value.stem, "foo")
  }

  func testStaticValueMemberExpr() throws {
    let input = SourceFile(contents: "{ A, B }.meta")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? NameExpr)
    XCTAssertEqual(expr.name.value.stem, "meta")

    if case .type(let domainID) = expr.domain {
      XCTAssertEqual(domainID.kind, .tupleTypeExpr)
    } else {
      XCTFail()
    }
  }

  func testFunctionCallExprWithoutArguments() throws {
    let input = SourceFile(contents: "foo()")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testFunctionCallExpr() throws {
    let input = SourceFile(contents: "foo(42, label: true)")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? FunCallExpr)
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "label")
    }
  }

  func testFunctionCallExprNewlineBeforeLParen() throws {
    let input = SourceFile(contents: "foo \n (42, label: true)")
    let (exprID, _) = try apply(Parser.compoundExpr, on: input)
    XCTAssertEqual(exprID?.kind, .nameExpr)
  }

  func testFunctionCallExprNewlineAfterLParen() throws {
    let input = SourceFile(contents: "foo \n (42, label: true)")
    let (exprID, _) = try apply(Parser.compoundExpr, on: input)
    XCTAssertEqual(exprID?.kind, .nameExpr)
  }

  func testSubscriptCallExprWithoutArguments() throws {
    let input = SourceFile(contents: "foo[]")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? SubscriptCallExpr)
    XCTAssertEqual(expr.arguments.count, 0)
  }

  func testSubscriptCallExpr() throws {
    let input = SourceFile(contents: "foo[42, label: true]")
    let (exprID, ast) = try apply(Parser.compoundExpr, on: input)
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
    let (exprID, ast) = try apply(Parser.booleanLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, true)
  }

  func testFalseLiteral() throws {
    let input = SourceFile(contents: "false")
    let (exprID, ast) = try apply(Parser.booleanLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, false)
  }

  func testDecimalLiteral() throws {
    let input = SourceFile(contents: "4_2_")
    let (exprID, ast) = try apply(Parser.integerLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, "42")
  }

  func testHexadecimalLiteral() throws {
    let input = SourceFile(contents: "0x2_a_")
    let (exprID, ast) = try apply(Parser.integerLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, "0x2a")
  }

  func testFloatingPointLiteral() throws {
    let input = SourceFile(contents: "4.2e+1")
    let (exprID, ast) = try apply(Parser.floatingPointLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, "4.2e+1")
  }

  func testStringLiteral() throws {
    let input = SourceFile(contents: #""Val""#)
    let (exprID, ast) = try apply(Parser.stringLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.value, "Val")
  }

  func testBufferLiteral() throws {
    let input = SourceFile(contents: "[]")
    XCTAssertNotNil(try apply(Parser.compoundLiteral, on: input))
  }

  func testBufferLiteralWithMultipleElements() throws {
    let input = SourceFile(contents: "[a, b, c]")
    let (exprID, ast) = try apply(Parser.compoundLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? BufferLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testMapLiteral() throws {
    let input = SourceFile(contents: "[:]")
    XCTAssertNotNil(try apply(Parser.compoundLiteral, on: input))
  }

  func testMapLiteralWithMultipleElements() throws {
    let input = SourceFile(contents: "[a: 0, b: 1, c: 2]")
    let (exprID, ast) = try apply(Parser.compoundLiteral, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? MapLiteralExpr)
    XCTAssertEqual(expr.elements.count, 3)
  }

  func testPrimaryDeclRef() throws {
    let input = SourceFile(contents: "foo<T, size: 42>")
    let (exprID, ast) = try apply(Parser.primaryDeclRef, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.name.value.stem, "foo")
    XCTAssertEqual(expr.arguments.count, 2)

    if expr.arguments.count == 2 {
      XCTAssertNil(expr.arguments[0].label)
      XCTAssertEqual(expr.arguments[1].label?.value, "size")
    }
  }

  func testImplicitMemberRef() throws {
    let input = SourceFile(contents: ".foo")
    let (exprID, ast) = try apply(Parser.implicitMemberRef, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.name.value.stem, "foo")
    XCTAssertEqual(expr.domain, .implicit)
  }

  func testLambdaExpr() throws {
    let input = SourceFile(contents: "fun (x) { x.foo() }")
    let (exprID, ast) = try apply(Parser.lambdaExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertTrue(ast[expr.decl].isInExprContext)
  }

  func testMatchExpr() throws {
    let input = SourceFile(contents: """
    match foo {
      let (x, y) where x == y { 0 }
      _ { 1 }
    }
    """)
    let (caseID, ast) = try apply(Parser.matchExpr, on: input)
    let expr = try XCTUnwrap(ast[caseID])
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
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.condition.count, 1)
  }

  func testConditionalExprWithMultipleConditions() throws {
    let input = SourceFile(contents: "if let x = foo, x > 0 { }")
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.condition.count, 2)
  }

  func testConditionalExprBlockThenNoElse() throws {
    let input = SourceFile(contents: "if true { }")
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    if case .block = expr.success {
    } else {
      XCTFail()
    }
    XCTAssertNil(expr.failure)
  }

  func testConditionalExprBlockThenBlockElse() throws {
    let input = SourceFile(contents: "if true { } else { }")
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])

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
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])

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
    let (exprID, ast) = try apply(Parser.conditionalExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    if case .expr(let elseID) = expr.failure {
      XCTAssertEqual(elseID.kind, .condExpr)
    } else {
      XCTFail()
    }
  }

  func testTupleExpr() throws {
    let input = SourceFile(contents: "()")
    let (exprID, ast) = try apply(Parser.tupleExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleExprWithOneElement() throws {
    let input = SourceFile(contents: "(42)")
    let (exprID, ast) = try apply(Parser.tupleExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleExprWithMultipleElements() throws {
    let input = SourceFile(contents: "((n, m), number: 42)")
    let (exprID, ast) = try apply(Parser.tupleExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testNilExpr() throws {
    let input = SourceFile(contents: "nil")
    XCTAssertNotNil(try apply(Parser.nilExpr, on: input))
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
    XCTAssertEqual(pattern.expr.kind, .nameExpr)
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
    let input = SourceFile(contents: """
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
    XCTAssertNotNil(try apply(Parser.returnStmt, on: input, flags: .parsingFunctionBody))
  }

  func testReturnStmtWithValue() throws {
    let input = SourceFile(contents: "return 42")
    let (stmtID, ast) = try apply(Parser.returnStmt, on: input, flags: .parsingFunctionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    XCTAssertNotNil(stmt.value)
  }

  func testYieldStmt() throws {
    let input = SourceFile(contents: "yield &foo.bar")
    XCTAssertNotNil(try apply(Parser.yieldStmt, on: input, flags: .parsingSubscriptBody))
  }

  func testBreak() throws {
    let input = SourceFile(contents: "break")
    XCTAssertNotNil(try apply(Parser.breakStmt, on: input, flags: .parsingLoopBody))
  }

  func testContinue() throws {
    let input = SourceFile(contents: "continue")
    XCTAssertNotNil(try apply(Parser.continueStmt, on: input, flags: .parsingLoopBody))
  }

  func testConditionalBinding() throws {
    let input = SourceFile(contents: "var x = foo() else return")
    let (stmtID, ast) = try apply(
      Parser.conditionalBindingStmt, on: input, flags: .parsingFunctionBody)
    let stmt = try XCTUnwrap(ast[stmtID])
    if case .exit = stmt.fallback {
    } else {
      XCTFail()
    }
  }

  func testConditionalBindingBlock() throws {
    let input = SourceFile(contents: "var x = foo() else { bar(); return }")
    let (stmtID, ast) = try apply(
      Parser.conditionalBindingStmt, on: input, flags: .parsingFunctionBody)
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
    XCTAssertNotNil(try apply(
      Parser.conditionalBindingStmt, on: input, flags: .parsingFunctionBody))
  }

  func testDeclStmt() throws {
    let input = SourceFile(contents: "typealias X = A")
    XCTAssertNotNil(try apply(Parser.declStmt, on: input))
  }

  func testExprStmt() throws {
    let input = SourceFile(contents: "foo()")
    XCTAssertNotNil(try apply(Parser.exprStmt, on: input))
  }

  // MARK: Type expressions

  func testUnionTypeExpr() throws {
    let input = SourceFile(contents: "A | B")
    let (exprID, ast) = try apply(Parser.unionTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? UnionTypeExpr)
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testExistentialTypeExpr() throws {
    let input = SourceFile(contents: "any T & U where T.Key == U.Value")
    let (exprID, ast) = try apply(Parser.modifiedTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? ExistentialTypeExpr)
    XCTAssertEqual(expr.traits.count, 2)
    XCTAssertEqual(expr.whereClause?.value.constraints.count, 1)
  }

  func testAsyncTypeExpr() throws {
    let input = SourceFile(contents: "async T")
    let (exprID, ast) = try apply(Parser.modifiedTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? AsyncTypeExpr)
    XCTAssertEqual(expr.operand.kind, .nameTypeExpr)
  }

  func testIndirectTypeExpr() throws {
    let input = SourceFile(contents: "indirect T")
    let (exprID, ast) = try apply(Parser.modifiedTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? IndirectTypeExpr)
    XCTAssertEqual(expr.operand.kind, .nameTypeExpr)
  }

  func testConformanceLensTypeExpr() throws {
    let input = SourceFile(contents: "{ T, U }::Baz")
    let (exprID, ast) = try apply(Parser.conformanceLensTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? ConformanceLensTypeExpr)
    XCTAssertEqual(expr.subject.kind, .tupleTypeExpr)
    XCTAssertEqual(expr.lens.kind, .nameTypeExpr)
  }

  func testTypeMemberExpr() throws {
    let input = SourceFile(contents: "A.B.C")
    let (exprID, ast) = try apply(Parser.typeMemberTypeExpr, on: input)
    var expr = try XCTUnwrap(ast[exprID] as? NameTypeExpr)
    XCTAssertEqual(expr.identifier.value, "C")

    expr = try XCTUnwrap(ast[expr.domain] as? NameTypeExpr)
    XCTAssertEqual(expr.identifier.value, "B")
  }

  func testInoutExpr() throws {
    let input = SourceFile(contents: "&foo")
    XCTAssertNotNil(try apply(Parser.inoutExpr, on: input))
  }

  func testTupleTypeExpr() throws {
    let input = SourceFile(contents: "{}")
    let (exprID, ast) = try apply(Parser.tupleTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 0)
  }

  func testTupleTypeExprWithOneElement() throws {
    let input = SourceFile(contents: "{ T }")
    let (exprID, ast) = try apply(Parser.tupleTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 1)

    if expr.elements.count == 1 {
      XCTAssertNil(expr.elements[0].label)
    }
  }

  func testTupleTypeExprWithMultipleElements() throws {
    let input = SourceFile(contents: "{ { T, U }, number: V }")
    let (exprID, ast) = try apply(Parser.tupleTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.elements.count, 2)
  }

  func testLambdaOrParenthesizedTypeExpr() throws {
    let input = SourceFile(contents: "((A) -> (B)) -> C")
    let (exprID, ast) = try apply(Parser.lambdaOrParenthesizedTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID] as? LambdaTypeExpr)
    XCTAssertEqual(expr.output.kind, .nameTypeExpr)
  }

  func testParenthesizedTypeExpr() throws {
    let input = SourceFile(contents: "({ A, B })")
    let (exprID, _) = try apply(Parser.parenthesizedTypeExpr, on: input)
    XCTAssertEqual(exprID?.kind, .tupleTypeExpr)
  }

  func testLambdaTypeExpr() throws {
    let input = SourceFile(contents: "[{ A, B }] (T, by: U) inout -> T")
    let (exprID, ast) = try apply(Parser.lambdaTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.environment?.value.kind, .tupleTypeExpr)
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .nameTypeExpr)
  }

  func testTypeErasedLambdaTypeExpr() throws {
    let input = SourceFile(contents: "(T, by: U) inout -> T")
    let (exprID, ast) = try apply(Parser.lambdaTypeExpr, on: input)
    let expr = try XCTUnwrap(ast[exprID])
    XCTAssertEqual(expr.receiverEffect?.value, .inout)
    XCTAssertEqual(expr.parameters.count, 2)
    XCTAssertEqual(expr.output.kind, .nameTypeExpr)
    XCTAssertNil(expr.environment)
  }

  func testThinLambdaEnvironment() throws {
    let input = SourceFile(contents: "thin")
    let environment = try XCTUnwrap(try apply(Parser.lambdaEnvironment, on: input).element)
    XCTAssertEqual(environment.value.kind, .tupleTypeExpr)
  }

  func testCustomLambdaEnvironement() throws {
    let input = SourceFile(contents: "[any Copyable]")
    let environment = try XCTUnwrap(try apply(Parser.lambdaEnvironment, on: input).element)
    XCTAssertEqual(environment.value.kind, .existentialTypeExpr)
  }

  func testWildcardTypeExpr() throws {
    let input = SourceFile(contents: "_")
    XCTAssertNotNil(try apply(Parser.wildcardTypeExpr, on: input))
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
    let input = SourceFile(contents: "<T, size: 42>")
    let list = try XCTUnwrap(try apply(Parser.staticArgumentList, on: input).element)
    XCTAssertEqual(list.count, 2)
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
      XCTAssertEqual(lhs.kind, .nameTypeExpr)
      XCTAssertEqual(rhs.kind, .tupleTypeExpr)
    } else {
      XCTFail()
    }
  }

  func testWhereClauseConformanceConstraint() throws {
    let input = SourceFile(contents: "T : U & V")
    let constraint = try XCTUnwrap(try apply(Parser.typeConstraint, on: input).element)
    if case .conformance(let lhs, _) = constraint.value {
      XCTAssertEqual(lhs.kind, .nameTypeExpr)
    } else {
      XCTFail()
    }
  }

  func testWhereClauseValueConstraint() throws {
    let input = SourceFile(contents: "@value x > 2")
    let constraint = try XCTUnwrap(try apply(Parser.valueConstraint, on: input).element)
    if case .value(let exprID) = constraint.value {
      XCTAssertEqual(exprID.kind, .sequenceExpr)
    } else {
      XCTFail()
    }
  }

  func testTraitComposition() throws {
    let input = SourceFile(contents: "T & U & V")
    let list = try XCTUnwrap(try apply(Parser.traitComposition, on: input).element)
    XCTAssertEqual(list.count, 3)
  }

  // MARK: Identifiers

  func testIdentifierExpr() throws {
    let input = SourceFile(contents: "foo")
    let name = try XCTUnwrap(try apply(Parser.identifierExpr, on: input).element)
    XCTAssertEqual(name.value.stem, "foo")
  }

  func testIdentifierExprWithIntroducer() throws {
    let input = SourceFile(contents: "foo(_:bar:).inout")
    let name = try XCTUnwrap(try apply(Parser.identifierExpr, on: input).element)
    XCTAssertEqual(name.value.stem, "foo")
    XCTAssertEqual(name.value.labels, [nil, "bar"])
    XCTAssertEqual(name.value.introducer, .inout)
  }

  func testEntityIdentifier() throws {
    let input = SourceFile(contents: "foo")
    let name = try XCTUnwrap(try apply(Parser.entityIdentifier, on: input).element)
    XCTAssertEqual(name.value.stem, "foo")
  }

  func testFunctionEntityIdentifier() throws {
    let input = SourceFile(contents: "foo(_:bar:)")
    let name = try XCTUnwrap(try apply(Parser.entityIdentifier, on: input).element)
    XCTAssertEqual(name.value.stem, "foo")
    XCTAssertEqual(name.value.labels, [nil, "bar"])
  }

  func testOperatorEntityIdentifier() throws {
    let input = SourceFile(contents: "infix+")
    let name = try XCTUnwrap(try apply(Parser.entityIdentifier, on: input).element)
    XCTAssertEqual(name.value.stem, "+")
    XCTAssertEqual(name.value.notation, .infix)
  }

  /// Applies `combinator` on `input`, optionally setting `flags` in the parser context.
  func apply<Combinator: ParserCombinator>(
    _ combinator: Combinator,
    on input: SourceFile,
    flags: ParserContext.Flags? = nil
  ) throws -> (element: Combinator.Element?, ast: AST) where Combinator.Context == ParserContext {
    var context = ParserContext(ast: AST(), lexer: Lexer(tokenizing: input))
    if let f = flags {
      context.flags = context.flags | f
    }

    let element = try combinator.parse(&context)
    return (element, context.ast)
  }

}
