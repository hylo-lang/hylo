import XCTest
import Compiler
import Library

final class CXXTests: XCTestCase {

  func testSimpleType() {

    // public type Vector2 {
    //   public var x: Double
    //   public var y: Double
    // }

    var ast = AST()
    insertStandardLibraryMockup(into: &ast)
    let main = ast.insert(ModuleDecl(name: "SomeLib"))

    ast[main].members.append(AnyDeclID(ast.insert(ProductTypeDecl(
      accessModifier: SourceRepresentable(value: .public),
      identifier: SourceRepresentable(value: "Vector2"),
      members: [
        AnyDeclID(ast.insert(BindingDecl(
          accessModifier: SourceRepresentable(value: .public),
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "x")))))),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Double"))))))))),
        AnyDeclID(ast.insert(BindingDecl(
          accessModifier: SourceRepresentable(value: .public),
          pattern: ast.insert(BindingPattern(
            introducer: SourceRepresentable(value: .var),
            subpattern: AnyPatternID(ast.insert(NamePattern(
              decl: ast.insert(VarDecl(
                identifier: SourceRepresentable(value: "y")))))),
            annotation: AnyTypeExprID(ast.insert(NameTypeExpr(
              identifier: SourceRepresentable(value: "Double"))))))))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))

    let transpiler = Transpiler(
      ast: ast,
      scopeHierarchy: checker.scopeHierarchy,
      declTypes: checker.declTypes)
    let header = transpiler.emitHeader(of: main)

    XCTAssertEqual(header, """
    #ifndef VAL_SOMELIB
    #define VAL_SOMELIB

    namespace SomeLib {

    class Vector2;

    class Vector2 {
    public:
    Val::Double x;
    Val::Double y;
    };

    }

    #endif

    """)
  }

}
