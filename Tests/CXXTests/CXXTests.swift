import XCTest
import Compiler
import Library

final class CXXTests: XCTestCase {

  func testSimpleType() {

    // public type Vector2 {
    //   public var x: Double
    //   public var y: Double
    //   public memberwise init
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
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .memberwiseInit),
          accessModifier: SourceRepresentable(value: .public)))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))

    var transpiler = Transpiler(
      ast: checker.ast,
      scopeHierarchy: checker.scopeHierarchy,
      declTypes: checker.declTypes)
    let header = transpiler.emitHeader(of: main)

    XCTAssertEqual(header, """
    #ifndef VAL_SOMELIB
    #define VAL_SOMELIB

    #include <utility>
    #include <functional>

    namespace SomeLib {

    class Vector2;

    class Vector2 {
    public:
      Val::Double x;
      Val::Double y;
    public:
      Vector2() = delete;
      Vector2(Vector2&&) = delete;
      Vector2& operator=(Vector2&&) = delete;
      Vector2(Vector2 const&) = delete;
      Vector2& operator=(Vector2 const&) = delete;
      explicit Vector2(Val::Double&& x, Val::Double&& y): x(std::move(x)), y(std::move(y)) {}
    };

    }

    #endif

    """)
  }

}
