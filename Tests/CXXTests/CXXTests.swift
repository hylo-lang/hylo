import XCTest
import Compiler
import Library

final class CXXTests: XCTestCase {

  func testSimpleTypeHeader() throws {

    // public type Vector2 {
    //   public var x: Double
    //   public var y: Double
    //   public memberwise init
    //
    //   public fun dot(_ other: Self) -> Double { ... }
    //   public fun offset(by delta: Self) -> Self {
    //     let   { ... }
    //     inout { ... }
    //     sink  { ... }
    //   }
    // }

    var ast = AST()
    try ast.importValModule()
    let main = ast.insert(ModuleDecl(name: "SomeLib"))
    let source = ast.insert(TopLevelDeclSet())
    ast[main].sources.append(source)

    ast[source].decls.append(AnyDeclID(ast.insert(ProductTypeDecl(
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
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          accessModifier: SourceRepresentable(value: .public),
          identifier: SourceRepresentable(value: "dot"),
          parameters: [
            ast.insert(ParameterDecl(
              identifier: SourceRepresentable(value: "other"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Self")))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Double")))),
          body: .expr(AnyExprID(ast.insert(IntegerLiteralExpr(value: "0"))))))),
        AnyDeclID(ast.insert(FunDecl(
          introducer: SourceRepresentable(value: .fun),
          accessModifier: SourceRepresentable(value: .public),
          identifier: SourceRepresentable(value: "offset"),
          parameters: [
            ast.insert(ParameterDecl(
              label: SourceRepresentable(value: "by"),
              identifier: SourceRepresentable(value: "other"),
              annotation: ast.insert(ParameterTypeExpr(
                convention: SourceRepresentable(value: .let),
                bareType: AnyTypeExprID(ast.insert(NameTypeExpr(
                  identifier: SourceRepresentable(value: "Self")))))))),
          ],
          output: AnyTypeExprID(ast.insert(NameTypeExpr(
            identifier: SourceRepresentable(value: "Self")))),
          body: .bundle([
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .let),
              body: .expr(AnyExprID(ast.insert(NameExpr(
                name: SourceRepresentable(value: "self"))))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .inout),
              body: .expr(AnyExprID(ast.insert(NameExpr(
                name: SourceRepresentable(value: "self"))))))),
            ast.insert(MethodImplDecl(
              introducer: SourceRepresentable(value: .sink),
              body: .expr(AnyExprID(ast.insert(NameExpr(
                name: SourceRepresentable(value: "self"))))))),
          ])))),
      ]))))

    var checker = TypeChecker(ast: ast)
    XCTAssertTrue(checker.check(module: main))

    let program = TypedProgram(
      ast: checker.ast,
      scopeHierarchy: checker.scopeHierarchy,
      declTypes: checker.declTypes,
      exprTypes: checker.exprTypes,
      referredDecls: checker.referredDecls)
    var transpiler = CXXTranspiler(program: program)
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
    SomeLib::Vector2 offset(SomeLib::Vector2 const&) const;
    SomeLib::Vector2 sink_offset(SomeLib::Vector2 const&) &&;
    Val::Double dot(SomeLib::Vector2 const&) const;
    void inplace_offset(SomeLib::Vector2 const&);
    };
    }
    #endif

    """)
  }

}
