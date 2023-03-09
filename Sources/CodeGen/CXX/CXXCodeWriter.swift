import ValModule

/// A type used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init(formatter: CodeTransform? = nil) {
    self.formatter = formatter
  }

  /// Object used to format output C++ code.
  private let formatter: CodeTransform?

  /// Indicates if we are currently writing the core library module.
  private var isCoreLibrary: Bool = false

  // MARK: API

  /// Returns the C++ code for a translation unit.
  public mutating func cxxCode(_ source: CXXModule) -> TranslationUnitCode {
    self.isCoreLibrary = source.isCoreLibrary
    return TranslationUnitCode(
      headerCode: generateHeaderCode(source), sourceCode: generateSourceCode(source))
  }

  // MARK: File type specific logic

  /// The C++ code for `module` that needs to be present in the header file.
  private func generateHeaderCode(_ source: CXXModule) -> String {
    var target: String = ""

    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    target.write("#pragma once\n")

    // Emit include clauses.
    if source.isCoreLibrary {
      target.write("#include <variant>\n")
      target.write("#include <cstdint>\n")
      target.write("#include <cstdlib>\n")
    } else {
      target.write("#include \"ValCore.h\"\n")
    }

    // Create a namespace for the entire module.
    if source.isCoreLibrary {
      target.write("namespace Val {\n")
    } else {
      target.write("namespace \(cxx: source.name) {\n")
      target.write("using namespace Val;\n")
    }

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      writeInterface(topLevel: decl, into: &target)
    }

    target.write("\n}\n")

    // Add extra native code to the stdlib header.
    if source.isCoreLibrary {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.h")
      if let text = try? String(contentsOf: fileToInclude) {
        target.write(text)
      }
    }

    return formatter?(target) ?? target
  }

  /// Returns the C++ code for `source` that needs to be present in the source file.
  private func generateSourceCode(_ source: CXXModule) -> String {
    var target: String = ""

    // Emit include clauses.
    target.write("#include \"\(source.name).h\"\n")

    // Create a namespace for the entire module.
    if isCoreLibrary {
      target.write("namespace Val {\n")
    } else {
      target.write("namespace \(cxx: source.name) {\n")
    }

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      writeDefinition(topLevel: decl, into: &target)
      target.write("\n")
    }

    target.write("}\n")

    // Add extra native code to the core library source file.
    if source.isCoreLibrary {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.cpp")
      if let text = try? String(contentsOf: fileToInclude) {
        target.write(text)
      }
    }

    // Write a CXX `main` function if the module has an entry point.
    if source.entryPointBody != nil {
      target.write("int main()")
      write(stmt: source.entryPointBody!, into: &target)
      target.write("\n")
    }

    return formatter?(target) ?? target
  }

  // MARK: Declarations

  private func writeInterface(topLevel decl: CXXTopLevelDecl, into target: inout String) {
    switch type(of: decl).kind {
    case CXXFunctionDecl.self:
      writeSignature(function: decl as! CXXFunctionDecl, into: &target)
    case CXXClassDecl.self:
      // We write the class definition in the header file.
      writeDefinition(type: decl as! CXXClassDecl, into: &target)
    case CXXComment.self:
      write(comment: decl as! CXXComment, into: &target)
    default:
      fatalError("unexpected top-level declaration")
    }
    target.write(";\n")
  }

  private func writeDefinition(topLevel decl: CXXTopLevelDecl, into target: inout String) {
    switch type(of: decl).kind {
    case CXXFunctionDecl.self:
      writeDefinition(function: decl as! CXXFunctionDecl, into: &target)
    case CXXClassDecl.self:
      // TODO: write implementation of methods
      break
    case CXXComment.self:
      write(comment: decl as! CXXComment, into: &target)
    default:
      fatalError("unexpected top-level declaration")
    }
  }

  private func writeSignature(function decl: CXXFunctionDecl, into target: inout String) {
    write(typeExpr: decl.output, into: &target)
    target.write(" ")
    target.write(decl.identifier)
    target.write("(")
    for i in 0 ..< decl.parameters.count {
      if i != 0 { target.write(", ") }
      write(typeExpr: decl.parameters[i].type, into: &target)
      target.write(" ")
      target.write(decl.parameters[i].name)
    }
    target.write(")")
  }
  private func writeDefinition(function decl: CXXFunctionDecl, into target: inout String) {
    writeSignature(function: decl, into: &target)
    if decl.body != nil {
      target.write(" ")
      write(stmt: decl.body!, into: &target)
    } else {
      target.write(";\n")
    }
  }

  private func writeSignature(type decl: CXXClassDecl, into target: inout String) {
    target.write("class ")
    target.write(decl.name.description)
  }
  private func writeDefinition(type decl: CXXClassDecl, into target: inout String) {
    writeSignature(type: decl, into: &target)
    target.write(" {\npublic:\n")
    for member in decl.members {
      switch member {
      case .attribute(let attribute):
        write(classAttribute: attribute, into: &target)
      case .method:
        target.write("// method\n")
      case .constructor:
        target.write("// constructor\n")
      }
    }

    // Special code for core library types
    if isCoreLibrary {
      // Try to generate implict conversion constructors from C++ literal types.
      write(conversionCtor: decl, into: &target)
    }

    target.write("};\n")
  }

  /// Writes to `target` the implicit conversion constructor for `source`, coverting from inner
  /// attribute type.
  ///
  /// This only applies for classes have one data member, and its type is native.
  private func write(conversionCtor source: CXXClassDecl, into target: inout String) {
    let dataMembers = source.members.compactMap({ m in
      switch m {
      case .attribute(let dataMember):
        return dataMember
      default:
        return nil
      }
    })

    // We need to have just one class attribute in the type,
    // and the type of the attribute needs to be native.
    if dataMembers.count == 1 && dataMembers[0].type.isNative {
      // Write implicit conversion constructor
      target.write("\(source.name)(")
      write(typeExpr: dataMembers[0].type, into: &target)
      target.write(" v) : ")
      target.write(dataMembers[0].name)
      target.write("(v) {}\n")
    }
  }

  private func write(classAttribute decl: CXXClassAttribute, into target: inout String) {
    write(typeExpr: decl.type, into: &target)
    target.write(" ")
    target.write(decl.name)
    if let value = decl.initializer {
      target.write(" = ")
      write(expr: value, into: &target)
    }
    target.write(";\n")
  }

  private func write(localVar decl: CXXLocalVarDecl, into target: inout String) {
    write(typeExpr: decl.type, into: &target)
    target.write(" ")
    target.write(decl.name)
    if let value = decl.initializer {
      target.write(" = ")
      write(expr: value, into: &target)
    }
    target.write(";\n")
  }

  // MARK: Statements

  private func write(stmt: CXXStmt, into target: inout String) {
    switch type(of: stmt).kind {
    case CXXScopedBlock.self:
      write(scopedBlock: stmt as! CXXScopedBlock, into: &target)
    case CXXLocalVarDecl.self:
      write(localVar: stmt as! CXXLocalVarDecl, into: &target)
    case CXXExprStmt.self:
      write(exprStmt: stmt as! CXXExprStmt, into: &target)
    case CXXReturnStmt.self:
      write(returnStmt: stmt as! CXXReturnStmt, into: &target)
    case CXXIfStmt.self:
      write(ifStmt: stmt as! CXXIfStmt, into: &target)
    case CXXWhileStmt.self:
      write(whileStmt: stmt as! CXXWhileStmt, into: &target)
    case CXXDoWhileStmt.self:
      write(doWhileStmt: stmt as! CXXDoWhileStmt, into: &target)
    case CXXBreakStmt.self:
      write(breakStmt: stmt as! CXXBreakStmt, into: &target)
    case CXXContinueStmt.self:
      write(continueStmt: stmt as! CXXContinueStmt, into: &target)
    case CXXComment.self:
      write(comment: stmt as! CXXComment, into: &target)
    default:
      fatalError("unexpected statement")
    }
  }

  private func write(scopedBlock stmt: CXXScopedBlock, into target: inout String) {
    target.write("{\n")
    for s in stmt.stmts {
      write(stmt: s, into: &target)
    }
    target.write("}\n")
  }
  private func write(exprStmt stmt: CXXExprStmt, into target: inout String) {
    write(expr: stmt.expr, into: &target)
    target.write(";\n")
  }
  private func write(returnStmt stmt: CXXReturnStmt, into target: inout String) {
    target.write("return")
    if stmt.expr != nil {
      target.write(" ")
      write(expr: stmt.expr!, into: &target)
    }
    target.write(";\n")
  }
  private func write(ifStmt stmt: CXXIfStmt, into target: inout String) {
    target.write("if ( ")
    write(expr: stmt.condition, into: &target)
    target.write(" ) ")
    write(stmt: stmt.trueStmt, into: &target)
    if stmt.falseStmt != nil {
      target.write("else ")
      write(stmt: stmt.falseStmt!, into: &target)
    }
  }
  private func write(whileStmt stmt: CXXWhileStmt, into target: inout String) {
    target.write("while ( ")
    write(expr: stmt.condition, into: &target)
    target.write(" ) ")
    write(stmt: stmt.body, into: &target)
  }
  private func write(doWhileStmt stmt: CXXDoWhileStmt, into target: inout String) {
    target.write("do ")
    write(stmt: stmt.body, into: &target)
    target.write("while ( ")
    write(expr: stmt.condition, into: &target)
    target.write(" );\n")
  }
  private func write(breakStmt stmt: CXXBreakStmt, into target: inout String) {
    target.write("break;\n")
  }
  private func write(continueStmt stmt: CXXContinueStmt, into target: inout String) {
    target.write("continue;\n")
  }

  // MARK: Expressions

  private func write(expr: CXXExpr, into target: inout String) {
    switch type(of: expr).kind {
    case CXXBooleanLiteralExpr.self:
      write(booleanLiteralExpr: expr as! CXXBooleanLiteralExpr, into: &target)
    case CXXIntegerLiteralExpr.self:
      write(integerLiteralExpr: expr as! CXXIntegerLiteralExpr, into: &target)
    case CXXIdentifier.self:
      target.write(expr as! CXXIdentifier)
    case CXXReceiverExpr.self:
      write(receiverExpr: expr as! CXXReceiverExpr, into: &target)
    case CXXTypeExpr.self:
      write(typeExpr: expr as! CXXTypeExpr, into: &target)
    case CXXInfixExpr.self:
      write(infixExpr: expr as! CXXInfixExpr, into: &target)
    case CXXPrefixExpr.self:
      write(prefixExpr: expr as! CXXPrefixExpr, into: &target)
    case CXXPostfixExpr.self:
      write(postfixExpr: expr as! CXXPostfixExpr, into: &target)
    case CXXFunctionCallExpr.self:
      write(functionCallExpr: expr as! CXXFunctionCallExpr, into: &target)
    case CXXVoidCast.self:
      write(voidCast: expr as! CXXVoidCast, into: &target)
    case CXXConditionalExpr.self:
      write(conditionalExpr: expr as! CXXConditionalExpr, into: &target)
    case CXXStmtExpr.self:
      write(stmtExpr: expr as! CXXStmtExpr, into: &target)
    case CXXComment.self:
      write(comment: expr as! CXXComment, into: &target)
    default:
      fatalError("unexpected expressions")
    }
  }

  private func write(
    booleanLiteralExpr expr: CXXBooleanLiteralExpr, into target: inout String
  ) {
    target.write(expr.value ? "true" : "false")
  }

  private func write(
    integerLiteralExpr expr: CXXIntegerLiteralExpr, into target: inout String
  ) {
    target.write(expr.value)
  }

  private func write(receiverExpr expr: CXXReceiverExpr, into target: inout String) {
    target.write("this")
  }

  private func write(typeExpr expr: CXXTypeExpr, into target: inout String) {
    target.write(expr.text)
  }

  private func write(infixExpr expr: CXXInfixExpr, into target: inout String) {
    // TODO: handle precedence and associativity; as of writing this comment, infix operators cannot be properly tested.
    write(expr: expr.lhs, into: &target)
    switch expr.oper {
    case .scopeResolution: target.write(" :: ")
    case .dotAccess: target.write(" . ")
    case .ptrAccess: target.write(" -> ")
    case .dotPtrToMember: target.write(" .* ")
    case .ptrToMember: target.write(" ->* ")
    case .multiplication: target.write(" * ")
    case .division: target.write(" / ")
    case .remainder: target.write(" % ")
    case .addition: target.write(" + ")
    case .subtraction: target.write(" - ")
    case .leftShift: target.write(" << ")
    case .rightShift: target.write(" >> ")
    case .spaceship: target.write(" <=> ")
    case .lessThan: target.write(" < ")
    case .lessEqual: target.write(" <= ")
    case .greaterThan: target.write(" > ")
    case .greaterEqual: target.write(" >= ")
    case .equality: target.write(" == ")
    case .inequality: target.write(" == ")
    case .bitwiseAnd: target.write(" & ")
    case .bitwiseXor: target.write(" ^ ")
    case .bitwiseOr: target.write(" | ")
    case .logicalAnd: target.write(" && ")
    case .logicalOr: target.write(" || ")
    case .assignment: target.write(" = ")
    case .addAssignment: target.write(" += ")
    case .subAssignment: target.write(" -= ")
    case .mulAssignment: target.write(" *= ")
    case .divAssignment: target.write(" /= ")
    case .remAssignment: target.write(" %= ")
    case .shiftLeftAssignment: target.write(" <<= ")
    case .shiftRightAssignment: target.write(" >>= ")
    case .bitwiseAndAssignment: target.write(" &= ")
    case .bitwiseXorAssignment: target.write(" ^= ")
    case .bitwiseOrAssignment: target.write(" |= ")
    case .comma: target.write(" , ")
    }
    write(expr: expr.rhs, into: &target)
  }
  private func write(prefixExpr expr: CXXPrefixExpr, into target: inout String) {
    // TODO: handle precedence and associativity; as of writing this comment, prefix operators cannot be properly tested.
    switch expr.oper {
    case .prefixIncrement: target.write("++")
    case .prefixDecrement: target.write("--")
    case .unaryPlus: target.write("+")
    case .unaryMinus: target.write("-")
    case .logicalNot: target.write("!")
    case .bitwiseNot: target.write("~")
    case .dereference: target.write("*")
    case .addressOf: target.write("&")
    case .sizeOf: target.write("sizeof ")
    case .coAwait: target.write("co_await ")
    case .throwOp: target.write("throw ")
    case .coYield: target.write("co_yield ")
    }
    write(expr: expr.base, into: &target)
  }
  private func write(postfixExpr expr: CXXPostfixExpr, into target: inout String) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    write(expr: expr.base, into: &target)
    switch expr.oper {
    case .suffixIncrement: target.write("++")
    case .suffixDecrement: target.write("--")
    }
  }
  private func write(functionCallExpr expr: CXXFunctionCallExpr, into target: inout String) {
    write(expr: expr.callee, into: &target)
    target.write("(")
    for (i, argument) in expr.arguments.enumerated() {
      if i > 0 {
        target.write(", ")
      }
      write(expr: argument, into: &target)
    }
    target.write(")")
  }
  private func write(voidCast expr: CXXVoidCast, into target: inout String) {
    target.write("(void) ")
    write(expr: expr.baseExpr, into: &target)
  }
  private func write(conditionalExpr expr: CXXConditionalExpr, into target: inout String) {
    write(expr: expr.condition, into: &target)
    target.write(" ? ")
    write(expr: expr.trueExpr, into: &target)
    target.write(" : ")
    write(expr: expr.falseExpr, into: &target)
  }
  private func write(stmtExpr expr: CXXStmtExpr, into target: inout String) {
    write(stmt: expr.stmt, into: &target)
  }

  // MARK: Miscellaneous

  private func write(comment c: CXXComment, into target: inout String) {
    if c.comment.contains("\n") {
      target.write("/* \(c.comment) */")
    } else {
      target.write("// \(c.comment)\n")
    }
  }
}

extension String {

  /// Adds `n` to the output code.
  mutating func write(_ n: CXXIdentifier) {
    write(n.description)
  }

}
