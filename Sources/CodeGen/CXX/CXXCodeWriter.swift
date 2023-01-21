/// Class used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init() {}

  // MARK: API

  /// Write the CXX header content for the given module to the given text stream.
  public func emitHeaderCode(_ module: CXXModule) -> String {
    var target: String = ""

    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    target.write("#pragma once\n")
    target.write("\n")

    // Emit include clauses.
    target.write("#include <variant>\n")
    target.write("\n")

    // Create a namespace for the entire module.
    target.write("namespace \(module.valDecl.name) {\n\n")

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in module.cxxTopLevelDecls {
      writeInterface(topLevel: decl, into: &target)
    }

    target.write("\n}\n")  // module namespace

    return target
  }

  /// Write the CXX source content for the given module to the given text stream.
  public func emitSourceCode(_ module: CXXModule) -> String {
    var target: String = ""

    // Emit include clauses.
    target.write("#include \"\(module.valDecl.name).h\"\n")
    target.write("\n")

    // Create a namespace for the entire module.
    target.write("namespace \(module.valDecl.name) {\n\n")

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in module.cxxTopLevelDecls {
      writeDefinition(topLevel: decl, into: &target)
    }

    target.write("\n}\n")  // module namespace

    return target
  }

  // MARK: Declarations

  private func writeInterface(topLevel decl: CXXTopLevelDecl, into target: inout String) {
    switch type(of: decl).kind {
    case CXXFunctionDecl.self:
      writeSignature(function: decl as! CXXFunctionDecl, into: &target)
    case CXXClassDecl.self:
      writeSignature(type: decl as! CXXClassDecl, into: &target)
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
      writeDefinition(type: decl as! CXXClassDecl, into: &target)
    default:
      fatalError("unexpected top-level declaration")
    }
  }

  private func writeSignature(function decl: CXXFunctionDecl, into target: inout String) {
    write(typeExpr: decl.output, into: &target)
    target.write(" ")
    write(identifier: decl.identifier, into: &target)
    target.write("(")
    for i in 0 ..< decl.parameters.count {
      if i != 0 { target.write(", ") }
      write(typeExpr: decl.parameters[i].type, into: &target)
      target.write(" ")
      write(identifier: decl.parameters[i].name, into: &target)
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
    write(identifier: decl.name, into: &target)
  }
  private func writeDefinition(type decl: CXXClassDecl, into target: inout String) {
    writeSignature(type: decl, into: &target)
    target.write(" {\n")
    target.write("public:\n")
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
    target.write("};\n")
  }

  private func write(classAttribute decl: CXXClassAttribute, into target: inout String) {
    write(typeExpr: decl.type, into: &target)
    target.write(" ")
    write(identifier: decl.name, into: &target)
    if let value = decl.initializer {
      target.write(" = ")
      write(expr: value, into: &target)
    }
    target.write(";\n")
  }

  private func write(localVar decl: CXXLocalVarDecl, into target: inout String) {
    write(typeExpr: decl.type, into: &target)
    target.write(" ")
    write(identifier: decl.name, into: &target)
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
    if stmt.expr != nil {
      target.write("return ")
      write(expr: stmt.expr!, into: &target)
      target.write(";\n")
    } else {
      target.write("return;\n")
    }
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
      write(identifier: expr as! CXXIdentifier, into: &target)
    case CXXReceiverExpr.self:
      write(receiverExpr: expr as! CXXReceiverExpr, into: &target)
    case CXXTypeExpr.self:
      write(typeExpr: expr as! CXXTypeExpr, into: &target)
    case CXXCompoundExpr.self:
      write(compoundExpr: expr as! CXXCompoundExpr, into: &target)
    case CXXInfixExpr.self:
      write(infixExpr: expr as! CXXInfixExpr, into: &target)
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

  private func write(booleanLiteralExpr expr: CXXBooleanLiteralExpr, into target: inout String) {
    target.write(expr.value ? "true" : "false")
  }
  private func write(integerLiteralExpr expr: CXXIntegerLiteralExpr, into target: inout String) {
    target.write(expr.value)
  }
  private func write(identifier expr: CXXIdentifier, into target: inout String) {
    target.write(expr.description)
  }
  private func write(receiverExpr expr: CXXReceiverExpr, into target: inout String) {
    target.write("this")
  }
  private func write(typeExpr expr: CXXTypeExpr, into target: inout String) {
    target.write(expr.description)
  }
  private func write(compoundExpr expr: CXXCompoundExpr, into target: inout String) {
    write(expr: expr.base, into: &target)
    target.write(".")
    write(expr: expr.id, into: &target)
  }
  private func write(infixExpr expr: CXXInfixExpr, into target: inout String) {
    write(expr: expr.lhs, into: &target)
    target.write(" ")
    write(expr: expr.callee, into: &target)
    target.write(" ")
    write(expr: expr.rhs, into: &target)
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
