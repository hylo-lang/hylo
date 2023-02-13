import ValModule

/// A type used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init(formatter: CXXCodeFormatter) {
    self.formatter = formatter
  }

  /// Object used to format output C++ code.
  private let formatter: CXXCodeFormatter

  /// Indicates if we are currently writing the standard library module.
  private var isStdLib: Bool = false

  // MARK: API

  /// Returns the C++ code for a translation unit.
  public mutating func cxxCode(_ source: CXXModule) -> TranslationUnitCode {
    self.isStdLib = source.isStdLib
    return TranslationUnitCode(
      headerCode: generateHeaderCode(source), sourceCode: generateSourceCode(source))
  }

  // MARK: File type specific logic

  /// The C++ code for `module` that needs to be present in the header file.
  private func generateHeaderCode(_ source: CXXModule) -> String {
    var target = CodeFormatter()

    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    target.writeLine("#pragma once")
    target.writeNewline()

    // Emit include clauses.
    if source.isStdLib {
      target.writeLine("#include <variant>")
      target.writeLine("#include <cstdint>")
      target.writeLine("#include <cstdlib>")
    } else {
      target.writeLine("#include \"ValStdLib.h\"")
    }
    target.writeNewline()

    // Create a namespace for the entire module.
    target.write("namespace \(source.name)")
    target.beginBrace()
    target.writeNewline()

    // If we are not in the standard library, use the namespace corresponding to the standard lib.
    if !source.isStdLib {
      target.writeLine("using namespace ValStdLib;")
      target.writeNewline()
    }

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      writeInterface(topLevel: decl, into: &target)
    }

    target.writeNewline()
    target.endBrace()
    target.writeNewline()

    // Add extra native code to the stdlib header.
    if source.isStdLib {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.h")
      if let text = try? String(contentsOf: fileToInclude) {
        target.write(text)
      }
    }

    return formatter.format(target.code)
  }

  /// Returns the C++ code for `source` that needs to be present in the source file.
  private func generateSourceCode(_ source: CXXModule) -> String {
    var target = CodeFormatter()

    // Emit include clauses.
    target.writeLine("#include \"\(source.name).h\"")
    target.writeNewline()

    // Create a namespace for the entire module.
    target.write("namespace \(source.name)")
    target.beginBrace()
    target.writeNewline()

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      writeDefinition(topLevel: decl, into: &target)
      target.writeNewline()
    }

    target.endBrace()
    target.writeNewline()

    // Add extra native code to the stdlib source file.
    if source.isStdLib {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.cpp")
      if let text = try? String(contentsOf: fileToInclude) {
        target.write(text)
      }
    }

    // Write a CXX `main` function if the module has an entry point.
    if source.entryPointBody != nil {
      target.writeNewline()
      target.write("int main()")
      write(stmt: source.entryPointBody!, into: &target)
      target.writeNewline()
    }

    return formatter.format(target.code)
  }

  // MARK: Declarations

  private func writeInterface(topLevel decl: CXXTopLevelDecl, into target: inout CodeFormatter) {
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
    target.writeLine(";")
  }

  private func writeDefinition(topLevel decl: CXXTopLevelDecl, into target: inout CodeFormatter) {
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

  private func writeSignature(function decl: CXXFunctionDecl, into target: inout CodeFormatter) {
    write(typeExpr: decl.output, into: &target)
    target.writeSpace()
    write(identifier: decl.identifier, into: &target)
    target.write("(")
    for i in 0 ..< decl.parameters.count {
      if i != 0 { target.write(", ") }
      write(typeExpr: decl.parameters[i].type, into: &target)
      target.writeSpace()
      write(identifier: decl.parameters[i].name, into: &target)
    }
    target.write(")")
  }
  private func writeDefinition(function decl: CXXFunctionDecl, into target: inout CodeFormatter) {
    writeSignature(function: decl, into: &target)
    if decl.body != nil {
      target.writeSpace()
      write(stmt: decl.body!, into: &target)
    } else {
      target.writeLine(";")
    }
  }

  private func writeSignature(type decl: CXXClassDecl, into target: inout CodeFormatter) {
    target.write("class ")
    write(identifier: decl.name, into: &target)
  }
  private func writeDefinition(type decl: CXXClassDecl, into target: inout CodeFormatter) {
    writeSignature(type: decl, into: &target)
    target.beginBrace()
    target.writeLine("public:")
    for member in decl.members {
      switch member {
      case .attribute(let attribute):
        write(classAttribute: attribute, into: &target)
      case .method:
        target.writeLine("// method")
      case .constructor:
        target.writeLine("// constructor")
      }
    }
    // Special code for stdlib types
    if isStdLib {
      // For standard value types try to generate implict conversion constructors from C++ literal types.
      write(conversionCtor: decl, into: &target)
    }
    target.endBrace()
    target.writeLine(";")
  }

  /// Writes to `target` the implicit conversion constructor for `source`, coverting from inner
  /// attribute type.
  ///
  /// This only applies for classes have one data member, and its type is native.
  private func write(conversionCtor source: CXXClassDecl, into target: inout CodeFormatter) {
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
      target.write("\(source.name.description)(")
      write(typeExpr: dataMembers[0].type, into: &target)
      target.write(" v) : ")
      write(identifier: dataMembers[0].name, into: &target)
      target.writeLine("(v) {}")
    }
  }

  private func write(classAttribute decl: CXXClassAttribute, into target: inout CodeFormatter) {
    write(typeExpr: decl.type, into: &target)
    target.writeSpace()
    write(identifier: decl.name, into: &target)
    if let value = decl.initializer {
      target.write(" = ")
      write(expr: value, into: &target)
    }
    target.writeLine(";")
  }

  private func write(localVar decl: CXXLocalVarDecl, into target: inout CodeFormatter) {
    write(typeExpr: decl.type, into: &target)
    target.writeSpace()
    write(identifier: decl.name, into: &target)
    if let value = decl.initializer {
      target.write(" = ")
      write(expr: value, into: &target)
    }
    target.writeLine(";")
  }

  // MARK: Statements

  private func write(stmt: CXXStmt, into target: inout CodeFormatter) {
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

  private func write(scopedBlock stmt: CXXScopedBlock, into target: inout CodeFormatter) {
    target.beginBrace()
    for s in stmt.stmts {
      write(stmt: s, into: &target)
    }
    target.endBrace()
    target.writeNewline()
  }
  private func write(exprStmt stmt: CXXExprStmt, into target: inout CodeFormatter) {
    write(expr: stmt.expr, into: &target)
    target.writeLine(";")
  }
  private func write(returnStmt stmt: CXXReturnStmt, into target: inout CodeFormatter) {
    target.write("return")
    if stmt.expr != nil {
      target.writeSpace()
      write(expr: stmt.expr!, into: &target)
    }
    target.writeLine(";")
  }
  private func write(ifStmt stmt: CXXIfStmt, into target: inout CodeFormatter) {
    target.write("if ( ")
    write(expr: stmt.condition, into: &target)
    target.write(" ) ")
    write(stmt: stmt.trueStmt, into: &target)
    if stmt.falseStmt != nil {
      target.write("else ")
      write(stmt: stmt.falseStmt!, into: &target)
    }
  }
  private func write(whileStmt stmt: CXXWhileStmt, into target: inout CodeFormatter) {
    target.write("while ( ")
    write(expr: stmt.condition, into: &target)
    target.write(" ) ")
    write(stmt: stmt.body, into: &target)
  }
  private func write(doWhileStmt stmt: CXXDoWhileStmt, into target: inout CodeFormatter) {
    target.write("do ")
    write(stmt: stmt.body, into: &target)
    target.write("while ( ")
    write(expr: stmt.condition, into: &target)
    target.writeLine(" );")
  }
  private func write(breakStmt stmt: CXXBreakStmt, into target: inout CodeFormatter) {
    target.writeLine("break;")
  }
  private func write(continueStmt stmt: CXXContinueStmt, into target: inout CodeFormatter) {
    target.writeLine("continue;")
  }

  // MARK: Expressions

  private func write(expr: CXXExpr, into target: inout CodeFormatter) {
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
    booleanLiteralExpr expr: CXXBooleanLiteralExpr, into target: inout CodeFormatter
  ) {
    target.write(expr.value ? "true" : "false")
  }
  private func write(
    integerLiteralExpr expr: CXXIntegerLiteralExpr, into target: inout CodeFormatter
  ) {
    target.write(expr.value)
  }
  private func write(identifier expr: CXXIdentifier, into target: inout CodeFormatter) {
    target.write(expr.description)
  }
  private func write(receiverExpr expr: CXXReceiverExpr, into target: inout CodeFormatter) {
    target.write("this")
  }
  private func write(typeExpr expr: CXXTypeExpr, into target: inout CodeFormatter) {
    target.write(expr.text)
  }
  private func write(infixExpr expr: CXXInfixExpr, into target: inout CodeFormatter) {
    // TODO: handle precedence and associativity; as of writing this comment, infix operators cannot be properly tested.
    write(expr: expr.lhs, into: &target)
    target.writeSpace()
    switch expr.oper {
    case .scopeResolution: target.write("::")
    case .dotAccess: target.write(".")
    case .ptrAccess: target.write("->")
    case .dotPtrToMember: target.write(".*")
    case .ptrToMember: target.write("->*")
    case .multiplication: target.write("*")
    case .division: target.write("/")
    case .remainder: target.write("%")
    case .addition: target.write("+")
    case .subtraction: target.write("-")
    case .leftShift: target.write("<<")
    case .rightShift: target.write(">>")
    case .spaceship: target.write("<=>")
    case .lessThan: target.write("<")
    case .lessEqual: target.write("<=")
    case .greaterThan: target.write(">")
    case .greaterEqual: target.write(">=")
    case .equality: target.write("==")
    case .inequality: target.write("==")
    case .bitwiseAnd: target.write("&")
    case .bitwiseXor: target.write("^")
    case .bitwiseOr: target.write("|")
    case .logicalAnd: target.write("&&")
    case .logicalOr: target.write("||")
    case .assignment: target.write("=")
    case .addAssignment: target.write("+=")
    case .subAssignment: target.write("-=")
    case .mulAssignment: target.write("*=")
    case .divAssignment: target.write("/=")
    case .remAssignment: target.write("%=")
    case .shiftLeftAssignment: target.write("<<=")
    case .shiftRightAssignment: target.write(">>=")
    case .bitwiseAndAssignment: target.write("&=")
    case .bitwiseXorAssignment: target.write("^=")
    case .bitwiseOrAssignment: target.write("|=")
    case .comma: target.write(",")
    }
    target.writeSpace()
    write(expr: expr.rhs, into: &target)
  }
  private func write(prefixExpr expr: CXXPrefixExpr, into target: inout CodeFormatter) {
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
  private func write(postfixExpr expr: CXXPostfixExpr, into target: inout CodeFormatter) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    write(expr: expr.base, into: &target)
    switch expr.oper {
    case .suffixIncrement: target.write("++")
    case .suffixDecrement: target.write("--")
    }
  }
  private func write(functionCallExpr expr: CXXFunctionCallExpr, into target: inout CodeFormatter) {
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
  private func write(voidCast expr: CXXVoidCast, into target: inout CodeFormatter) {
    target.write("(void) ")
    write(expr: expr.baseExpr, into: &target)
  }
  private func write(conditionalExpr expr: CXXConditionalExpr, into target: inout CodeFormatter) {
    write(expr: expr.condition, into: &target)
    target.write(" ? ")
    write(expr: expr.trueExpr, into: &target)
    target.write(" : ")
    write(expr: expr.falseExpr, into: &target)
  }
  private func write(stmtExpr expr: CXXStmtExpr, into target: inout CodeFormatter) {
    write(stmt: expr.stmt, into: &target)
  }

  // MARK: Miscellaneous

  private func write(comment c: CXXComment, into target: inout CodeFormatter) {
    if c.comment.contains("\n") {
      target.write("/* \(c.comment) */")
    } else {
      target.writeLine("// \(c.comment)")
    }
  }
}
