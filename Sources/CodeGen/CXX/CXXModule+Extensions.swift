import ValModule

extension CXXModule {

  /// Returns the C++ code for `self`.
  public func code(withFormatter formatter: @escaping CodeTransform = identity())
    -> TranslationUnitCode
  {
    let c = WriteContext(isStdLib: self.isStdLib)
    return TranslationUnitCode(
      headerCode: formatter(HeaderFile(self).code(inContext: c)),
      sourceCode: formatter(SourceFile(self).code(inContext: c)))
  }

}

// MARK: Files

/// Knows how to write the content of C++ header file for a `CXXModule`.
private struct HeaderFile: WriteableInContext {

  let source: CXXModule

  init(_ source: CXXModule) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    output.write("#pragma once\n")

    // Emit include clauses.
    if source.isStdLib {
      output.write("#include <variant>\n")
      output.write("#include <cstdint>\n")
      output.write("#include <cstdlib>\n")
    } else {
      output.write("#include \"ValStdLib.h\"\n")
    }

    // Create a namespace for the entire module.
    output.write("namespace \(source.name) {\n")

    // If we are not in the standard library, use the namespace corresponding to the standard lib.
    if !source.isStdLib {
      output.write("using namespace ValStdLib;\n")
    }

    // Emit the C++ text needed for the header corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      output.write(TopLevelInterface(decl), inContext: c)
    }

    output.write("\n}\n")

    // Add extra native code to the stdlib header.
    if source.isStdLib {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.h")
      if let text = try? String(contentsOf: fileToInclude) {
        output.write(text)
      }
    }
  }

}

/// Knows how to write the content of C++ source file for a `CXXModule`.
private struct SourceFile: WriteableInContext {

  let source: CXXModule

  init(_ source: CXXModule) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    // Emit include clauses.
    output.write("#include \"\(source.name).h\"\n")

    // Create a namespace for the entire module.
    output.write("namespace \(source.name) {\n")

    // Emit the C++ text needed for the source file corresponding to the C++ declarations.
    for decl in source.topLevelDecls {
      output.write(TopLevelDefinition(decl), inContext: c)
      output.write("\n")
    }

    output.write("}\n")

    // Add extra native code to the stdlib source file.
    if source.isStdLib {
      let fileToInclude = ValModule.cxxSupport!.appendingPathComponent("NativeCode.cpp")
      if let text = try? String(contentsOf: fileToInclude) {
        output.write(text)
      }
    }

    // Write a CXX `main` function if the module has an entry point.
    if source.entryPointBody != nil {
      output.write("int main()")
      output.write(StmtWriteable(source.entryPointBody!), inContext: c)
      output.write("\n")
    }
  }

}

// MARK: Declarations

/// Knows how to write the interface for a top level declaration.
private struct TopLevelInterface: WriteableInContext {

  let source: CXXTopLevelDecl

  init(_ source: CXXTopLevelDecl) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    switch type(of: source).kind {
    case CXXFunctionDecl.self:
      output.write(FunctionSignature(source as! CXXFunctionDecl), inContext: c)
    case CXXClassDecl.self:
      // We write the class definition in the header file.
      output.write(ClassDefinition(source as! CXXClassDecl), inContext: c)
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected top-level declaration")
    }
    output.write(";\n")
  }

}

/// Knows how to write the definition for a top level declaration.
private struct TopLevelDefinition: WriteableInContext {

  let source: CXXTopLevelDecl

  init(_ source: CXXTopLevelDecl) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    switch type(of: source).kind {
    case CXXFunctionDecl.self:
      output.write(FunctionDefinition(source as! CXXFunctionDecl), inContext: c)
    case CXXClassDecl.self:
      // TODO: write implementation of methods
      break
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected top-level declaration")
    }
  }

}

/// Knows how to write a function signature.
private struct FunctionSignature: WriteableInContext {

  let source: CXXFunctionDecl

  init(_ source: CXXFunctionDecl) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(source.output)
    output.write(" ")
    output.write(source.identifier)
    output.write("(")
    for i in 0 ..< source.parameters.count {
      if i != 0 { output.write(", ") }
      output.write(source.parameters[i].type)
      output.write(" ")
      output.write(source.parameters[i].name)
    }
    output.write(")")
  }

}

/// Knows how to write a function definition.
private struct FunctionDefinition: WriteableInContext {

  let source: CXXFunctionDecl

  init(_ source: CXXFunctionDecl) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(FunctionSignature(source), inContext: c)
    if source.body != nil {
      output.write(" ")
      output.write(StmtWriteable(source.body!), inContext: c)
    } else {
      output.write(";\n")
    }
  }

}

/// Knows how to write a class signature.
private struct ClassDefinition: WriteableInContext {

  let source: CXXClassDecl

  init(_ source: CXXClassDecl) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("class ")
    output.write(source.name)
    output.write(" {\npublic:\n")
    for member in source.members {
      switch member {
      case .attribute(let attribute):
        output.write(attribute, inContext: c)
      case .method:
        output.write("// method\n")
      case .constructor:
        output.write("// constructor\n")
      }
    }
    // Special code for stdlib types
    if c.isStdLib {
      // For standard value types try to generate implict conversion constructors from C++ literal types.
      output.write(ConversionConstructor(source), inContext: c)
    }
    output.write("};\n")
  }
}

/// Knows how to write a conversion constructor for a class.
///
/// This only applies for classes have one data member, and its type is native.
private struct ConversionConstructor: WriteableInContext {

  let parentClass: CXXClassDecl

  init(_ parentClass: CXXClassDecl) {
    self.parentClass = parentClass
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    let dataMembers = parentClass.members.compactMap({ m in
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
      output.write(parentClass.name.description)
      output.write("(")
      output.write(dataMembers[0].type)
      output.write(" v) : ")
      output.write(dataMembers[0].name)
      output.write("(v) {}\n")
    }
  }
}

extension CXXClassAttribute: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write([type, " ", name])
    if let value = initializer {
      output.write(" = ")
      output.write(ExprWriteable(value), inContext: c)
    }
    output.write(";\n")
  }

}

extension CXXLocalVarDecl: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write([type, " ", name])
    if let value = initializer {
      output.write(" = ")
      output.write(ExprWriteable(value), inContext: c)
    }
    output.write(";\n")
  }

}

// MARK: Statements

private struct StmtWriteable: WriteableInContext {
  let source: CXXStmt

  init(_ source: CXXStmt) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    switch type(of: source).kind {
    case CXXScopedBlock.self:
      output.write(source as! CXXScopedBlock, inContext: c)
    case CXXLocalVarDecl.self:
      output.write(source as! CXXLocalVarDecl, inContext: c)
    case CXXExprStmt.self:
      output.write(source as! CXXExprStmt, inContext: c)
    case CXXReturnStmt.self:
      output.write(source as! CXXReturnStmt, inContext: c)
    case CXXIfStmt.self:
      output.write(source as! CXXIfStmt, inContext: c)
    case CXXWhileStmt.self:
      output.write(source as! CXXWhileStmt, inContext: c)
    case CXXDoWhileStmt.self:
      output.write(source as! CXXDoWhileStmt, inContext: c)
    case CXXBreakStmt.self:
      output.write(source as! CXXBreakStmt)
    case CXXContinueStmt.self:
      output.write(source as! CXXContinueStmt)
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected statement")
    }
  }
}

extension CXXScopedBlock: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("{\n")
    for s in stmts {
      output.write(StmtWriteable(s), inContext: c)
    }
    output.write("}\n")
  }

}

extension CXXExprStmt: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(ExprWriteable(expr), inContext: c)
    output.write(";\n")
  }

}

extension CXXReturnStmt: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("return")
    if expr != nil {
      output.write(" ")
      output.write(ExprWriteable(expr!), inContext: c)
    }
    output.write(";\n")
  }

}

extension CXXIfStmt: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("if ( ")
    output.write(ExprWriteable(condition), inContext: c)
    output.write(" ) ")
    output.write(StmtWriteable(trueStmt), inContext: c)
    if falseStmt != nil {
      output.write("else ")
      output.write(StmtWriteable(falseStmt!), inContext: c)
    }
  }

}

extension CXXWhileStmt: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("while ( ")
    output.write(ExprWriteable(condition), inContext: c)
    output.write(" ) ")
    output.write(StmtWriteable(body), inContext: c)
  }

}

extension CXXDoWhileStmt: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("do ")
    output.write(StmtWriteable(body), inContext: c)
    output.write("while ( ")
    output.write(ExprWriteable(condition), inContext: c)
    output.write(" );\n")
  }

}

extension CXXBreakStmt: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write("break;\n")
  }

}

extension CXXContinueStmt: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write("continue;\n")
  }

}

// MARK: Expressions

private struct ExprWriteable: WriteableInContext {

  let source: CXXExpr

  init(_ source: CXXExpr) {
    self.source = source
  }

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    switch type(of: source).kind {
    case CXXBooleanLiteralExpr.self:
      output.write(source as! CXXBooleanLiteralExpr)
    case CXXIntegerLiteralExpr.self:
      output.write(source as! CXXIntegerLiteralExpr)
    case CXXIdentifier.self:
      output.write(source as! CXXIdentifier)
    case CXXReceiverExpr.self:
      output.write(source as! CXXReceiverExpr)
    case CXXTypeExpr.self:
      output.write(source as! CXXTypeExpr)
    case CXXInfixExpr.self:
      output.write(source as! CXXInfixExpr, inContext: c)
    case CXXPrefixExpr.self:
      output.write(source as! CXXPrefixExpr, inContext: c)
    case CXXPostfixExpr.self:
      output.write(source as! CXXPostfixExpr, inContext: c)
    case CXXFunctionCallExpr.self:
      output.write(source as! CXXFunctionCallExpr, inContext: c)
    case CXXVoidCast.self:
      output.write(source as! CXXVoidCast, inContext: c)
    case CXXConditionalExpr.self:
      output.write(source as! CXXConditionalExpr, inContext: c)
    case CXXStmtExpr.self:
      output.write(source as! CXXStmtExpr, inContext: c)
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected expressions")
    }
  }

}

extension CXXBooleanLiteralExpr: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(value ? "true" : "false")
  }

}
extension CXXIntegerLiteralExpr: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(value)
  }

}
extension CXXIdentifier: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(description)
  }

}
extension CXXReceiverExpr: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write("this")
  }

}
extension CXXTypeExpr: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    output.write(text)
  }

}
extension CXXInfixExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    // TODO: handle precedence and associativity; as of writing this comment, infix operators cannot be properly tested.
    output.write(ExprWriteable(lhs), inContext: c)
    switch oper {
    case .scopeResolution: output.write(" :: ")
    case .dotAccess: output.write(" . ")
    case .ptrAccess: output.write(" -> ")
    case .dotPtrToMember: output.write(" .* ")
    case .ptrToMember: output.write(" ->* ")
    case .multiplication: output.write(" * ")
    case .division: output.write(" / ")
    case .remainder: output.write(" % ")
    case .addition: output.write(" + ")
    case .subtraction: output.write(" - ")
    case .leftShift: output.write(" << ")
    case .rightShift: output.write(" >> ")
    case .spaceship: output.write(" <=> ")
    case .lessThan: output.write(" < ")
    case .lessEqual: output.write(" <= ")
    case .greaterThan: output.write(" > ")
    case .greaterEqual: output.write(" >= ")
    case .equality: output.write(" == ")
    case .inequality: output.write(" == ")
    case .bitwiseAnd: output.write(" & ")
    case .bitwiseXor: output.write(" ^ ")
    case .bitwiseOr: output.write(" | ")
    case .logicalAnd: output.write(" && ")
    case .logicalOr: output.write(" || ")
    case .assignment: output.write(" = ")
    case .addAssignment: output.write(" += ")
    case .subAssignment: output.write(" -= ")
    case .mulAssignment: output.write(" *= ")
    case .divAssignment: output.write(" /= ")
    case .remAssignment: output.write(" %= ")
    case .shiftLeftAssignment: output.write(" <<= ")
    case .shiftRightAssignment: output.write(" >>= ")
    case .bitwiseAndAssignment: output.write(" &= ")
    case .bitwiseXorAssignment: output.write(" ^= ")
    case .bitwiseOrAssignment: output.write(" |= ")
    case .comma: output.write(" , ")
    }
    output.write(ExprWriteable(rhs), inContext: c)
  }
}

extension CXXPrefixExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    // TODO: handle precedence and associativity; as of writing this comment, prefix operators cannot be properly tested.
    switch oper {
    case .prefixIncrement: output.write("++")
    case .prefixDecrement: output.write("--")
    case .unaryPlus: output.write("+")
    case .unaryMinus: output.write("-")
    case .logicalNot: output.write("!")
    case .bitwiseNot: output.write("~")
    case .dereference: output.write("*")
    case .addressOf: output.write("&")
    case .sizeOf: output.write("sizeof ")
    case .coAwait: output.write("co_await ")
    case .throwOp: output.write("throw ")
    case .coYield: output.write("co_yield ")
    }
    output.write(ExprWriteable(base), inContext: c)
  }
}

extension CXXPostfixExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    output.write(ExprWriteable(base), inContext: c)
    switch oper {
    case .suffixIncrement: output.write("++")
    case .suffixDecrement: output.write("--")
    }
  }
}

extension CXXFunctionCallExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(ExprWriteable(callee), inContext: c)
    output.write("(")
    for (i, argument) in arguments.enumerated() {
      if i > 0 {
        output.write(", ")
      }
      output.write(ExprWriteable(argument), inContext: c)
    }
    output.write(")")
  }
}
extension CXXVoidCast: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write("(void) ")
    output.write(ExprWriteable(baseExpr), inContext: c)
  }
}
extension CXXConditionalExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(ExprWriteable(condition), inContext: c)
    output.write(" ? ")
    output.write(ExprWriteable(trueExpr), inContext: c)
    output.write(" : ")
    output.write(ExprWriteable(falseExpr), inContext: c)
  }
}
extension CXXStmtExpr: WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext) {
    output.write(StmtWriteable(stmt), inContext: c)
  }
}

// MARK: Miscellaneous

extension CXXComment: TextOutputStreamable {

  func write<Target: TextOutputStream>(to output: inout Target) {
    if comment.contains("\n") {
      output.write("/* \(comment) */")
    } else {
      output.write("// \(comment)\n")
    }
  }

}

// MARK: Support

/// Type that holds the context needed for writing the C++ code.
struct WriteContext {

  /// True if we are translating the standard library.
  let isStdLib: Bool

}

private protocol WriteableInContext {

  func write<Target: TextOutputStream>(to output: inout Target, inContext c: WriteContext)

}

extension TextOutputStream {

  /// Writes `source` to `self`.
  fileprivate mutating func write<T: TextOutputStreamable>(_ source: T) {
    source.write(to: &self)
  }

  /// Writes `source` to `self`.
  fileprivate mutating func write(_ source: [TextOutputStreamable]) {
    for element in source {
      element.write(to: &self)
    }
  }

  /// Writes `source` to `self`.
  fileprivate mutating func write<T: WriteableInContext>(_ source: T, inContext c: WriteContext) {
    source.write(to: &self, inContext: c)
  }

}

extension WriteableInContext {

  /// Returns the C++ code string corresponding to `self`.
  fileprivate func code(inContext c: WriteContext) -> String {
    var output: String = ""
    output.write(self, inContext: c)
    return output
  }

}
