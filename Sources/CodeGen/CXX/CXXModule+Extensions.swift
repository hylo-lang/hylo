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
private struct HeaderFile: Writeable {

  let source: CXXModule

  init(_ source: CXXModule) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // Emit the header guard.
    // "#pragma once" is non-standard, but implemented by all major compilers,
    // and it typically does a better job
    // (more efficiently treated in the compiler, and reduces probability of accidents)
    output.write("#pragma once\n")

    if source.isStdLib {
      output.write("#include <variant>\n")
      output.write("#include <cstdint>\n")
      output.write("#include <cstdlib>\n")
    } else {
      output.write("#include \"ValStdLib.h\"\n")
    }
    output.write("namespace \(source.name) {\n")
    if !source.isStdLib {
      output.write("using namespace ValStdLib;\n")
    }
    output.write(source.topLevelDecls.map({ decl in TopLevelInterface(decl) }))
    output.write("\n}\n")

    // Add extra native code to the stdlib header.
    if source.isStdLib {
      output.write(AdditionalFileContent("NativeCode.h"))
    }
  }

}

/// Knows how to write the content of C++ source file for a `CXXModule`.
private struct SourceFile: Writeable {

  let source: CXXModule

  init(_ source: CXXModule) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("#include \"\(source.name).h\"\n")
    output.write("namespace \(source.name) {\n")
    output.write(source.topLevelDecls.map({ decl in TopLevelDefinition(decl) }))
    output.write("}\n")

    // Add extra native code to the stdlib source file.
    if source.isStdLib {
      output.write(AdditionalFileContent("NativeCode.cpp"))
    }

    // Write a CXX `main` function if the module has an entry point.
    if source.entryPointBody != nil {
      output.write("int main()")
      output.write(StmtWriteable(source.entryPointBody!))
      output.write("\n")
    }
  }

}

/// Knows how to write additional file content from ValModule
private struct AdditionalFileContent: Writeable {

  let fileName: String

  init(_ fileName: String) {
    self.fileName = fileName
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    if let text = try? String(contentsOf: ValModule.cxxSupport!.appendingPathComponent(fileName)) {
      output.write(text)
    }
  }

}

// MARK: Declarations

/// Knows how to write the interface for a top level declaration.
private struct TopLevelInterface: Writeable {

  let source: CXXTopLevelDecl

  init(_ source: CXXTopLevelDecl) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    switch type(of: source).kind {
    case CXXFunctionDecl.self:
      output.write(FunctionSignature(source as! CXXFunctionDecl))
    case CXXClassDecl.self:
      // We write the class definition in the header file.
      output.write(ClassDefinition(source as! CXXClassDecl))
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected top-level declaration")
    }
    output.write(";\n")
  }

}

/// Knows how to write the definition for a top level declaration.
private struct TopLevelDefinition: Writeable {

  let source: CXXTopLevelDecl

  init(_ source: CXXTopLevelDecl) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    switch type(of: source).kind {
    case CXXFunctionDecl.self:
      output.write(FunctionDefinition(source as! CXXFunctionDecl))
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
private struct FunctionSignature: Writeable {

  let source: CXXFunctionDecl

  init(_ source: CXXFunctionDecl) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << source.output << " " << source.identifier << "("
    for (i, parameter) in source.parameters.enumerated() {
      if i != 0 { output.write(", ") }
      output << parameter.type << " " << parameter.name
    }
    output.write(")")
  }

}

/// Knows how to write a function definition.
private struct FunctionDefinition: Writeable {

  let source: CXXFunctionDecl

  init(_ source: CXXFunctionDecl) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(FunctionSignature(source))
    if source.body != nil {
      output << " " << StmtWriteable(source.body!)
    } else {
      output.write(";\n")
    }
  }

}

/// Knows how to write a class signature.
private struct ClassDefinition: Writeable {

  let source: CXXClassDecl

  init(_ source: CXXClassDecl) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "class " << source.name << " {\npublic:\n"
    for member in source.members {
      switch member {
      case .attribute(let attribute):
        output.write(attribute)
      case .method:
        output.write("// method\n")
      case .constructor:
        output.write("// constructor\n")
      }
    }
    if output.context.isStdLib {
      // For standard value types try to generate implict conversion constructors from C++ literal types.
      output.write(ConversionConstructor(source))
    }
    output.write("};\n")
  }
}

/// Knows how to write a conversion constructor for a class.
///
/// This only applies for classes have one data member, and its type is native.
private struct ConversionConstructor: Writeable {

  let parentClass: CXXClassDecl

  init(_ parentClass: CXXClassDecl) {
    self.parentClass = parentClass
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
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
      output << parentClass.name.description << "(" << dataMembers[0].type << " v) : " << dataMembers[0].name << "(v) {}\n"
    }
  }
}

extension CXXClassAttribute: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << type << " " << name
    if let value = initializer {
      output << " = " << ExprWriteable(value)
    }
    output.write(";\n")
  }

}

extension CXXLocalVarDecl: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << type << " " << name
    if let value = initializer {
      output << " = " << ExprWriteable(value)
    }
    output.write(";\n")
  }

}

// MARK: Statements

private struct StmtWriteable: Writeable {
  let source: CXXStmt

  init(_ source: CXXStmt) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    switch type(of: source).kind {
    case CXXScopedBlock.self:
      output.write(source as! CXXScopedBlock)
    case CXXLocalVarDecl.self:
      output.write(source as! CXXLocalVarDecl)
    case CXXExprStmt.self:
      output.write(source as! CXXExprStmt)
    case CXXReturnStmt.self:
      output.write(source as! CXXReturnStmt)
    case CXXIfStmt.self:
      output.write(source as! CXXIfStmt)
    case CXXWhileStmt.self:
      output.write(source as! CXXWhileStmt)
    case CXXDoWhileStmt.self:
      output.write(source as! CXXDoWhileStmt)
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

extension CXXScopedBlock: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("{\n")
    for s in stmts {
      output.write(StmtWriteable(s))
    }
    output.write("}\n")
  }

}

extension CXXExprStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(ExprWriteable(expr))
    output.write(";\n")
  }

}

extension CXXReturnStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("return")
    if expr != nil {
      output.write(" ")
      output.write(ExprWriteable(expr!))
    }
    output.write(";\n")
  }

}

extension CXXIfStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("if ( ")
    output.write(ExprWriteable(condition))
    output.write(" ) ")
    output.write(StmtWriteable(trueStmt))
    if falseStmt != nil {
      output.write("else ")
      output.write(StmtWriteable(falseStmt!))
    }
  }

}

extension CXXWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("while ( ")
    output.write(ExprWriteable(condition))
    output.write(" ) ")
    output.write(StmtWriteable(body))
  }

}

extension CXXDoWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("do ")
    output.write(StmtWriteable(body))
    output.write("while ( ")
    output.write(ExprWriteable(condition))
    output.write(" );\n")
  }

}

extension CXXBreakStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("break;\n")
  }

}

extension CXXContinueStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("continue;\n")
  }

}

// MARK: Expressions

private struct ExprWriteable: Writeable {

  let source: CXXExpr

  init(_ source: CXXExpr) {
    self.source = source
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
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
      output.write(source as! CXXInfixExpr)
    case CXXPrefixExpr.self:
      output.write(source as! CXXPrefixExpr)
    case CXXPostfixExpr.self:
      output.write(source as! CXXPostfixExpr)
    case CXXFunctionCallExpr.self:
      output.write(source as! CXXFunctionCallExpr)
    case CXXVoidCast.self:
      output.write(source as! CXXVoidCast)
    case CXXConditionalExpr.self:
      output.write(source as! CXXConditionalExpr)
    case CXXStmtExpr.self:
      output.write(source as! CXXStmtExpr)
    case CXXComment.self:
      output.write(source as! CXXComment)
    default:
      fatalError("unexpected expressions")
    }
  }

}

extension CXXBooleanLiteralExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(value ? "true" : "false")
  }

}
extension CXXIntegerLiteralExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(value)
  }

}
extension CXXIdentifier: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(description)
  }

}
extension CXXReceiverExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write("this")
  }

}
extension CXXTypeExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(text)
  }

}
extension CXXInfixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, infix operators cannot be properly tested.
    output.write(ExprWriteable(lhs))
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
    output.write(ExprWriteable(rhs))
  }
}

extension CXXPrefixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
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
    output.write(ExprWriteable(base))
  }
}

extension CXXPostfixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    output.write(ExprWriteable(base))
    switch oper {
    case .suffixIncrement: output.write("++")
    case .suffixDecrement: output.write("--")
    }
  }
}

extension CXXFunctionCallExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(ExprWriteable(callee))
    output.write("(")
    for (i, argument) in arguments.enumerated() {
      if i > 0 {
        output.write(", ")
      }
      output.write(ExprWriteable(argument))
    }
    output.write(")")
  }
}
extension CXXVoidCast: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "(void) " << ExprWriteable(baseExpr)
  }
}
extension CXXConditionalExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(
      [
        ExprWriteable(condition),
        " ? ",
        ExprWriteable(trueExpr),
        " : ",
        ExprWriteable(falseExpr),
      ])
  }
}
extension CXXStmtExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output.write(StmtWriteable(stmt))
  }
}

// MARK: Miscellaneous

extension CXXComment: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
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

/// A stream to write C++ code into, along with the context in which we are writing the code.
struct CXXStream {

  /// Where we write the C++ code.
  var output: String

  /// True if we are translating the standard library.
  let context: WriteContext

  mutating func write(_ source: String) {
    output.write(source)
  }

}

extension CXXStream {

  /// Writes `source` to `self`.
  fileprivate mutating func write<T: Writeable>(_ source: T) {
    source.write(to: &self)
  }

  /// Writes `source` to `self`.
  fileprivate mutating func write(_ source: [Writeable]) {
    for element in source {
      element.write(to: &self)
    }
  }

}

/// An entitity that can be written as C++ code.
private protocol Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream)

}

/// String is also a Writeable.
extension String: Writeable {

  func write(to output: inout CXXStream) {
    output.write(self)
  }

}

precedencegroup StreamPrecendence {
  associativity: right
}

infix operator << : StreamPrecendence

fileprivate func << <T: Writeable>(lhs: inout CXXStream, rhs: T) {
  rhs.write(to: &lhs)
}

fileprivate func << <T, U: Writeable>(lhs: T, rhs: U) -> Pair<T, U> {
  return Pair<T, U>(first: lhs, second: rhs)
}

fileprivate struct Pair<T: Writeable, U: Writeable>: Writeable {

  let first: T
  let second: U

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    first.write(to: &output)
    second.write(to: &output)
  }

}



extension Writeable {

  /// Returns the C++ code string corresponding to `self`.
  fileprivate func code(inContext c: WriteContext) -> String {
    var output = CXXStream(output: "", context: c)
    output.write(self)
    return output.output
  }

}
