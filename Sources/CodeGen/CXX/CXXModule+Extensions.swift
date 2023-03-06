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
    output << "#pragma once\n"

    if source.isStdLib {
      output << "#include <variant>\n"
      output << "#include <cstdint>\n"
      output << "#include <cstdlib>\n"
    } else {
      output << "#include \"ValStdLib.h\"\n"
    }
    output << "namespace \(source.name) {\n"
    if !source.isStdLib {
      output << "using namespace ValStdLib;\n"
    }
    output.write(source.topLevelDecls.lazy.map({ decl in TopLevelInterface(decl) }))
    output << "\n}\n"

    // Add extra native code to the stdlib header.
    if source.isStdLib {
      output << AdditionalFileContent("NativeCode.h")
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
    output << "#include \"\(source.name).h\"\n"
    output << "namespace \(source.name) {\n"
    output.write(source.topLevelDecls.lazy.map({ decl in TopLevelDefinition(decl) }))
    output << "}\n"

    // Add extra native code to the stdlib source file.
    if source.isStdLib {
      output << AdditionalFileContent("NativeCode.cpp")
    }

    // Write a CXX `main` function if the module has an entry point.
    if source.entryPointBody != nil {
      output << "int main()" << StmtWriteable(source.entryPointBody!) << "\n"
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
      output << text
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
      output << FunctionSignature(source as! CXXFunctionDecl)
    case CXXClassDecl.self:
      // We write the class definition in the header file.
      output << ClassDefinition(source as! CXXClassDecl)
    case CXXComment.self:
      output << (source as! CXXComment)
    default:
      fatalError("unexpected top-level declaration")
    }
    output << ";\n"
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
      output << FunctionDefinition(source as! CXXFunctionDecl)
    case CXXClassDecl.self:
      // TODO: write implementation of methods
      break
    case CXXComment.self:
      output << (source as! CXXComment)
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
    output.write(source.parameters.lazy.map({ p in p.type << " " << p.name }), joinedBy: ", ")
    output << ")"
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
    output << FunctionSignature(source)
    if source.body != nil {
      output << " " << StmtWriteable(source.body!)
    } else {
      output << ";\n"
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
        output << attribute
      case .method:
        output << "// method\n"
      case .constructor:
        output << "// constructor\n"
      }
    }
    if output.context.isStdLib {
      // For standard value types try to generate implict conversion constructors from C++ literal types.
      output << ConversionConstructor(source)
    }
    output << "};\n"
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
      output << parentClass.name.description << "(" << dataMembers[0].type << " v) : "
        << dataMembers[0].name << "(v) {}\n"
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
    output << ";\n"
  }

}

extension CXXLocalVarDecl: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << type << " " << name
    if let value = initializer {
      output << " = " << ExprWriteable(value)
    }
    output << ";\n"
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
      output << (source as! CXXScopedBlock)
    case CXXLocalVarDecl.self:
      output << (source as! CXXLocalVarDecl)
    case CXXExprStmt.self:
      output << (source as! CXXExprStmt)
    case CXXReturnStmt.self:
      output << (source as! CXXReturnStmt)
    case CXXIfStmt.self:
      output << (source as! CXXIfStmt)
    case CXXWhileStmt.self:
      output << (source as! CXXWhileStmt)
    case CXXDoWhileStmt.self:
      output << (source as! CXXDoWhileStmt)
    case CXXBreakStmt.self:
      output << (source as! CXXBreakStmt)
    case CXXContinueStmt.self:
      output << (source as! CXXContinueStmt)
    case CXXComment.self:
      output << (source as! CXXComment)
    default:
      fatalError("unexpected statement")
    }
  }
}

extension CXXScopedBlock: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "{\n"
    output.write(stmts.lazy.map({element in StmtWriteable(element)}))
    output << "}\n"
  }

}

extension CXXExprStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << ExprWriteable(expr) << ";\n"
  }

}

extension CXXReturnStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "return"
    if expr != nil {
      output << " " << ExprWriteable(expr!)
    }
    output << ";\n"
  }

}

extension CXXIfStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "if ( " << ExprWriteable(condition) << " ) " << StmtWriteable(trueStmt)
    if falseStmt != nil {
      output << "else " << StmtWriteable(falseStmt!)
    }
  }

}

extension CXXWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "while ( " << ExprWriteable(condition) << " ) " << StmtWriteable(body)
  }

}

extension CXXDoWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "do " << StmtWriteable(body) << "while ( " << ExprWriteable(condition) << " );\n"
  }

}

extension CXXBreakStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "break;\n"
  }

}

extension CXXContinueStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "continue;\n"
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
      output << (source as! CXXBooleanLiteralExpr)
    case CXXIntegerLiteralExpr.self:
      output << (source as! CXXIntegerLiteralExpr)
    case CXXIdentifier.self:
      output << (source as! CXXIdentifier)
    case CXXReceiverExpr.self:
      output << (source as! CXXReceiverExpr)
    case CXXTypeExpr.self:
      output << (source as! CXXTypeExpr)
    case CXXInfixExpr.self:
      output << (source as! CXXInfixExpr)
    case CXXPrefixExpr.self:
      output << (source as! CXXPrefixExpr)
    case CXXPostfixExpr.self:
      output << (source as! CXXPostfixExpr)
    case CXXFunctionCallExpr.self:
      output << (source as! CXXFunctionCallExpr)
    case CXXVoidCast.self:
      output << (source as! CXXVoidCast)
    case CXXConditionalExpr.self:
      output << (source as! CXXConditionalExpr)
    case CXXStmtExpr.self:
      output << (source as! CXXStmtExpr)
    case CXXComment.self:
      output << (source as! CXXComment)
    default:
      fatalError("unexpected expressions")
    }
  }

}

extension CXXBooleanLiteralExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << (value ? "true" : "false")
  }

}
extension CXXIntegerLiteralExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << value
  }

}
extension CXXIdentifier: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << description
  }

}
extension CXXReceiverExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "this"
  }

}
extension CXXTypeExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << text
  }

}
extension CXXInfixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, infix operators cannot be properly tested.
    output << ExprWriteable(lhs)
    switch oper {
    case .scopeResolution: output << " :: "
    case .dotAccess: output << " . "
    case .ptrAccess: output << " -> "
    case .dotPtrToMember: output << " .* "
    case .ptrToMember: output << " ->* "
    case .multiplication: output << " * "
    case .division: output << " / "
    case .remainder: output << " % "
    case .addition: output << " + "
    case .subtraction: output << " - "
    case .leftShift: output << " << "
    case .rightShift: output << " >> "
    case .spaceship: output << " <=> "
    case .lessThan: output << " < "
    case .lessEqual: output << " <= "
    case .greaterThan: output << " > "
    case .greaterEqual: output << " >= "
    case .equality: output << " == "
    case .inequality: output << " == "
    case .bitwiseAnd: output << " & "
    case .bitwiseXor: output << " ^ "
    case .bitwiseOr: output << " | "
    case .logicalAnd: output << " && "
    case .logicalOr: output << " || "
    case .assignment: output << " = "
    case .addAssignment: output << " += "
    case .subAssignment: output << " -= "
    case .mulAssignment: output << " *= "
    case .divAssignment: output << " /= "
    case .remAssignment: output << " %= "
    case .shiftLeftAssignment: output << " <<= "
    case .shiftRightAssignment: output << " >>= "
    case .bitwiseAndAssignment: output << " &= "
    case .bitwiseXorAssignment: output << " ^= "
    case .bitwiseOrAssignment: output << " |= "
    case .comma: output << " , "
    }
    output << ExprWriteable(rhs)
  }
}

extension CXXPrefixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, prefix operators cannot be properly tested.
    switch oper {
    case .prefixIncrement: output << "++"
    case .prefixDecrement: output << "--"
    case .unaryPlus: output << "+"
    case .unaryMinus: output << "-"
    case .logicalNot: output << "!"
    case .bitwiseNot: output << "~"
    case .dereference: output << "*"
    case .addressOf: output << "&"
    case .sizeOf: output << "sizeof "
    case .coAwait: output << "co_await "
    case .throwOp: output << "throw "
    case .coYield: output << "co_yield "
    }
    output << ExprWriteable(base)
  }
}

extension CXXPostfixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    output << ExprWriteable(base)
    switch oper {
    case .suffixIncrement: output << "++"
    case .suffixDecrement: output << "--"
    }
  }
}

extension CXXFunctionCallExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << ExprWriteable(callee) << "("
    output.write(arguments.lazy.map({ a in ExprWriteable(a) }), joinedBy: ", ")
    output << ")"
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
    output << ExprWriteable(condition) << " ? " << ExprWriteable(trueExpr) << " : "
      << ExprWriteable(falseExpr)
  }
}
extension CXXStmtExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << StmtWriteable(stmt)
  }
}

// MARK: Miscellaneous

extension CXXComment: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    if comment.contains("\n") {
      output << "/* \(comment) */"
    } else {
      output << "// \(comment)\n"
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
  fileprivate mutating func write<S: Sequence>(_ source: S) where S.Element: Writeable {
    for element in source {
      element.write(to: &self)
    }
  }

  fileprivate mutating func write<S: Sequence>(_ items: S, joinedBy separator: String)
  where S.Element: Writeable {
    var isFirst = true
    for i in items {
      if !isFirst {
        output.write(separator)
        isFirst = false
      }
      i.write(to: &self)
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

private func << <T: Writeable>(lhs: inout CXXStream, rhs: T) {
  rhs.write(to: &lhs)
}

private func << <T, U: Writeable>(lhs: T, rhs: U) -> Pair<T, U> {
  return Pair<T, U>(first: lhs, second: rhs)
}

private struct Pair<T: Writeable, U: Writeable>: Writeable {

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
