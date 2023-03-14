import ValModule

extension CXXModule {

  /// Returns the C++ code for `self`.
  public func code(formatter: CodeTransform? = nil) -> TranslationUnitCode {
    let c = WriteContext(isCoreLibrary: self.isCoreLibrary)
    return TranslationUnitCode(
      headerCode: HeaderFile(self).code(inContext: c, withFormatter: formatter),
      sourceCode: SourceFile(self).code(inContext: c, withFormatter: formatter))
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

    if source.isCoreLibrary {
      output << "#include <variant>\n"
      output << "#include <cstdint>\n"
      output << "#include <cstdlib>\n"
      output << AdditionalFileContent("NativeCodePreamble.h")
      output << "namespace Val {\n"
    } else {
      output << "#include \"ValCore.h\"\n"
      output << "namespace \(cxx: source.name) {\n"
      output << "using namespace Val;\n"
    }
    output.write(source.topLevelDecls.lazy.map({ decl in TopLevelInterface(decl) }))
    output << "\n}\n"

    // Add extra native code to the stdlib header.
    if source.isCoreLibrary {
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
    if source.isCoreLibrary {
      output << "#include \"ValCore.h\"\n"
      output << "namespace Val {\n"
    } else {
      output << "#include \"\(source.name).h\"\n"
      output << "namespace \(cxx: source.name) {\n"
    }
    output.write(source.topLevelDecls.lazy.map({ decl in TopLevelDefinition(decl) }))
    output << "}\n"

    // Add extra native code to the core library source file.
    if source.isCoreLibrary {
      output << AdditionalFileContent("NativeCode.cpp")
    }

    // Write a CXX `main` function if the module has an entry point.
    if let body = source.entryPointBody {
      output << "int main() " << AnyStmt(body) << "\n"
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
    if let body = source.body {
      output << " " << AnyStmt(body)
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
      case .constructor(let constructor):
        output << constructor
      }
    }
    output << "};\n"
  }

}

extension CXXClassAttribute: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << type << " " << name
    if let value = initializer {
      output << " = " << AnyExpr(value)
    }
    output << ";\n"
  }

}

extension CXXConstructor: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << name << "("
    output.write(parameters.lazy.map({ p in p.type << " " << p.name }), joinedBy: ", ")
    output << ")"
    if !initializers.isEmpty {
      output << " : "
      output.write(
        initializers.lazy.map({ i in i.name << "(" << AnyExpr(i.value) << ")" }), joinedBy: ", ")
    }
    if let b = body {
      output << " " << AnyStmt(b)
    } else {
      output << " {}\n"
    }
  }
}

extension CXXLocalVarDecl: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << type << " " << name
    if let value = initializer {
      output << " = " << AnyExpr(value)
    }
    output << ";\n"
  }

}

// MARK: Statements

private struct AnyStmt: Writeable {
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
    output.write(stmts.lazy.map({ element in AnyStmt(element) }))
    output << "}\n"
  }

}

extension CXXExprStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << AnyExpr(expr) << ";\n"
  }

}

extension CXXReturnStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "return"
    if let value = expr {
      output << " " << AnyExpr(value)
    }
    output << ";\n"
  }

}

extension CXXIfStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "if ( " << AnyExpr(condition) << " ) " << AnyStmt(trueStmt)
    if let alternative = falseStmt {
      output << "else " << AnyStmt(alternative)
    }
  }

}

extension CXXWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "while ( " << AnyExpr(condition) << " ) " << AnyStmt(body)
  }

}

extension CXXDoWhileStmt: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "do " << AnyStmt(body) << "while ( " << AnyExpr(condition) << " );\n"
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

private struct AnyExpr: Writeable {

  /// The C++ expression to write to the output C++ stream.
  let source: CXXExpr

  /// `true` iff we need to write the expression in parentheses.
  let writeParentheses: Bool

  init(_ source: CXXExpr, withParentheses p: Bool = false) {
    self.source = source
    self.writeParentheses = p
  }

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    if writeParentheses {
      output << "("
    }
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
    if writeParentheses {
      output << ")"
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
    let translation = [
      Operator.scopeResolution: " :: ",
      Operator.dotAccess: " . ",
      Operator.ptrAccess: " -> ",
      Operator.dotPtrToMember: " .* ",
      Operator.ptrToMember: " ->* ",
      Operator.multiplication: " * ",
      Operator.division: " / ",
      Operator.remainder: " % ",
      Operator.addition: " + ",
      Operator.subtraction: " - ",
      Operator.leftShift: " << ",
      Operator.rightShift: " >> ",
      Operator.spaceship: " <=> ",
      Operator.lessThan: " < ",
      Operator.lessEqual: " <= ",
      Operator.greaterThan: " > ",
      Operator.greaterEqual: " >= ",
      Operator.equality: " == ",
      Operator.inequality: " == ",
      Operator.bitwiseAnd: " & ",
      Operator.bitwiseXor: " ^ ",
      Operator.bitwiseOr: " | ",
      Operator.logicalAnd: " && ",
      Operator.logicalOr: " || ",
      Operator.assignment: " = ",
      Operator.addAssignment: " += ",
      Operator.subAssignment: " -= ",
      Operator.mulAssignment: " *= ",
      Operator.divAssignment: " /= ",
      Operator.remAssignment: " %= ",
      Operator.shiftLeftAssignment: " <<= ",
      Operator.shiftRightAssignment: " >>= ",
      Operator.bitwiseAndAssignment: " &= ",
      Operator.bitwiseXorAssignment: " ^= ",
      Operator.bitwiseOrAssignment: " |= ",
      Operator.comma: " , ",
    ]
    let lhsNeedsParentheses =
      lhs.precedence > self.precedence || (lhs.precedence == self.precedence && !self.isLeftToRight)
    let rhsNeedsParentheses =
      rhs.precedence > self.precedence || (rhs.precedence == self.precedence && self.isLeftToRight)
    output << AnyExpr(lhs, withParentheses: lhsNeedsParentheses)
      << translation[oper]!
      << AnyExpr(rhs, withParentheses: rhsNeedsParentheses)
  }

}

extension CXXPrefixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, prefix operators cannot be properly tested.
    let translation = [
      Operator.prefixIncrement: "++",
      Operator.prefixDecrement: "--",
      Operator.unaryPlus: "+",
      Operator.unaryMinus: "-",
      Operator.logicalNot: "!",
      Operator.bitwiseNot: "~",
      Operator.dereference: "*",
      Operator.addressOf: "&",
      Operator.sizeOf: "sizeof ",
      Operator.coAwait: "co_await ",
      Operator.throwOp: "throw ",
      Operator.coYield: "co_yield ",
    ]
    output << translation[oper]! << AnyExpr(base)
  }

}

extension CXXPostfixExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    // TODO: handle precedence and associativity; as of writing this comment, postfix operators cannot be properly tested.
    let translation = [
      Operator.suffixIncrement: "++",
      Operator.suffixDecrement: "--",
    ]
    output << AnyExpr(base) << translation[oper]!
  }

}

extension CXXFunctionCallExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << AnyExpr(callee) << "("
    output.write(arguments.lazy.map({ a in AnyExpr(a) }), joinedBy: ", ")
    output << ")"
  }

}
extension CXXVoidCast: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << "(void) " << AnyExpr(baseExpr)
  }

}
extension CXXConditionalExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    let conditionNeedsParentheses = condition.precedence == self.precedence
    let trueExprNeedsParentheses = trueExpr.precedence == self.precedence
    let falseExprNeedsParentheses = falseExpr.precedence == self.precedence
    output << AnyExpr(condition, withParentheses: conditionNeedsParentheses)
      << " ? " << AnyExpr(trueExpr, withParentheses: trueExprNeedsParentheses)
      << " : " << AnyExpr(falseExpr, withParentheses: falseExprNeedsParentheses)
  }

}
extension CXXStmtExpr: Writeable {

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    output << AnyStmt(stmt)
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
  let isCoreLibrary: Bool

}

/// A stream to write C++ code into, along with the context in which we are writing the code.
struct CXXStream {

  /// Where we write the C++ code.
  var output: String

  /// True if we are translating the standard library.
  let context: WriteContext

  /// Writes `source` to `self`.
  mutating func write(_ source: String) {
    output.write(source)
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

extension CXXStream {

  /// Write `items` to `self`.
  fileprivate mutating func write<S: Sequence>(_ items: S) where S.Element: Writeable {
    for i in items {
      i.write(to: &self)
    }
  }

  /// Write `items` to `self`, separated by `separator`.
  fileprivate mutating func write<S: Sequence>(_ items: S, joinedBy separator: String)
  where S.Element: Writeable {
    var isFirst = true
    for i in items {
      if isFirst {
        isFirst = false
      } else {
        output.write(separator)
      }
      i.write(to: &self)
    }
  }

}

precedencegroup StreamPrecendence {
  associativity: right
}
/// Define operator for concatenating `Writeable` objects and writing them to `CXXStream`.
infix operator << : StreamPrecendence

/// Writes `rhs` to `lhs`.
private func << <T: Writeable>(lhs: inout CXXStream, rhs: T) {
  rhs.write(to: &lhs)
}

/// Builds a Writeable object from the concatenation of `lhs` and `rhs`.
private func << <T, U: Writeable>(lhs: T, rhs: U) -> Pair<T, U> {
  return Pair<T, U>(first: lhs, second: rhs)
}

/// A pair of writeable elements.
///
/// This is a writeable type, thus allowing to build chains of writeable objects.
private struct Pair<T: Writeable, U: Writeable>: Writeable {

  /// The first writeable object (head).
  let first: T
  /// The second writeable object (rest).
  let second: U

  /// Writes 'self' to 'output'.
  func write(to output: inout CXXStream) {
    first.write(to: &output)
    second.write(to: &output)
  }

}

extension Writeable {

  /// Returns the C++ code string corresponding to `self`.
  fileprivate func code(inContext c: WriteContext, withFormatter formatter: CodeTransform?)
    -> String
  {
    var output = CXXStream(output: "", context: c)
    output << self
    return formatter?(output.output) ?? output.output
  }

}
