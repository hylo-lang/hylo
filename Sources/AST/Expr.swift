import Basic

/// An expression.
public protocol Expr: Node {

  /// The type of the expression.
  var type: ValType { get set }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: An expression visitor.
  func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor

}

/// A Boolean literal.
public final class BoolLiteralExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The value of the literal.
  public var value: Bool

  public init(value: Bool, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An integer literal.
public final class IntLiteralExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The value of the literal.
  public var value: Int

  public init(value: Int, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A floating-point literal.
public final class FloatLiteralExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The value of the literal.
  public var value: Double

  public init(value: Double, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A string literal.
public final class StringLiteralExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The value of the literal.
  public var value: String

  public init(value: String, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An assignment expression (e.g. `foo.bar = baz`).
///
/// This has always a unit type.
public final class AssignExpr: Expr {

  public var range: SourceRange?

  public var type: ValType {
    didSet { assert(type.isUnit) }
  }

  /// An expression representing the storage to which `rvalue` is assigned.
  public var lvalue: Expr

  /// The value of the assignment.
  public var rvalue: Expr

  public init(lvalue: Expr, rvalue: Expr, range: SourceRange? = nil) {
    self.lvalue = lvalue
    self.rvalue = rvalue
    self.type = lvalue.type.context.unitType
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// The base class for cast expressions.
public class BaseCastExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The value being cast.
  public var value: Expr

  /// The signature of type to which the value is being cast.
  public var sign: Sign

  public init(value: Expr, sign: Sign, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.sign = sign
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An runtime cast expression (e.g., `foo as! Bar`).
public final class RuntimeCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A tuple expression (e.g., `(fst: 4, snd: 2)`).
public final class TupleExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The elements of the tuple.
  public var elems: [TupleElem]

  public init(elems: [TupleElem], type: ValType, range: SourceRange? = nil) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A tuple's element, or a function call's argument.
public struct TupleElem {

  /// The source range of this element's textual representation.
  public var range: SourceRange?

  /// The label of the element.
  public var label: String?

  /// The value of the element.
  public var value: Expr

  public init(label: String? = nil, value: Expr, range: SourceRange? = nil) {
    self.label = label
    self.value = value
    self.range = range
  }

}

/// A function call expression.
///
/// This represents any function application, including infix, prefix and postfix expressions.
///
/// - Note: Assignments and address-taking expressions are represented with the nodes `AssignExpr`
///   and `AddrOfExpr`, respectively. These denote a particular behavior for which it is useful to
///   have dedicate nodes.
public final class CallExpr: Expr {

  /// The notation of a call expression.
  public enum Notation {

    /// Standard notation: the arguments are given in parentheses after the callee.
    case standard

    /// Infix notation: the callee is between two operands.
    case infix

    /// Prefix notation: the callee is an operator that directly precedes a single operand.
    case prefix

    /// Postfix notation: the callee is an operator that directly follows a signle operand.
    case postfix

  }

  public var range: SourceRange?

  public var type: ValType

  /// The function being called.
  public var fun: Expr

  /// The arguments of the call.
  public var args: [TupleElem]

  /// The notation of the call.
  public var notation: Notation

  public init(
    fun: Expr,
    args: [TupleElem],
    notation: Notation = .standard,
    type: ValType,
    range: SourceRange? = nil
  ) {
    self.fun = fun
    self.args = args
    self.notation = notation
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

  /// Creates a prefix call.
  public static func prefix(
    fun: MemberExpr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    return CallExpr(fun: fun, args: [], notation: .prefix, type: type, range: range)
  }

  /// Creates a postfix call.
  public static func postfix(
    fun: MemberExpr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    return CallExpr(fun: fun, args: [], notation: .postfix, type: type, range: range)
  }

  /// Creates an infix call.
  public static func infix(
    fun: MemberExpr,
    operand: Expr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    let arg = CallArg(value: operand, range: operand.range)
    return CallExpr(fun: fun, args: [arg], notation: .prefix, type: type, range: range)
  }

}

public typealias CallArg = TupleElem

/// An identifier referring to an unresolved declaration.
///
/// This has always an unresolved type. The referred declaration must be resolved by the semantic
/// analysis, which should ultimately substitute this node with a `DeclRefExpr`.
public final class UnresolvedDeclRefExpr: Expr {

  public var type: ValType {
    didSet { assert(type.isUnresolved) }
  }

  /// An identifier.
  public var ident: Ident

  public init(ident: Ident, type: UnresolvedType) {
    self.ident = ident
    self.type = type
  }

  /// The unqualified name of the referred declaration.
  public var name: String { ident.name }

  public var range: SourceRange? { ident.range }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to an unresolved declaration, explicitly qualified by a type namespace
/// (e.g., `Builtin::bitcast`).
///
/// Conceptually, this wraps an unresolved declaration reference, providing context for the space
/// into which it points. The type prefix is resolved during name binding.
public final class UnresolvedQualDeclRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType {
    didSet { assert(type.isUnresolved) }
  }

  /// A type identifier.
  public var namespace: IdentSign

  /// An identifier.
  public var ident: Ident

  public init(
    namespace: IdentSign,
    ident: Ident,
    type: UnresolvedType,
    range: SourceRange? = nil
  ) {
    self.namespace = namespace
    self.ident = ident
    self.type = type
    self.range = range
  }

  /// The unqualified name of the referred declaration.
  public var name: String { ident.name }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a set of overloaded value declarations.
///
/// This is typically substituted for an `UnresolvedDeclRefExpr` after the name binding. Additional
/// contextual information is required to disambiguate the referred declaration, at which point the
/// expression can be finally substituted with a `DeclRefExpr`.
public final class OverloadedDeclRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The set of candidate declarations for the expresssion.
  public var declSet: [ValueDecl] = []

  public init(subExpr: Expr, declSet: [ValueDecl], type: ValType, range: SourceRange? = nil) {
    self.declSet = declSet
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a resolved value declaration.
public final class DeclRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public init(decl: ValueDecl, type: ValType, range: SourceRange? = nil) {
    self.decl = decl
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a resolved type declaration.
public final class TypeDeclRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType {
    get { decl.type }
    set { assert(newValue === decl.type) }
  }

  /// The declaration referred by the expresssion.
  public var decl: TypeDecl

  public init(decl: TypeDecl, range: SourceRange? = nil) {
    self.decl = decl
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A member expression (e.g., `foo.bar`).
public protocol MemberExpr: Expr {

  /// The base expression.
  var base: Expr { get }

}

/// A member expression (e.g., `foo.bar`) with an unresolved type.
///
/// The base may be resolved, but the compiler may still require contextual type information to
/// determine the member declaration to which the node refers.
public final class UnresolvedMemberExpr: MemberExpr {

  public var range: SourceRange?

  public var type: ValType

  /// The base expression.
  public var base: Expr

  /// The member's identifier.
  public var ident: Ident

  public init(base: Expr, ident: Ident, type: UnresolvedType, range: SourceRange? = nil) {
    self.base = base
    self.ident = ident
    self.type = type
    self.range = range
  }

  /// The unqualified name of the member.
  public var memberName: String { ident.name }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression referring to a resolved member declaration.
///
/// This does not represent expressions that denote the member of a tuple, as tuple member do not
/// have declarations. Instead, those are represented by `TupleMemberExpr`.
public final class MemberDeclRefExpr: MemberExpr {

  public var range: SourceRange?

  public var type: ValType

  /// The base expression.
  public var base: Expr

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public init(base: Expr, decl: ValueDecl, type: ValType, range: SourceRange? = nil) {
    self.base = base
    self.decl = decl
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression that refers to the member of a tuple (e.g., `(fst: 3, snd: 2).fst`).
public final class TupleMemberExpr: MemberExpr {

  public var range: SourceRange?

  public var type: ValType

  /// The base expression.
  public var base: Expr

  /// The index of the member in the tuple.
  public var memberIndex: Int

  public init(base: Expr, memberIndex: Int, type: ValType, range: SourceRange? = nil) {
    self.base = base
    self.memberIndex = memberIndex
    self.type = type
    self.range = range
  }

  /// The label of the member in the tuple, if any.
  public var memberLabel: String? {
    guard let tType = type as? TupleType else { return nil }
    return tType.elems[memberIndex].label
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An anonymous function.
public final class LambdaExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The declaration of the function.
  public var decl: FunDecl

  public init(decl: FunDecl, type: ValType, range: SourceRange? = nil) {
    self.decl = decl
    self.type = type
    self.range = range
  }

  /// Realizes the type of the function, filling blanks with fresh type variables.
  public func realize() -> ValType {
    if decl.state >= .realized { return decl.type }

    // Realize the type of the parameters.
    var params: [FunType.Param] = []
    for param in decl.params {
      let rawType: ValType
      if let sign = param.sign {
        rawType = sign.realize(unqualifiedFrom: decl)
      } else {
        rawType = TypeVar(context: type.context, node: param)
        param.setState(.realized)
      }
      params.append(FunType.Param(label: param.label, type: rawType))
    }

    // Realize the return type.
    let retType = decl.retSign?.realize(unqualifiedFrom: decl) ?? TypeVar(context: type.context)

    decl.type = type.context.funType(params: params, retType: retType)
    decl.setState(.realized)
    return decl.type
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression that is evaluated asynchronously (e.g., `async foo()`).
///
/// An async expression is a wrapper around a "future". It describes a value that might still being
/// computed, typically by a concurrent thread.
public final class AsyncExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The function to evaluate asynchronously.
  public var body: FunDecl

  public init(body: FunDecl, type: ValType, range: SourceRange? = nil) {
    self.body = body
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression awaiting the result of an asynchronous value (e.g., `await foo()`).
public final class AwaitExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The asynchronous value to await.
  public var value: Expr

  public init(value: Expr, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression resolving to the address of a value (e.g., `&foo.bar`).
public final class AddrOfExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The expression representing the address to resolve.
  public var value: Expr

  public init(value: Expr, type: ValType, range: SourceRange? = nil) {
    self.value = value
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A match expressions or statement.
///
/// This denotes either a match expression or a match statement, depending on the context in which
/// the node appears: it is an expression when it represents a sub-expression, otherwise it is a
/// statement.
///
/// All cases of a match *expression* must contain exactly one expression, which should have the
/// same type as all its siblings.
public final class MatchExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// Indicates whether the match appears as a sub-expression, in a larger expression.
  public var isSubexpr: Bool

  /// The subject being matched
  public var subject: Expr

  /// The patterns against which the subject is being matched.
  public var cases: [MatchCaseStmt]

  public init(
    isSubexpr: Bool,
    subject: Expr,
    cases: [MatchCaseStmt],
    type: ValType,
    range: SourceRange? = nil
  ) {
    self.isSubexpr = isSubexpr
    self.subject = subject
    self.cases = cases
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A wildcard expression.
public final class WildcardExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  public init(type: ValType, range: SourceRange? = nil) {
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An ill-formed expression.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorExpr: Expr {

  public var range: SourceRange?

  public var type: ValType  {
    didSet { assert(type.isError) }
  }

  public init(type: ErrorType, range: SourceRange? = nil) {
    self.type = type
    self.range = range
  }

  public init(replacing expr: Expr) {
    self.type = expr.type.context.errorType
    self.range = expr.range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

}
