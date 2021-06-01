import Basic

/// An expression.
public protocol Expr: Node {

  /// The type of the expression.
  var type: ValType { get set }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: An expression visitor.
  func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor

}

/// An integer literal.
public final class IntLiteralExpr: Expr {

  public init(value: Int, type: ValType, range: SourceRange) {
    self.value = value
    self.type = type
    self.range = range
  }

  /// The literal's value.
  public var value: Int

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An assignment expression (e.g. `foo.bar = baz`).
///
/// This has always a unit type.
public final class AssignExpr: Expr {

  public init(lvalue: Expr, rvalue: Expr, range: SourceRange) {
    self.lvalue = lvalue
    self.rvalue = rvalue
    self.type = lvalue.type.context.unitType
    self.range = range
  }

  /// An expression representing the storage to which `rvalue` is assigned.
  public var lvalue: Expr

  /// The value of the assignment.
  public var rvalue: Expr

  public var type: ValType {
    didSet { assert(type is TupleType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// The base class for cast expressions.
public class BaseCastExpr: Expr {

  public init(value: Expr, sign: TypeRepr, type: ValType, range: SourceRange) {
    self.value = value
    self.sign = sign
    self.type = type
    self.range = range
  }

  /// The value being cast.
  public var value: Expr

  /// The type to which the value is being cast.
  public var sign: TypeRepr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A safe, dynamic cast expression (e.g., `foo as? Bar`).
public final class DynCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An unsafe cast expression (e.g., `foo as! Bar`).
public final class UnsafeCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A tuple expression (e.g., `(fst: 4, snd: 2)`).
public final class TupleExpr: Expr {

  public init(elems: [TupleElem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  /// The elements of the tuple.
  public var elems: [TupleElem]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A tuple's element, or a function call's argument.
public struct TupleElem {

  public init(label: String? = nil, value: Expr, range: SourceRange) {
    self.label = label
    self.value = value
    self.range = range
  }

  /// The label of the element.
  public var label: String?

  /// The value of the element.
  public var value: Expr

  /// The source range of this element's textual representation.
  public var range: SourceRange

}

/// A function call expression.
///
/// This represents any function application, including infix, prefix and postfix expressions.
///
/// - Note: Assignments and address-taking expressions are represented with the nodes `AssignExpr`
///   and `AddrOfExpr`, respectively. These denote a particular behavior for which it is useful to
///   have dedicate nodes.
public final class CallExpr: Expr {

  public init(fun: Expr, args: [TupleElem], type: ValType, range: SourceRange) {
    self.fun = fun
    self.args = args
    self.type = type
    self.range = range
  }

  /// The function being called.
  public var fun: Expr

  /// The arguments of the call.
  public var args: [TupleElem]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

public typealias CallArg = TupleElem

/// An identifier referring to an unresolved declaration.
///
/// This has always an unresolved type. The referred declaration must be resolved by the semantic
/// analysis, which should ultimately substitute this node with a `DeclRefExpr`.
public final class UnresolvedDeclRefExpr: Expr {

  public init(name: String, type: UnresolvedType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  /// An identifier.
  public let name: String

  public var type: ValType {
    didSet { assert(type is UnresolvedType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to an unresolved declaration, explicitly qualified by a type namespace
/// (e.g., `Builtin::bitcast`).
///
/// Conceptually, this wraps an unresolved declaration reference, providing context for the space
/// into which it points. The type prefix is resolved during name binding.
public final class UnresolvedQualDeclRefExpr: Expr {

  public init(namespace: IdentTypeRepr, name: String, type: UnresolvedType, range: SourceRange) {
    self.namespace = namespace
    self.name = name
    self.type = type
    self.range = range
  }

  /// A type identifier.
  public var namespace: IdentTypeRepr

  /// An identifier.
  public let name: String

  public var type: ValType {
    didSet { assert(type is UnresolvedType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a set of overloaded value declarations.
///
/// This is typically substituted for an `UnresolvedDeclRefExpr` after the name binding. Additional
/// contextual information is required to disambiguate the referred declaration, at which point the
/// expression can be finally substituted with a `DeclRefExpr`.
public final class OverloadedDeclRefExpr: Expr {

  public init(subExpr: Expr, declSet: [ValueDecl], type: ValType, range: SourceRange) {
    self.declSet = declSet
    self.type = type
    self.range = range
  }

  /// The set of candidate declarations for the expresssion.
  public var declSet: [ValueDecl] = []

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a resolved value declaration.
public final class DeclRefExpr: Expr {

  public init(decl: ValueDecl, type: ValType, range: SourceRange) {
    self.decl = decl
    self.type = type
    self.range = range
  }

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a resolved type declaration.
public final class TypeDeclRefExpr: Expr {

  public init(decl: TypeDecl, range: SourceRange) {
    self.decl = decl
    self.range = range
  }

  /// The declaration referred by the expresssion.
  public var decl: TypeDecl

  public var type: ValType {
    get { decl.type }
    set { assert(newValue === decl.type) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
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

  public init(base: Expr, memberName: String, type: UnresolvedType, range: SourceRange) {
    self.base = base
    self.memberName = memberName
    self.type = type
    self.range = range
  }

  /// The base expression.
  public var base: Expr

  /// The name of the member.
  public var memberName: String

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression referring to a resolved member declaration.
///
/// This does not represent expressions that denote the member of a tuple, as tuple member do not
/// have declarations. Instead, those are represented by `TupleMemberExpr`.
public final class MemberDeclRefExpr: MemberExpr {

  public init(base: Expr, decl: ValueDecl, type: ValType, range: SourceRange) {
    self.base = base
    self.decl = decl
    self.type = type
    self.range = range
  }

  /// The base expression.
  public var base: Expr

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression that refers to the member of a tuple (e.g., `(fst: 3, snd: 2).fst`).
public final class TupleMemberExpr: MemberExpr {

  public init(base: Expr, memberIndex: Int, type: ValType, range: SourceRange) {
    self.base = base
    self.memberIndex = memberIndex
    self.type = type
    self.range = range
  }

  /// The base expression.
  public var base: Expr

  /// The index of the member in the tuple.
  public var memberIndex: Int

  /// The label of the member in the tuple, if any.
  public var memberLabel: String? {
    guard let tType = type as? TupleType else { return nil }
    return tType.elems[memberIndex].label
  }

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression that is evaluated asynchronously (e.g., `async foo()`).
///
/// An async expression is a wrapper around a "future". It describes a value that might still being
/// computed, typically by a concurrent thread.
public final class AsyncExpr: Expr {

  public init(value: Expr, type: ValType, range: SourceRange) {
    self.value = value
    self.type = type
    self.range = range
  }

  /// The expression to evaluate asynchronously.
  public var value: Expr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression awaiting the result of an asynchronous value (e.g., `await foo()`).
public final class AwaitExpr: Expr {

  public init(value: Expr, type: ValType, range: SourceRange) {
    self.value = value
    self.type = type
    self.range = range
  }

  /// The asynchronous value to await.
  public var value: Expr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An expression resolving to the address of a value (e.g., `&foo.bar`).
public final class AddrOfExpr: Expr {

  public init(value: Expr, type: ValType, range: SourceRange) {
    self.value = value
    self.type = type
    self.range = range
  }

  /// The expression representing the address to resolve.
  public var value: Expr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
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

  public init(
    isSubExpr : Bool,
    subject   : Expr,
    cases     : [MatchCaseStmt],
    type      : ValType,
    range     : SourceRange
  ) {
    self.isSubExpr = isSubExpr
    self.subject = subject
    self.cases = cases
    self.type = type
    self.range = range
  }

  /// Indicates whether the match appears as a sub-expression, in a larger expression.
  public var isSubExpr: Bool

  /// The subject being matched
  public var subject: Expr

  /// The patterns against which the subject is being matched.
  public var cases: [MatchCaseStmt]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// A wildcard expression.
public final class WildcardExpr: Expr {

  public init(type: ValType, range: SourceRange) {
    self.type = type
    self.range = range
  }

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    return visitor.visit(self)
  }

}

/// An ill-formed expression.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorExpr: Expr {

  public init(type: ErrorType, range: SourceRange) {
    self.type = type
    self.range = range
  }

  public var type: ValType  {
    didSet { assert(type is ErrorType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

}
