/// An expression.
public protocol Expr: Node {

  /// The type of the expression.
  var type: ValType { get set }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: An expression visitor.
  func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor

  /// Calls `action` on this expression and then on each sub-expression in pre-order. Visitation
  /// stops if `action` returns false.
  ///
  /// - Returns: `true` unless one call to `action` returned `false`.
  @discardableResult
  func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool

}

extension Expr {

  /// A unique identifier.
  public typealias ID = ObjectIdentifier

  /// The declaration's unique identifier.
  public var id: ID { ObjectIdentifier(self) }

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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An assignment expression (e.g. `foo.bar = baz`).
///
/// This has always a unit type.
public final class AssignExpr: Expr {

  public var range: SourceRange?

  public var type: ValType {
    didSet { assert(type == .unit) }
  }

  /// An expression representing the storage to which `rvalue` is assigned.
  public var lvalue: Expr

  /// The value of the assignment.
  public var rvalue: Expr

  public init(lvalue: Expr, rvalue: Expr, range: SourceRange? = nil) {
    self.lvalue = lvalue
    self.rvalue = rvalue
    self.type = .unit
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(lvalue) && action(rvalue)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(value)
  }

}

/// A static cast expression (e.g., `foo as Bar`).
public final class StaticCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

}

/// An runtime checked cast expression (e.g., `foo as! Bar`).
public final class RuntimeCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

}

/// A built-in pointer cast expression (e.g., `foo as!! Bar`).
///
/// Pointer casts are used in the standard library to convert a built-in raw pointer type into a
/// function argument of a specific type.
public final class PointerCastExpr: BaseCastExpr {

  public override func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
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

  /// A collection containing just the labels of the tuple.
  public var labels: [String?] { elems.map({ $0.label }) }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    guard try action(self) else { return false }
    for elem in elems {
      guard try action(elem.value) else { return false }
    }
    return true
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

  /// The callee.
  public var callee: Expr

  /// The arguments of the call.
  public var args: [TupleElem]

  /// The notation of the call.
  public var notation: Notation

  public init(
    callee: Expr,
    args: [TupleElem],
    notation: Notation = .standard,
    type: ValType,
    range: SourceRange? = nil
  ) {
    self.callee = callee
    self.args = args
    self.notation = notation
    self.type = type
    self.range = range
  }

  /// A collection containing just the argument labels of the call.
  public var labels: [String?] { args.map({ $0.label }) }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    guard try action(self) && action(callee) else { return false }
    for arg in args {
      guard try action(arg.value) else { return false }
    }
    return true
  }

  /// Creates a prefix call.
  public static func prefix(
    fun: MemberExpr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    return CallExpr(callee: fun, args: [], notation: .prefix, type: type, range: range)
  }

  /// Creates a postfix call.
  public static func postfix(
    fun: MemberExpr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    return CallExpr(callee: fun, args: [], notation: .postfix, type: type, range: range)
  }

  /// Creates an infix call.
  public static func infix(
    fun: MemberExpr,
    operand: Expr,
    type: ValType,
    range: SourceRange? = nil
  ) -> CallExpr {
    let arg = CallArg(value: operand, range: operand.range)
    return CallExpr(callee: fun, args: [arg], notation: .prefix, type: type, range: range)
  }

}

public typealias CallArg = TupleElem

/// An unspecialized reference to a declaration.
public protocol BareDeclRefExpr: Expr {}

/// An identifier referring to an unresolved declaration.
///
/// This node always an unresolved type. The referred declaration must be resolved by the semantic
/// analysis, which should ultimately substitute this node with a `DeclRefExpr`.
public final class UnresolvedDeclRefExpr: BareDeclRefExpr {

  public var range: SourceRange?

  public var type: ValType {
    didSet { assert(type == .unresolved) }
  }

  /// The unqualified (possibly labeled) identifier of the referred declaration.
  public var ident: LabeledIdent

  public init(ident: LabeledIdent, type: UnresolvedType, range: SourceRange? = nil) {
    self.ident = ident
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An identifier referring to an unresolved declaration, explicitly qualified by a namespace
/// (e.g., `Builtin::bitcast`).
///
/// Conceptually, this node wraps an unresolved declaration reference, providing context for the
/// wraps into which it points. The type prefix is resolved during name binding.
public final class UnresolvedQualDeclRefExpr: BareDeclRefExpr {

  public var range: SourceRange?

  public var type: ValType {
    didSet { assert(type == .unresolved) }
  }

  /// A type signature describing the qualifying namespace.
  public var namespace: Sign

  /// The unqualified (possibly labeled) identifier of the referred declaration.
  public var ident: LabeledIdent

  /// The range of the identifier.
  public var identRange: SourceRange?

  public init(
    namespace: Sign,
    ident: LabeledIdent,
    identRange: SourceRange? = nil,
    type: UnresolvedType,
    range: SourceRange? = nil
  ) {
    self.namespace = namespace
    self.ident = ident
    self.identRange = identRange
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An identifier referring to a set of overloaded value declarations.
///
/// This node is substituted for an `DeclRefExpr` by the type checker if it can disambiguate the
/// referred declaration.
public final class OverloadedDeclRefExpr: BareDeclRefExpr {

  public var range: SourceRange?

  public var type: ValType

  /// The set of candidate declarations for the expresssion.
  public var declSet: [ValueDecl] = []

  public init(declSet: [ValueDecl], range: SourceRange? = nil) {
    self.declSet = declSet
    self.type = .unresolved
    self.range = range
  }

  @available(*, deprecated)
  public convenience init(declSet: [ValueDecl], type: ValType, range: SourceRange? = nil) {
    self.init(declSet: declSet, range: range)
    self.type = type
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An identifier referring to a resolved value declaration.
public final class DeclRefExpr: BareDeclRefExpr {

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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An identifier referring to a type kind (e.g., `Int.self`).
@available(*, deprecated)
public final class KindRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType {
    willSet { assert(newValue is KindType) }
  }

  public init(type: KindType, range: SourceRange? = nil) {
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// A member expression (e.g., `foo.bar`).
public protocol MemberExpr: BareDeclRefExpr {

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

  /// The unqualified (possibly labeled) identifier of the referred declaration.
  public var ident: LabeledIdent

  /// The range of the identifier.
  public var identRange: SourceRange?

  public init(
    base: Expr,
    ident: LabeledIdent,
    identRange: SourceRange? = nil,
    type: UnresolvedType,
    range: SourceRange? = nil
  ) {
    self.base = base
    self.ident = ident
    self.identRange = identRange
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(base)
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

  public init(base: Expr, decl: ValueDecl, range: SourceRange? = nil) {
    self.base = base
    self.decl = decl
    self.type = .unresolved
    self.range = range
  }

  @available(*, deprecated)
  public convenience init(base: Expr, decl: ValueDecl, type: ValType, range: SourceRange? = nil) {
    self.init(base: base, decl: decl, range: range)
    self.type = type
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(base)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(base)
  }

}

/// A declaration reference with type arguments (e.g., `Array<Int64>`).
public final class SpecializedDeclRefExpr: Expr {

  public var range: SourceRange?

  public var type: ValType

  /// The reference to the unspecialized declaration.
  public var unspecialized: BareDeclRefExpr

  /// The type arguments.
  public var args: [Sign]

  public init(
    unspecialized: BareDeclRefExpr,
    args: [Sign],
    type: UnresolvedType,
    range: SourceRange? = nil
  ) {
    self.unspecialized = unspecialized
    self.args = args
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
        rawType = TypeVar(node: param)
        param.state = .realized
      }

      param.type = FunParamType(policy: param.policy, rawType: rawType)
      params.append(FunType.Param(label: param.label, type: rawType))
    }

    // Realize the return type.
    let retType = decl.retSign?.realize(unqualifiedFrom: decl) ?? TypeVar()

    decl.type = FunType(params: params, retType: retType)
    decl.state = .realized
    return decl.type
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(value)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(value)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self) && action(subject)
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
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}

/// An ill-formed expression.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorExpr: Expr {

  public var range: SourceRange?

  public var type: ValType  {
    didSet { assert(type == .error) }
  }

  public init(range: SourceRange? = nil) {
    self.type = .error
    self.range = range
  }

  public init(replacing expr: Expr) {
    self.type = .error
    self.range = expr.range
  }

  public func accept<V>(_ visitor: inout V) -> V.ExprResult where V: ExprVisitor {
    visitor.visit(self)
  }

  @discardableResult
  public func forEach(_ action: (Expr) throws -> Bool) rethrows -> Bool {
    try action(self)
  }

}
