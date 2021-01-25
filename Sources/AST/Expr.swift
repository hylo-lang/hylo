import Basic

/// An expression.
public protocol Expr: Node {

  /// The type of the expression.
  var type: ValType { get set }

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

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
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
    didSet { precondition(type is TupleType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
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

  public init(callee: Expr, args: [CallArg], type: ValType, range: SourceRange) {
    self.fun = callee
    self.args = args
    self.type = type
    self.range = range
  }

  /// The function being called.
  public var fun: Expr

  /// The arguments of the call.
  public var args: [CallArg]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A function call's argument.
public struct CallArg {

  public init(label: String? = nil, value: Expr, range: SourceRange) {
    self.label = label
    self.value = value
    self.range = range
  }

  /// The label of the argument.
  public var label: String?

  /// The value of the argument.
  public var value: Expr

  /// The source range of this argument’s textual representation.
  public var range: SourceRange

}

/// An identifier referring to an unresolved declaration.
///
/// This has always an unresolved type. The referred declaration must be resolved by the semantic
/// analysis, which should ultimately substitute this node with a `DeclRefExpr`.
public final class UnresolvedDeclRefExpr: Expr {

  public init(name: String, type: ValType, range: SourceRange) {
    precondition(type is UnresolvedType)
    self.name = name
    self.type = type
    self.range = range
  }

  /// An identifier.
  public let name: String

  public var type: ValType {
    didSet { precondition(type is UnresolvedType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to an unresolved declaration, explicitly qualified by a type namespace
/// (e.g., `Builtin::bitcast`).
///
/// Conceptually, this wraps an unresolved declaration reference, providing context for the space
/// into which it points. The type prefix is resolved during name binding.
public final class QualDeclRefExpr: Expr {

  public init(namespace: IdentTypeRepr, name: String, type: ValType, range: SourceRange) {
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
    didSet { precondition(type is UnresolvedType) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a set of overloaded value declarations.
///
/// This is typically substituted for an `UnresolvedDeclRefExpr` after the name binding. Additional
/// contextual information is required to disambiguate the referred declaration, at which point the
/// expression can be finally substituted with a `DeclRefExpr`.
public final class OverloadedDeclRefExpr: Expr {

  public init(declSet: [ValueDecl], type: ValType, range: SourceRange) {
    self.declSet = declSet
    self.type = type
    self.range = range
  }

  /// The set of candidate declarations for the expresssion.
  public var declSet: [ValueDecl] = []

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// An identifier referring to a resolved value declaration.
public final class DeclRefExpr: Expr {

  public init(decl: ValueDecl, range: SourceRange) {
    self.decl = decl
    self.range = range
  }

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public var type: ValType {
    get { decl.type }
    set { precondition(newValue === decl.type) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
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
    set { precondition(newValue === decl.type) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A member expression (e.g., `foo.bar`).
public protocol MemberExpr: Expr {

  /// The base expression.
  var base: Expr { get }

}

/// A member expression (e.g., `foo.bar`) with an unresolved type.
public final class UnresolvedMemberExpr: MemberExpr {

  public init(base: Expr, memberName: String, type: ValType, range: SourceRange) {
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

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// An expression referring to a resolved member declaration.
public final class MemberRefExpr: MemberExpr {

  public init(base: Expr, decl: ValueDecl, range: SourceRange) {
    self.base = base
    self.decl = decl
    self.range = range
  }

  /// The base expression.
  public var base: Expr

  /// The declaration referred by the expresssion.
  public var decl: ValueDecl

  public var type: ValType {
    get { decl.type }
    set { precondition(newValue === decl.type) }
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
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

  public var value: Expr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
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

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}
