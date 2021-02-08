import Basic

/// The base class for all semantic types.
///
/// Types are uniqued in the AST context. Once created, they remain immutable.
public class ValType {

  /// Create a new type.
  init(context: Context, props: RecursiveProps) {
    self.context = context
    self.props = props
  }

  /// The AST context in which this type was uniqued.
  public unowned let context: Context

  /// A set of recursively defined properties.
  public let props: RecursiveProps

  /// The type is in canonical form.
  public var isCanonical: Bool { props.contains(.isCanonical) }

  /// The type contains one or more type variables.
  public var hasVariables: Bool { props.contains(.hasVariables) }

  /// The type contains one or more generic type parameters.
  public var hasTypeParams: Bool { props.contains(.hasTypeParams) }

  /// The type contains the unresolved type.
  public var hasUnresolved: Bool { props.contains(.hasUnresolved) }

  /// The type contains the error type.
  public var hasErrors: Bool { props.contains(.hasErrors) }

  /// The type is well-formed; it does not contain type variables, unresolved or error types.
  public var isWellFormed: Bool { !hasVariables && !hasUnresolved && !hasErrors }

  /// The kind of the type.
  public var kind: KindType { context.kindType(type: self) }

  /// The canonical form of the type, where all sugars have been stripped off.
  public var canonical: ValType { self }

  public func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    fatalError("unreachable")
  }

  /// Returns whether this type is equal to another one.
  ///
  /// This method is intended to be overridden in derived classes. It is used internally to
  /// uniquing new instances.
  func isEqual(to other: ValType) -> Bool {
    return self === other
  }

  /// Hashes the essential components of type into the given hasher.
  func hash(into hasher: inout Hasher) {
  }

  /// An internal witness of `ValType`'s conformance to `Hashable`.
  struct HashWitness: Basic.HashWitness {

    typealias Value = ValType

    static func hash(_ value: ValType, into hasher: inout Hasher) {
      value.hash(into: &hasher)
    }

    static func equals(_ lhs: ValType, _ rhs: ValType) -> Bool {
      return lhs.isEqual(to: rhs)
    }

  }

  /// A set of recursively defined properties.
  public struct RecursiveProps {

    public init(value: UInt = 0) {
      self.value = value
    }

    public init<S>(_ flags: S) where S: Sequence, S.Element == RecursiveProps {
      self.value = flags.reduce(0, { $0 | $1.value })
    }

    private let value: UInt

    /// Returns whether the set contains all the specified properties.
    ///
    /// - Parameter props: A set of properties.
    public func contains(_ props: RecursiveProps) -> Bool {
      return (value & props.value) == props.value
    }

    /// Returns the union of this property set with another.
    ///
    /// This computes the intersection of the universal properties and the union of the existential
    /// properties that are defined in each property set.
    ///
    /// - Parameter other: Another property set.
    public func merged(with other: RecursiveProps) -> RecursiveProps {
      // A type expression is canonical only if *all* sub-expressions are canonical, whereas other
      // properties are existential only.
      return RecursiveProps(
        value: ~1 & (value | other.value) | (value & other.value))
    }

    /// Returns this set without the given properties.
    ///
    /// - Parameter props: The properties to remove.
    public func removing(_ props: RecursiveProps) -> RecursiveProps {
      return RecursiveProps(value: value & ~props.value)
    }

    public static let isCanonical   = RecursiveProps(value: 1 << 0)
    public static let hasVariables  = RecursiveProps(value: 1 << 1)
    public static let hasTypeParams = RecursiveProps(value: 1 << 2)
    public static let hasUnresolved = RecursiveProps(value: 1 << 3)
    public static let hasErrors     = RecursiveProps(value: 1 << 4)

    /// Merges a collection of recursive properties.
    ///
    /// This computes the intersection of all universal properties and the union of all existential
    /// properties, over the property sets passed as an argument.
    ///
    /// - Parameter collection: A collection of property sets.
    public static func merge<C>(_ collection: C) -> RecursiveProps
    where C: Collection, C.Element == RecursiveProps
    {
      guard var result = collection.first else { return RecursiveProps(value: 0) }
      for props in collection.dropFirst() {
        result = result.merged(with: props)
      }
      return result
    }

  }

}

extension ValType: Equatable {

  public static func == (lhs: ValType, rhs: ValType) -> Bool {
    return lhs.canonical === rhs.canonical
  }

}

/// A kind type (i.e., the type of a type).
public final class KindType: ValType {

  init(context: Context, type: ValType) {
    self.type = type
    super.init(context: context, props: type.props)
  }

  /// The type constructed by this kind.
  public let type: ValType

  public override var canonical: ValType {
    return isCanonical
      ? self
      : type.canonical.kind
  }

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? KindType else { return false }
    return self.type.isEqual(to: that.type)
  }

  override func hash(into hasher: inout Hasher) {
    withUnsafeBytes(of: KindType.self, { hasher.combine(bytes: $0) })
    type.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension KindType: CustomStringConvertible {

  public var description: String {
    return "\(type)::Kind"
  }

}

/// A built-in type.
///
/// Built-in types are not "defined" anywhere. They are created on demand by the AST context.
public class BuiltinType: ValType {

  init(context: Context, name: String) {
    self.name = name
    super.init(context: context, props: .isCanonical)
  }

  /// The name of the type.
  public let name: String

  override func hash(into hasher: inout Hasher) {
    withUnsafeBytes(of: BuiltinType.self, { hasher.combine(bytes: $0) })
    hasher.combine(name)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension BuiltinType: CustomStringConvertible {

  public var description: String { name }

}

/// A built-in literal type.
public protocol BuiltinLiteral {}

/// A built-in integer literal type.
///
/// This is the type of an integer literal expression.
public final class BuiltinIntLiteralType: BuiltinType, BuiltinLiteral {

  init(context: Context) {
    super.init(context: context, name: "IntLiteral")
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A built-in integer type.
///
/// Built-in integer types corresponds directly to the target's integer types. They can be of any
/// bitwidth and do not specify signedness.
///
/// This type does *not* correspond to Val's `Int`. The latter is an actual Val type, defined in
/// the standard library, which wrap a built-in integer type.
public final class BuiltinIntType: BuiltinType {

  init(context: Context, name: String, bitWidth: Int) {
    self.bitWidth = bitWidth
    super.init(context: context, name: name)
  }

  /// The number of bits in the binary representation of values of this type.
  public let bitWidth: Int

  override func hash(into hasher: inout Hasher) {
    withUnsafeBytes(of: BuiltinIntType.self, { hasher.combine(bytes: $0) })
    hasher.combine(bitWidth)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// The type of a module.
///
/// This is the type given to a module value, typically given to the base of a qualified identifier
/// (e.g., `Builtin` in `Builtin::i32`).
public final class ModuleType: ValType {

  init(context: Context, module: Module) {
    self.module = module
    super.init(context: context, props: .isCanonical)
  }

  /// The module corresponding to this type.
  public unowned let module: Module

  override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(module))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension ModuleType: CustomStringConvertible {

  public var description: String { module.name }

}

/// A nominal type.
public class NominalType: ValType, CustomStringConvertible {

  init(context: Context, decl: NominalTypeDecl, props: RecursiveProps = .isCanonical) {
    self.decl = decl
    super.init(context: context, props: props)
  }

  /// The declaration of this nominal type.
  public unowned let decl: NominalTypeDecl

  override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public var description: String { decl.name }

}

/// A product type, representing a collection of labeled value members.
public final class ProductType: NominalType {

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ProductType else { return false }
    return self.decl === that.decl
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A view type.
public final class ViewType: NominalType {

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ViewType else { return false }
    return self.decl === that.decl
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A type whose generic parameters have been bound.
public final class BoundGenericType: NominalType {

  init(context: Context, decl: NominalTypeDecl, args: [ValType]) {
    self.args = args
    super.init(context: context, decl: decl, props: RecursiveProps.merge(args.map({ $0.props })))
  }

  /// The arguments provided for the underyling type's generic parameters.
  public let args: [ValType]

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? BoundGenericType else { return false }

    guard self.decl === that.decl else { return false }
    guard self.args.count == that.args.count else { return false }
    for (lhs, rhs) in zip(self.args, that.args) {
      guard (lhs.isEqual(to: rhs)) else { return false }
    }
    return true
  }

  override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
    for arg in args {
      arg.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    let args = self.args.map(String.init(describing:)).joined(separator: ", ")
    return "\(decl.name)<\(args)>"
  }

}

/// A generic parameter type.
public final class GenericParamType: ValType, Hashable {

  init(context: Context, decl: GenericParamDecl) {
    self.decl = decl
    super.init(context: context, props: RecursiveProps([.isCanonical, .hasTypeParams]))
  }

  /// The declaration of this generic parameter type.
  public unowned let decl: GenericParamDecl

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? GenericParamType else { return false }
    return self.decl === that.decl
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension GenericParamType: CustomStringConvertible {

  public var description: String { decl.name }

}

/// An existential type.
///
/// This denotes an unspecified type that is known to conform to a set of views at runtime. It is
/// used to represent generic parameters within their generic context, as well as the type of
/// existential containers.
///
/// - Note: The `Any` is an existential type.
public final class ExistentialType: ValType {

  init(context: Context, interface: ValType, genericEnv: GenericEnv) {
    self.interface = interface
    self.genericEnv = genericEnv
    super.init(context: context, props: .isCanonical)
  }

  /// The interface type of this existential.
  public unowned let interface: ValType

  /// The generic environment in which this existential resides.
  public unowned let genericEnv: GenericEnv

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ExistentialType else { return false }
    return self.interface.isEqual(to: that.interface)
  }

  override func hash(into hasher: inout Hasher) {
    withUnsafeBytes(of: ExistentialType.self, { hasher.combine(bytes: $0) })
    interface.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension ExistentialType: CustomStringConvertible {

  public var description: String {
    return "∃X.\(interface)"
  }

}

/// A tuple type.
public final class TupleType: ValType {

  init(context: Context, elems: [Elem]) {
    self.elems = elems

    // Compute the type's recursive properties.
    var props = RecursiveProps.merge(elems.map({ $0.type.props }))
    if (elems.count == 1) && (elems[0].label == nil) {
      props = props.removing(.isCanonical)
    }

    super.init(context: context, props: props)
  }

  /// The elements of the tuple.
  public let elems: [Elem]

  public override var canonical: ValType {
    if isCanonical {
      return self
    }
    if (elems.count == 1) && elems[0].label == nil {
      return elems[0].type.canonical
    }
    return context.tupleType(elems.map({ elem in
      Elem(label: elem.label, type: elem.type.canonical)
    }))
  }

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? TupleType else { return false }

    guard elems.count == that.elems.count else { return false }
    for (lhs, rhs) in zip(elems, that.elems) {
      guard (lhs.label == rhs.label) && (lhs.type.isEqual(to: rhs.type)) else { return false }
    }
    return true
  }

  override func hash(into hasher: inout Hasher) {
    for elem in elems {
      hasher.combine(elem.label)
      elem.type.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  /// An element in a tuple type.
  public struct Elem: Equatable {

    public init(label: String? = nil, type: ValType) {
      self.label = label
      self.type = type
    }

    /// The label of the element, if any.
    public let label: String?

    /// The type of the element.
    public let type: ValType

    public static func == (lhs: Elem, rhs: Elem) -> Bool {
      return (lhs.label == rhs.label) && (lhs.type == rhs.type)
    }

  }

}

extension TupleType: CustomStringConvertible {

  public var description: String {
    let elems = self.elems.map(String.init(describing:)).joined(separator: ", ")
    return "(\(elems))"
  }

}

extension TupleType.Elem: CustomStringConvertible {

  public var description: String {
    if let label = self.label {
      return "\(label): \(type)"
    } else {
      return String(describing: type)
    }
  }

}

/// A function type.
public final class FunType: ValType {

  init(context: Context, paramType: ValType, retType: ValType) {
    self.paramType = paramType
    self.retType = retType
    super.init(context: context, props: paramType.props.merged(with: retType.props))
  }

  /// The function's domain.
  public let paramType: ValType

  /// The function's codomain.
  public let retType: ValType

  public override var canonical: ValType {
    return isCanonical
      ? self
      : context.funType(paramType: paramType.canonical, retType: retType.canonical)
  }

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? FunType,
          retType.isEqual(to: that.retType),
          paramType.isEqual(to: that.paramType)
    else { return false }
    return true
  }

  override func hash(into hasher: inout Hasher) {
    paramType.hash(into: &hasher)
    retType.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension FunType: CustomStringConvertible {

  public var description: String {
    return "\(paramType) -> \(retType)"
  }

}

/// An "inout" type (e.g. `mut Int`)
///
/// Inout types can only appear as function parameters. They specify that arguments are expected to
/// be passed by reference (using the `&` prefix operator). They also denote the implicit receiver
/// type of a mutating method.
public final class InoutType: ValType {

  init(context: Context, base: ValType) {
    self.base = base
    super.init(context: context, props: base.props)
  }

  /// A type.
  public let base: ValType

  public override var canonical: ValType {
    return isCanonical
      ? self
      : context.inoutType(of: base.canonical)
  }

  override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? InoutType,
          base.isEqual(to: that.base)
    else { return false }
    return true
  }

  override func hash(into hasher: inout Hasher) {
    withUnsafeBytes(of: InoutType.self, { hasher.combine(bytes: $0) })
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

extension InoutType: CustomStringConvertible {

  public var description: String { "mut \(base)" }

}

/// An unresolved type.
///
/// This is used internally to denote the type of an unresolved declaration reference.
public final class UnresolvedType: ValType {

  init(context: Context) {
    super.init(context: context, props: RecursiveProps([.isCanonical, .hasUnresolved]))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// The type of an ill-formed declaration or expression.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorType: ValType {

  init(context: Context) {
    super.init(context: context, props: RecursiveProps([.isCanonical, .hasErrors]))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A type variable.
public final class TypeVar: ValType, Hashable {

  public init(context: Context, node: Node? = nil) {
    self.id = TypeVar.createID()
    self.node = node
    super.init(context: context, props: RecursiveProps([.isCanonical, .hasVariables]))
  }

  /// The variable's identifier.
  public let id: Int

  /// The optional node representing the sub-expression with which the variable is associated.
  public private(set) weak var node: Node?

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  private static var nextID = 0

  private static func createID() -> Int {
    nextID += 1
    return nextID
  }

}

extension TypeVar: CustomStringConvertible {

  public var description: String { "τ\(id)" }

}
