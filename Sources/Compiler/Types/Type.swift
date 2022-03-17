import Utils

/// The base class for all semantic types.
///
/// Types are uniqued in the AST context. Once created, they remain immutable.
public class ValType: CustomStringConvertible, Equatable {

  /// An internal witness of `ValType`'s conformance to `Hashable`.
  struct HashWitness: Utils.HashWitness {

    typealias Value = ValType

    static func hash(_ value: ValType, into hasher: inout Hasher) { value.hash(into: &hasher) }

    static func equals(_ lhs: ValType, _ rhs: ValType) -> Bool { lhs.isEqual(to: rhs) }

  }

  /// A set of type flags.
  public struct Flags: Equatable, ExpressibleByArrayLiteral {

    private let rawValue: UInt

    private init(rawValue: UInt) {
      self.rawValue = rawValue
    }

    public init<S>(_ flags: S) where S: Sequence, S.Element == Flags {
      self.rawValue = flags.reduce(0, { $0 | $1.rawValue })
    }

    public init(arrayLiteral elements: Flags...) {
      self = Flags(elements)
    }

    /// Returns whether the set contains all the specified flags.
    ///
    /// - Parameter flags: A set of type flags.
    public func contains(_ flags: Flags) -> Bool {
      (rawValue & flags.rawValue) == flags.rawValue
    }

    /// Returns this set of type flags merged with another one.
    ///
    /// Merging is defined as the intersection of the universal flags and the union of the
    /// existential flags that are defined in each set.
    ///
    /// - Parameter other: Another set of type flags.
    public func merged(with other: Flags) -> Flags {
      // `isCanonical` is universal; other flags are existential.
      Flags(rawValue: ~1 & (rawValue | other.rawValue) | (rawValue & other.rawValue))
    }

    /// Returns the union of this set with another.
    ///
    /// - Parameter other: Another set of type flags.
    public func union(with other: Flags) -> Flags {
      Flags(rawValue: rawValue | other.rawValue)
    }

    /// Returns this set without the specified flags.
    ///
    /// - Parameter flags: The flags to remove.
    public func removing(_ flags: Flags) -> Flags {
      Flags(rawValue: rawValue & ~flags.rawValue)
    }

    public static let isCanonical   = Flags(rawValue: 1 << 0)
    public static let hasAsync      = Flags(rawValue: 1 << 1)
    public static let hasAlias      = Flags(rawValue: 1 << 2)
    public static let hasVariables  = Flags(rawValue: 1 << 3)
    public static let hasTypeParams = Flags(rawValue: 1 << 4)
    public static let hasSkolems    = Flags(rawValue: 1 << 5)
    public static let hasUnresolved = Flags(rawValue: 1 << 6)
    public static let hasErrors     = Flags(rawValue: 1 << 7)

    /// Merges a collection of flags.
    ///
    /// Merging is defined as the intersection of the universal flags and the union of the
    /// existential flags that are defined in each set.
    ///
    /// - Parameter collection: A collection of set of type flags.
    public static func merge<C>(_ collection: C) -> Flags
    where C: Collection, C.Element == Flags
    {
      guard var result = collection.first else { return Flags(rawValue: 0) }
      for flags in collection.dropFirst() {
        result = result.merged(with: flags)
      }
      return result
    }

  }

  /// A set of type flags, defined recursively.
  public let flags: Flags

  /// Create a new type.
  fileprivate init(flags: Flags) {
    self.flags = flags
  }

  /// Returns whether the specified flags are raised on this type.
  ///
  /// - parameter flags: A set of type flags.
  public subscript(flags: Flags) -> Bool { self.flags.contains(flags) }

  /// Indicates whether the type is well-formed (i.e., it does not contain variables, unresolved
  /// types, or error types).
  public var isWellFormed: Bool {
    !self[.hasVariables] && !self[.hasUnresolved] && !self[.hasErrors]
  }

  /// Indicates whether the type is existential.
  ///
  /// A type is existential if it cannot be resolved to a concrete representation statically, but
  /// it is known to represent a runtime type that satisfies a set of requirements. That does *not*
  /// include type variables, which have yet to be inferred as actual (potentially conrete) types.
  ///
  /// Instances of existential types are represented by existential packages.
  public final var isExistential: Bool {
    switch canonical {
    case is ViewType, is ViewCompositionType, is UnionType, is SkolemType, is GenericParamType:
      return true
    case let type as KindType:
      return type.base.isExistential
    case let type as BoundGenericType where type.decl is AliasTypeDecl:
      return type.canonical.isExistential
    case let type as AssocType:
      return type.base.isExistential
    case let type as FunParamType:
      return type.rawType.isExistential
    default:
      return false
    }
  }

  /// The kind of the type.
  public var kind: KindType { KindType(base: self) }

  /// The canonical form of the type.
  public var canonical: ValType { self }

  /// The uncontextualized interface of this type, that is the type in which all skolems have been
  /// substituted by their interface type.
  public var uncontextualized: ValType { self }

  /// Returns this type after substituting generic type parameters with the given mapping.
  ///
  /// - Parameter args: A substitution table.
  public final func specialized(with args: [GenericParamType: ValType]) -> ValType {
    if self[.hasTypeParams] {
      var specializer = TypeSpecializer(args: args)
      return specializer.walk(self)
    } else {
      return self
    }
  }

  /// Looks up for member declarations that match the given name.
  ///
  /// - Parameter memberName: The bare name of the member to search.
  public func lookup(member memberName: String) -> LookupResult {
    return LookupResult()
  }

  /// Looks up for type member declarations that match the given name.
  ///
  /// - Parameter memberName: The bare name of the type member to search.
  public func lookup(typeMember memberName: String) -> [TypeDecl] {
    return lookup(member: memberName).types
  }

  /// Returns whether this type matches another one.
  ///
  /// This walks down the type expression in parallel with the argument, calling `reconcile` for
  /// each mismatching pair.
  ///
  /// - Parameters:
  ///   - other: Another type.
  ///   - reconcile: A closure that accepts two non-equal types at the same structural position and
  ///     returns `true` if they should match nonetheless.
  /// - Returns: `false` if any call to `reconcile` returns `false`; otherwise, `true`.
  public func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    return (self == other) || reconcile(self, other)
  }

  /// Returns whether this type is subtype of another one.
  ///
  /// - Warning: This method relies on the current state of the AST; it will produce inaccurate
  ///   results until type checking completes.
  ///
  /// - Parameter other: Another type.
  public func isSubtype(of other: ValType) -> Bool {
    switch other {
    case self:
      return true
    case .any:
      return true
    case let that as UnionType:
      return that.elems.contains(where: isSubtype(of:))
    default:
      return false
    }
  }

  /// Returns whether this type is equal to another one.
  ///
  /// This method is used internally for uniquing type instances. All derived classes should
  /// override it to implement their one value equality logic. The default implementation checks
  /// for reference equality.
  ///
  /// - Parameter other: Another type.
  fileprivate func isEqual(to other: ValType) -> Bool { self === other }

  /// Hashes the essential components of type into the given hasher.
  ///
  /// This method is used internally for uniquing type instances. All derived classes should
  /// override it to implement their own hashing logic. The default implementation is a no-op.
  ///
  /// - Parameter hasher: The hasher to use when combining the components of this type.
  public func hash(into hasher: inout Hasher) {}

  public func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    fatalError("unreachable")
  }

  public var description: String { String(describing: type(of: self)) }

  public static func == (lhs: ValType, rhs: ValType) -> Bool { lhs.isEqual(to: rhs) }

}

extension ValType {

  /// Val's `Unit` type.
  public static let unit = TupleType([])

  /// Val's `Any` type.
  public static let any = ViewCompositionType([])

  /// Val's `Nothing` type.
  public static let nothing = UnionType([])

  /// The unresolved type.
  public static let unresolved = UnresolvedType.instance

  /// The error type.
  public static let error = ErrorType.instance

}

/// A simple type walker that substitutes generic arguments for their corresponding parameters.
struct TypeSpecializer: TypeWalker {

  var parent: ValType?

  /// The specialization arguments to apply.
  private let args: [GenericParamType: ValType]

  public init(args: [GenericParamType: ValType]) {
    self.args = args
  }

  public func willVisit(_ type: ValType) -> TypeWalkerAction {
    if let param = type as? GenericParamType {
      return .stepOver(args[param] ?? param)
    }

    return type[.hasTypeParams]
      ? .stepInto(type)
      : .stepOver(type)
  }

}

/// Converts the given type to a string suitable to be inserted into the textual representation
/// of a type expression.
///
/// - Parameter type: A type.
fileprivate func stringify(_ type: ValType) -> String {
  let desc = String(describing: type)
  return desc.contains(" ")
    ? "(\(desc))"
    : desc
}

/// A kind type (i.e., the type of a type).
public final class KindType: ValType {

  /// The type constructed by this kind.
  public let base: ValType

  public init(base: ValType) {
    self.base = base
    super.init(flags: base.flags)
  }

  public override var canonical: ValType {
    self[.isCanonical] ? self : base.canonical.kind
  }

  public override var uncontextualized: ValType {
    !self[.hasSkolems] ? self : base.uncontextualized.kind
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other {
      return true
    } else if let that = other as? KindType {
      return base.matches(with: that.base, reconcilingWith: reconcile)
    } else {
      return reconcile(self, other)
    }
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other {
    case let that as KindType:
      return base.isSubtype(of: that.base)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? KindType {
      return base.isEqual(to: that.base)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(KindType.self))
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { stringify(base) + "::Kind" }

}

/// A built-in type.
///
/// Built-in types are not "defined" anywhere. They are created on demand by the AST context.
public class BuiltinType: ValType {

  /// The name of the type.
  public let name: String

  fileprivate init(name: String) {
    self.name = name
    super.init(flags: .isCanonical)
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? BuiltinType {
      return self.name == that.name
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(name)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { name }

}

extension BuiltinType {

  /// Returns the built-in type with the specified name.
  public static func get(name: String) -> BuiltinType? {
    if name == "Pointer" {
      return BuiltinPointerType.instance
    }

    if name == "IntLiteral" {
      return BuiltinIntLiteralType.instance
    }

    if name.starts(with: "i") {
      if let bitWidth = Int(name.dropFirst()), (bitWidth > 0) && (bitWidth <= 64) {
        return BuiltinIntType(bitWidth: bitWidth)
      }
    }

    return nil
  }

}

/// A built-in pointer type.
public final class BuiltinPointerType: BuiltinType {

  private init() {
    super.init(name: "Pointer")
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  static let instance = BuiltinPointerType()

}

/// A built-in literal type.
public protocol BuiltinLiteral {}

/// A built-in integer literal type.
///
/// This type is used during type checking to infer the type of an literal expression.
public final class BuiltinIntLiteralType: BuiltinType, BuiltinLiteral {

  private init() {
    super.init(name: "IntLiteral")
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public static let instance = BuiltinIntLiteralType()

}

/// A built-in integer type.
///
/// This type represents the target's integer types. These can be of any bitwidth and do not
/// specify signedness.
///
/// - Note: This type does *not* correspond to Val's `Int`. The latter is an actual Val type,
///   defined in the standard library, which wrap a built-in integer type.
public final class BuiltinIntType: BuiltinType {

  /// The number of bits in the binary representation of values of this type.
  public let bitWidth: Int

  public init?(bitWidth: Int) {
    assert((bitWidth > 0) && (bitWidth <= 64))
    self.bitWidth = bitWidth
    super.init(name: "i\(bitWidth)")
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? BuiltinIntType else { return false }
    return bitWidth == that.bitWidth
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(bitWidth)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public static let i1 = BuiltinIntType(bitWidth: 1)!

}

/// The type of a module.
public final class ModuleType: ValType {

  /// The module corresponding to this type.
  public unowned let decl: ModuleDecl

  public init(decl: ModuleDecl) {
    self.decl = decl
    super.init(flags: .isCanonical)
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? ModuleType {
      return decl === that.decl
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { decl.ident }

}

/// The type of a namespace.
public final class NamespaceType: ValType {

  /// The namespace corresponding to this type.
  public unowned let decl: NamespaceDecl

  public init(decl: NamespaceDecl) {
    self.decl = decl
    super.init(flags: .isCanonical)
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? NamespaceType {
      return decl === that.decl
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { decl.ident }

}

/// A nominal type.
public class NominalType: ValType {

  /// The declaration of this nominal type.
  public unowned let decl: GenericTypeDecl

  fileprivate init(decl: GenericTypeDecl, flags: Flags) {
    self.decl = decl
    super.init(flags: flags)
  }

  public override func lookup(member memberName: String) -> LookupResult {
    return decl.lookup(qualified: memberName)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.canonical {
    case let that as ViewType:
      return decl.conformanceTable[that]?.state == .checked
    case let that as ViewCompositionType:
      return that.elems.allSatisfy({ decl.conformanceTable[$0]?.state == .checked })
    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? NominalType {
      return decl === that.decl
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override var description: String { decl.ident }

}

/// A product type, representing a collection of labeled value members.
public final class ProductType: NominalType {

  public init(decl: ProductTypeDecl) {
    super.init(decl: decl, flags: .isCanonical)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A view type.
public final class ViewType: NominalType {

  public init(decl: ViewTypeDecl) {
    super.init(decl: decl, flags: .isCanonical)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  /// Determines if a view must appear before another one in a canonical composition, based on
  /// module and name.
  fileprivate static func precedes(lhs: ViewType, rhs: ViewType) -> Bool {
    guard lhs.decl !== rhs.decl else { return false }

    // FIXME: Use all components of the FQNs.
    if lhs.decl.rootDeclSpace === rhs.decl.rootDeclSpace {
      return lhs.decl.ident.lexicographicallyPrecedes(rhs.decl.ident)
    } else {
      return lhs.decl.rootDeclSpace.ident.lexicographicallyPrecedes(rhs.decl.rootDeclSpace.ident)
    }
  }

}

/// A type alias denoting a possibly generic type expression.
public final class AliasType: NominalType {

  public init(decl: AliasTypeDecl) {
    assert(decl.state >= .realized, "can't create alias type from unrealized declaration")

    let flags: Flags
    if let type = decl.aliasedSign.type as? NominalType {
      flags = type.flags.merged(with: .hasAlias).removing(.isCanonical)
    } else {
      flags = [.isCanonical, .hasAlias]
    }
    super.init(decl: decl, flags: flags)
  }

  /// The nominal type represented by this alias.
  ///
  /// If the alias denotes a synonym, its delegate is the nominal type to which it ultimately
  /// refers. If the alias denotes a type definition, the alias is its own delegate.
  public var delegate: NominalType {
    var nominal: NominalType = self
    while let alias = nominal as? AliasType {
      guard let next = (alias.decl as! AliasTypeDecl).aliasedSign.type as? NominalType else {
        return alias
      }
      nominal = next
    }
    return nominal
  }

  public override var canonical: ValType {
    if self[.isCanonical] {
      return self
    } else {
      return delegate.canonical
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? AliasType {
      return self.decl === that.decl
    } else {
      return false
    }
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A view composition type.
///
/// This is the type of an existential package whose witness is known to conform to a set of views.
/// If this set is empty, then the witness can be any (synchronous) type (a.k.a., `Any`).
public final class ViewCompositionType: ValType {

  /// The views that are part of the compositio.
  public let elems: [ViewType]

  init<S>(_ elems: S) where S: Sequence, S.Element == ViewType {
    let elems = Array(elems)

    // Determine canonicity.
    var flags: Flags
    switch elems.count {
    case 0:
      // This is the `Any` type.
      flags = .isCanonical

    case 1:
      // The canonical form of a composition with a unique view is the view itself.
      flags = elems[0].flags.removing(.isCanonical)

    default:
      // The composition is canonical if the views are "sorted".
      // FIXME: We should also remove duplicate views.
      flags = .merge(elems.map({ $0.flags }))
      for i in 1 ..< elems.count {
        guard ViewType.precedes(lhs: elems[i - 1], rhs: elems[i]) else {
          flags = flags.removing(.isCanonical)
          break
        }
      }
    }

    self.elems = elems
    super.init(flags: flags)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch elems.count {
    case 0:
      return self === other.canonical
    case 1:
      return elems[0].isSubtype(of: other)
    default:
      return elems.allSatisfy({ $0.isSubtype(of: other) })
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ViewCompositionType,
          elems.count == that.elems.count
    else { return false }

    for (lhs, rhs) in zip(elems, that.elems) {
      guard lhs.isEqual(to: rhs) else { return false }
    }
    return true
  }

  public override func hash(into hasher: inout Hasher) {
    for elem in elems {
      elem.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    if elems.isEmpty {
      return "Any"
    } else {
      return elems.map(stringify(_:)).joined(separator: " & ")
    }
  }

}

/// A union type (e.g. `A | B`).
public final class UnionType: ValType {

  /// The members of the union.
  ///
  /// Uniqueness of each element is not guaranteed, unless the union type is canonical.
  public let elems: [ValType]

  init<S>(_ elems: S) where S: Sequence, S.Element == ValType {
    let elems = Array(elems)
    assert(
      elems.allSatisfy({ !($0 is TypeVar) }),
      "unconstrained type variables cannot occur in union type")

    // Determine canonicity.
    var flags: Flags
    switch elems.count {
    case 0:
      // This is the `Nothing` (a.k.a., uninhabited) type.
      flags = .isCanonical

    case 1:
      // The canonical form of a union with a unique type element is the element itself.
      flags = elems[0].flags.removing(.isCanonical)

    default:
      flags = elems[0].flags

      // Determines canonicity.
      for i in 1 ..< elems.count {
        flags = flags.merged(with: elems[i].flags)

        // The union is not canonical if it's not flat (e.g., `A | (B | C)`).
        if elems[i] is UnionType {
          flags = flags.removing(.isCanonical)
        }

        // The union is not canonical if it's not sorted.
        // FIXME: Do we need a more stable ordering, independent of the compiler's runtime?
        if ObjectIdentifier(elems[i - 1]) >= ObjectIdentifier(elems[i]) {
          flags = flags.removing(.isCanonical)
        }
      }
    }

    self.elems = elems
    super.init(flags: flags)
  }

  public override var canonical: ValType {
    return UnionType.create(unionOf: elems.map({ $0.canonical }))
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch elems.count {
    case 0:
      return true
    case 1:
      return elems[0].isSubtype(of: other)
    default:
      break
    }

    switch other.canonical {
    case let that as UnionType:
      return elems.allSatisfy(that.elems.contains)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? UnionType,
          elems.count == that.elems.count
    else { return false }

    for (lhs, rhs) in zip(elems, that.elems) {
      guard lhs.isEqual(to: rhs) else { return false }
    }

    return true
  }

  public override func hash(into hasher: inout Hasher) {
    for elem in elems {
      elem.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    if elems.isEmpty {
      return "Nothing"
    } else {
      return elems.map(stringify(_:)).joined(separator: " | ")
    }
  }

  /// Creates `UnionType` with the given collection unless it contains a single element; in this
  /// case, returns it as is.
  public static func create<C>(unionOf types: C) -> ValType
  where C: Collection, C.Element == ValType
  {
    if types.isEmpty {
      return .nothing
    } else if types.count == 1 {
      return types.first!
    } else {
      return UnionType(types)
    }
  }

}

/// A type whose generic parameters have been bound.
public final class BoundGenericType: NominalType {

  /// The arguments provided for the underyling type's generic parameters.
  public let args: [ValType]

  public init(decl: GenericTypeDecl, args: [ValType]) {
    self.args = args
    super.init(decl: decl, flags: Flags.merge(args.map({ $0.flags })))
  }

  /// A dictionary mapping the generic type parameters of the underlying type to their argument.
  public var bindings: [GenericParamType: ValType] {
    let env = decl.genericEnv!
    return Dictionary(zip(env.params, args), uniquingKeysWith: { lhs, _ in lhs })
  }

  public override var canonical: ValType {
    let dealiasedArgs = args.map({ $0.canonical })

    if let aliasDecl = decl as? AliasTypeDecl {
      let underylingType = aliasDecl.realizeAliasedType().canonical
      assert(!(underylingType is AliasType))

      let subst = Dictionary(
        zip(decl.genericEnv!.params, dealiasedArgs),
        uniquingKeysWith: { lhs, _ in lhs })
      return underylingType.specialized(with: subst).canonical
    }

    return BoundGenericType(decl: decl, args: dealiasedArgs)
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? BoundGenericType(decl: decl, args: args.map({ $0.uncontextualized }))
      : self
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard let that = other as? BoundGenericType,
          decl === that.decl,
          args.count == that.args.count
    else { return reconcile(self, other) }

    for (lhs, rhs) in zip(args, that.args) {
      guard lhs.matches(with: rhs, reconcilingWith: reconcile) else { return false }
    }
    return true
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? BoundGenericType,
          decl === that.decl,
          args.count == that.args.count
    else { return false }

    for (lhs, rhs) in zip(args, that.args) {
      guard (lhs.isEqual(to: rhs)) else { return false }
    }

    return true
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
    for arg in args {
      arg.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    let args = self.args.map(String.init(describing:)).joined(separator: ", ")
    return "\(decl.ident)<\(args)>"
  }

}

/// A generic parameter type.
public final class GenericParamType: ValType, Hashable {

  /// The declaration of this generic parameter type.
  public unowned let decl: GenericParamDecl

  public init(decl: GenericParamDecl) {
    self.decl = decl
    super.init(flags: [.isCanonical, .hasTypeParams])
  }

  // Searches the generic environment that defines this generic parameter type.
  var genericEnv: GenericEnv? {
    var space = decl.parentDeclSpace?.innermostGenericSpace
    while let env = space?.prepareGenericEnv() {
      if env.params.contains(self) {
        return env
      }
      space = env.space.parentDeclSpace?.innermostGenericSpace
    }
    return nil
  }

  public override func lookup(member memberName: String) -> LookupResult {
    var result = LookupResult()
    guard let conformances = genericEnv?.conformances(of: self) else { return result }
    for conf in conformances {
      result.append(contentsOf: conf.viewDecl.lookup(qualified: memberName))
    }

    return result
  }

  public override func lookup(typeMember memberName: String) -> [TypeDecl] {
    var result: [TypeDecl] = []
    guard let conformances = genericEnv?.conformances(of: self) else { return [] }
    for conf in conformances {
      if let member = conf.viewDecl.typeMemberTable[memberName] {
        result.append(member)
      }
    }

    return result
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? GenericParamType {
      return self.decl === that.decl
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { decl.ident }

}

/// A type that refers to a type member of some existentially quantified parameter (e.g., `X::Y`
/// within the scope of a function `fun foo<X: V>(...)`, where `V::Y` is an abstract type).
public final class AssocType: ValType {

  /// The type of which this type is a member.
  public let base: ValType

  /// The interface type of this associated type.
  ///
  /// This type parameter identifies a particular abstract type in the type members of `base`. It
  /// is not meant to be substituted by a skolem if contextualized. Hence, it does not trigger the
  /// associated type to raise the `hasTypeParams` flag.
  public unowned let interface: GenericParamType

  public init(interface: GenericParamType, base: ValType) {
    precondition(
      base is GenericParamType || base is AssocType || base is SkolemType || base is TypeVar,
      "illegal base for associated type")

    self.base = base
    self.interface = interface
    super.init(flags: base.flags)
  }

  /// The root of this type.
  public var root: ValType {
    if let parent = base as? AssocType {
      return parent.base
    } else {
      return base
    }
  }

  public override func lookup(member memberName: String) -> LookupResult {
    var result = LookupResult()
    guard let genericEnv = interface.genericEnv else { return result }
    let decl = genericEnv.space as! ViewTypeDecl

    let key = AssocType(interface: interface, base: decl.receiverType)
    guard let conformances = genericEnv.conformances(of: key) else { return result }
    for conf in conformances {
      result.append(contentsOf: conf.viewDecl.lookup(qualified: memberName))
    }

    return result
  }

  public override func lookup(typeMember memberName: String) -> [TypeDecl] {
    var result: [TypeDecl] = []
    guard let genericEnv = interface.genericEnv else { return result }
    let decl = genericEnv.space as! ViewTypeDecl

    let key = AssocType(interface: interface, base: decl.receiverType)
    guard let conformances = genericEnv.conformances(of: key) else { return result }
    for conf in conformances {
      if let member = conf.viewDecl.typeMemberTable[memberName] {
        result.append(member)
      }
    }

    return result
  }

  public override var canonical: ValType {
    return AssocType(interface: interface, base: base.canonical)
  }

  public override var uncontextualized: ValType {
    return AssocType(
      interface: interface,
      base: base.uncontextualized)
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? AssocType {
      return self.base.isEqual(to: that.base) && self.interface.isEqual(to: that.interface)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    interface.hash(into: &hasher)
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "\(base)::\(interface)" }

}

/// A skolemized (a.k.a. rigid) type variable.
///
/// A skolem is a generic type parameter that has been existentially quantified within its generic
/// environment (e.g., `X` within the scope of a function `fun foo<X>(...)`.
public final class SkolemType: ValType {

  /// The interface type of this skolem.
  public unowned let interface: GenericParamType

  /// The generic environment in which this skolem is existentially quantified.
  public unowned let genericEnv: GenericEnv

  public init(interface: GenericParamType, genericEnv: GenericEnv) {
    self.interface = interface
    self.genericEnv = genericEnv
    super.init(flags: [.isCanonical, .hasSkolems])
  }

  public override var uncontextualized: ValType { interface }

  public override func lookup(member memberName: String) -> LookupResult {
    // Members of a skolem can only be generic parameters, defined as abstract type requirements in
    // the views to which the skolem conforms (note: concrete type declarations can't be nested in
    // a view). These members must be returned as associated types rather than uncontextualized
    // parameters, so that they remain bound to the same quantifier.
    var result = LookupResult()
    guard let conformances = genericEnv.conformances(of: interface) else { return result }
    for conf in conformances {
      result.append(contentsOf: conf.viewDecl.lookup(qualified: memberName))
    }

    return result
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? SkolemType {
      return self.interface.isEqual(to: that.interface)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(SkolemType.self))
    interface.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "$" + stringify(interface) }

}

/// The dynamic type of an opened existential at runtime.
public final class WitnessType: ValType {

  /// The existential type that this type is witnessing.
  public unowned let interface: ValType

  public init(interface: ValType) {
    self.interface = interface
    super.init(flags: [.isCanonical])
  }

  /// The stable identity of this witness.
  public var id: Int { Int(bitPattern: ObjectIdentifier(self)) }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? WitnessType {
      return self.interface.isEqual(to: that.interface)
    } else { return false }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(WitnessType.self))
    interface.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    let code = String(id, radix: 36, uppercase: true)
    return "@opened[\(code)] \(stringify(interface))"
  }

}

/// A tuple type.
public final class TupleType: ValType {

  /// An element in a tuple type.
  public struct Elem: Hashable, CustomStringConvertible {

    /// The label of the element, if any.
    public let label: String?

    /// The type of the element.
    public let type: ValType

    public init(label: String? = nil, type: ValType) {
      self.label = label
      self.type = type
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(label)
      type.hash(into: &hasher)
    }

    public var description: String {
      if let label = self.label {
        return "\(label): \(type)"
      } else {
        return String(describing: type)
      }
    }

    public static func == (lhs: Elem, rhs: Elem) -> Bool {
      return lhs.label == rhs.label && lhs.type.isEqual(to: rhs.type)
    }

  }

  /// The elements of the tuple.
  public let elems: [Elem]

  public init<S>(_ elems: S) where S: Sequence, S.Element == TupleType.Elem {
    let elems = Array(elems)

    // Compute the type's flags.
    var flags = Flags.merge(elems.map({ $0.type.flags }))
    if elems.count == 0 {
      flags = .isCanonical
    } else if (elems.count == 1) && (elems[0].label == nil) && elems[0].type != .unit {
      flags = flags.removing(.isCanonical)
    }

    self.elems = elems
    super.init(flags: flags)
  }

  public convenience init<S>(labelAndTypes: S) where S: Sequence, S.Element == (String?, ValType) {
    self.init(labelAndTypes.map(TupleType.Elem.init))
  }

  public convenience init<S>(types: S) where S: Sequence, S.Element == ValType {
    self.init(types.map({ type in TupleType.Elem(type: type) }))
  }

  /// The canonical form of the tuple.
  ///
  /// A tuple with only one element is canonical if and only if that element has a label or if that
  /// element is the unit type (i.e., `(()) != ()`).
  public override var canonical: ValType {
    let newTupleType = TupleType(elems.map({ elem in
      Elem(label: elem.label, type: elem.type.canonical)
    }))
    return newTupleType
  }

  public override var uncontextualized: ValType {
    if self[.hasSkolems] {
      return TupleType(elems.map({ elem in
        Elem(label: elem.label, type: elem.type.uncontextualized)
      }))
    } else {
      return self
    }
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard let that = other as? TupleType,
          elems.count == that.elems.count
    else { return reconcile(self, other) }

    for (lhs, rhs) in zip(elems, that.elems) {
      guard lhs.label == rhs.label else { return reconcile(self, other) }
      guard lhs.type.matches(with: rhs.type, reconcilingWith: reconcile) else { return false }
    }
    return true
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.canonical {
    case let that as TupleType:
      guard elems.count == that.elems.count else { return false }
      return zip(elems, that.elems).allSatisfy({ (a, b) -> Bool in
        (a.label == b.label) && a.type.isSubtype(of: b.type)
      })

    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? TupleType,
          elems.count == that.elems.count
    else { return false }

    for (lhs, rhs) in zip(elems, that.elems) {
      guard (lhs.label == rhs.label) && (lhs.type.isEqual(to: rhs.type)) else { return false }
    }

    return true
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(elems)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    let elems = self.elems.map(String.init(describing:)).joined(separator: ", ")
    return "(\(elems))"
  }

}

/// A function type.
public final class FunType: ValType {

  /// A parameter of in a function type.
  public struct Param: Hashable, CustomStringConvertible {

    /// The label of the parameter, if any.
    public let label: String?

    /// The type of the parameter.
    public let type: ValType

    public init(label: String? = nil, type: ValType) {
      assert(type is FunParamType || type is TypeVar || type is ErrorType)
      self.label = label
      self.type = type
    }

    public init(label: String? = nil, policy: PassingPolicy, rawType: ValType) {
      assert(!(rawType is FunParamType))
      self.label = label
      self.type = FunParamType(policy: policy, rawType: rawType)
    }

    /// The passing policy of this parameter.
    public var policy: PassingPolicy? { (type as? FunParamType)?.policy }

    /// Returns a new parameter with the same label but whose type is transformed by of the given
    /// closure.
    ///
    /// - Parameter transform: A closure that transforms the parameter's type.
    public func map(_ transform: (ValType) -> ValType) -> Param {
      return Param(label: label, type: transform(type))
    }

    public func hash(into hasher: inout Hasher) {
      hasher.combine(label)
      type.hash(into: &hasher)
    }

    public var description: String {
      if let label = self.label {
        return "\(label): \(type)"
      } else {
        return "\(type)"
      }
    }

    public static func == (lhs: Param, rhs: Param) -> Bool {
      return (lhs.label == rhs.label && lhs.type.isEqual(to: rhs.type))
    }

  }

  /// The function's domain.
  public let params: [Param]

  /// The function's codomain.
  public let retType: ValType

  public init(params: [Param], retType: ValType) {
    self.params = params
    self.retType = retType

    let flags = Flags.merge(params.map({ $0.type.flags }))
    super.init(flags: flags.merged(with: retType.flags))
  }

  public override var canonical: ValType {
    return FunType(
      params: params.map({ $0.map({ $0.canonical }) }),
      retType: retType.canonical)
  }

  public override var uncontextualized: ValType {
    if self[.hasSkolems] {
      return self
    } else {
      return FunType(
        params: params.map({ $0.map({ $0.uncontextualized }) }),
        retType: retType.uncontextualized)
    }
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard let that = other as? FunType,
          params.count == that.params.count
    else { return reconcile(self, other) }

    for (a, b) in zip(params, that.params) {
      guard (a.label == b.label) else { return reconcile(self, other) }
      guard a.type.matches(with: b.type, reconcilingWith: reconcile) else { return false }
    }

    return retType.matches(with: that.retType, reconcilingWith: reconcile)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.canonical {
    case let that as FunType:
      guard retType.isSubtype(of: that.retType) else { return false }
      guard params.count == that.params.count else { return false }
      return zip(params, that.params).allSatisfy({ (lhs, rhs) -> Bool in
        return (lhs.label == rhs.label) && (rhs.type.isSubtype(of: lhs.type))
      })

    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? FunType {
      return (params == that.params) && retType.isEqual(to: that.retType)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    for param in params {
      param.hash(into: &hasher)
    }
    retType.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    let params = self.params.map(String.init(describing:)).joined(separator: ", ")
    return "(\(params)) -> \(retType)"
  }

}

/// A function parameter type.
public final class FunParamType: ValType {

  /// The passing policly of the parameter.
  public let policy: PassingPolicy

  /// The raw type of the parameter.
  public let rawType: ValType

  public init(policy: PassingPolicy, rawType: ValType) {
    self.policy = policy
    self.rawType = rawType
    super.init(flags: rawType.flags)
  }

  public override var canonical: ValType {
    return FunParamType(policy: policy, rawType: rawType.canonical)
  }

  public override var uncontextualized: ValType {
    self[.hasSkolems] ? FunParamType(policy: policy, rawType: rawType.uncontextualized) : self
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard
      let that = other as? FunParamType,
      policy == that.policy
    else { return reconcile(self, other) }

    return rawType.matches(with: that.rawType, reconcilingWith: reconcile)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    return false
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? FunParamType {
      return (policy == that.policy) && rawType.isEqual(to: that.rawType)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(policy)
    rawType.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "\(policy) \(rawType)" }

}

/// An asynchronous type (e.g., `async Int`).
public final class AsyncType: ValType {

  /// A type.
  public let base: ValType

  public init(base: ValType) {
    self.base = base
    super.init(flags: base.flags.union(with: .hasAsync))
  }

  public override var canonical: ValType {
    return self[.hasAlias]
      ? AsyncType(base: base.canonical)
      : self
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? AsyncType(base: base.uncontextualized)
      : self
  }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard let that = other as? AsyncType else { return reconcile(self, other) }
    return base.matches(with: that.base, reconcilingWith: reconcile)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.canonical {
    case let that as AsyncType:
      return base.isSubtype(of: that.base)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  fileprivate override func isEqual(to other: ValType) -> Bool {
    if let that = other as? AsyncType {
      return base.isEqual(to: that.base)
    } else {
      return false
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(AsyncType.self))
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "async \(stringify(base))" }

}

/// An unresolved type.
///
/// This is used internally to denote the type of an unresolved declaration reference.
public final class UnresolvedType: ValType {

  private init() {
    super.init(flags: [.isCanonical, .hasUnresolved])
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public static let instance = UnresolvedType()

}

/// The type of an ill-formed declaration or expression.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorType: ValType {

  private init() {
    super.init(flags: [.isCanonical, .hasErrors])
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public static let instance = ErrorType()

}

/// A type variable.
public final class TypeVar: ValType, Hashable {

  /// The variable's identifier.
  public let id: Int

  /// The optional node representing the sub-expression with which the variable is associated.
  public private(set) weak var node: Node?

  public init(node: Node? = nil) {
    self.id = TypeVar.idFactory.makeID()
    self.node = node
    super.init(flags: [.isCanonical, .hasVariables])
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

  public override func accept<V>(_ visitor: inout V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "τ\(id)" }

  private static var idFactory = AutoIncrementFactory(start: 1)

}
