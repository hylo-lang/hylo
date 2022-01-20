import Basic

/// The base class for all semantic types.
///
/// Types are uniqued in the AST context. Once created, they remain immutable.
public class ValType: CustomStringConvertible, Equatable {

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
      return (rawValue & flags.rawValue) == flags.rawValue
    }

    /// Returns this set of type flags merged with another one.
    ///
    /// Merging is defined as the intersection of the universal flags and the union of the
    /// existential flags that are defined in each set.
    ///
    /// - Parameter other: Another set of type flags.
    public func merged(with other: Flags) -> Flags {
      // `isCanonical` is universal; other flags are existential.
      return Flags(
        rawValue: ~1 & (rawValue | other.rawValue) | (rawValue & other.rawValue))
    }

    /// Returns the union of this set with another.
    ///
    /// - Parameter other: Another set of type flags.
    public func union(with other: Flags) -> Flags {
      return Flags(rawValue: rawValue | other.rawValue)
    }

    /// Returns this set without the specified flags.
    ///
    /// - Parameter flags: The flags to remove.
    public func removing(_ flags: Flags) -> Flags {
      return Flags(rawValue: rawValue & ~flags.rawValue)
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

  /// The AST context in which this type was uniqued.
  public unowned let context: Context

  /// A set of type flags, defined recursively.
  public let flags: Flags

  /// Create a new type.
  init(context: Context, flags: Flags) {
    self.context = context
    self.flags = flags
  }

  /// Returns whether the specified flags are raised on this type.
  ///
  /// - parameter flags: A set of type flags.
  public subscript(flags: Flags) -> Bool {
    return self.flags.contains(flags)
  }

  /// Indicates whether the type is well-formed (i.e., it does not contain variables, unresolved
  /// types, or error types).
  public var isWellFormed: Bool {
    return !self[.hasVariables] && !self[.hasUnresolved] && !self[.hasErrors]
  }

  /// Indicates whether the type is existential.
  ///
  /// A type is existential if it cannot be resolved to a concrete representation statically, but
  /// it is known to represent a runtime type that satisfies a set of requirements. That does *not*
  /// include type variables, which have yet to be inferred as actual (potentially conrete) types.
  ///
  /// Instances of existential types are represented by existential packages.
  public final var isExistential: Bool {
    switch dealiased {
    case is ViewType, is ViewCompositionType, is UnionType, is SkolemType, is GenericParamType:
      return true
    case let type as KindType:
      return type.type.isExistential
    case let type as BoundGenericType where type.decl is AliasTypeDecl:
      return type.dealiased.isExistential
    case let type as AssocType:
      return type.base.isExistential
    case let type as FunParamType:
      return type.rawType.isExistential
    default:
      return false
    }
  }

  /// Indicates whether the type is `Unit`.
  public final var isUnit: Bool { self === context.unitType }

  /// Indicates whether the type is `Any`.
  public final var isAny: Bool { self === context.anyType }

  /// Indicates whether the type is `Nothing`.
  public final var isNothing: Bool { self === context.nothingType }

  /// Indicates whether the type is the unresolved type.
  public final var isUnresolved: Bool { self === context.unresolvedType }

  /// Indicates whether the type is the error type.
  public final var isError: Bool { self === context.errorType }

  /// Indicates whether the type is copyable.
  public var isCopyable: Bool { fatalError("unreachable") }

  /// The kind of the type.
  public var kind: KindType { context.kindType(type: self) }

  /// The canonical form of the type.
  ///
  /// A canonical type is a unique representation of a type, resilient to its syntactic spelling.
  /// For instance, both `(Int)` and `((Int))` have the same canonical type (i.e., `Int`).
  /// Canonical types are typically used to check whether two types are equal.
  ///
  /// - Note: Type aliases that are not mere synonyms for an existing nominal type are canonical.
  ///   For instance, given an alias `type A = B | C`, type `A` is canonical. Use `dealiased` if
  ///   you need to expand an alias.
  public var canonical: ValType { self }

  /// This type with all aliases resolved to their canonical form.
  public var dealiased: ValType { self }

  /// The uncontextualized interface of this type, that is the type in which all skolems have been
  /// substituted by their interface type.
  public var uncontextualized: ValType { self }

  /// Returns this type after substituting generic type parameters with the given mapping.
  ///
  /// - Parameter args: A substitution table.
  public final func specialized(with args: [GenericParamType: ValType]) -> ValType {
    guard self[.hasTypeParams] else { return self }
    return TypeSpecializer(args: args).walk(self)
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
    switch other.dealiased {
    case self.dealiased:
      return true
    case context.anyType:
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
  public func isEqual(to other: ValType) -> Bool {
    return self === other
  }

  /// Hashes the essential components of type into the given hasher.
  ///
  /// This method is used internally for uniquing type instances. All derived classes should
  /// override it to implement their own hashing logic. The default implementation is a no-op.
  ///
  /// - Parameter hasher: The hasher to use when combining the components of this type.
  public func hash(into hasher: inout Hasher) {}

  public func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    fatalError("unreachable")
  }

  public var description: String { String(describing: type(of: self)) }

  public static func == (lhs: ValType, rhs: ValType) -> Bool {
    return lhs.canonical === rhs.canonical
  }

}

/// A simple type walker that substitutes generic arguments for their corresponding parameters.
fileprivate final class TypeSpecializer: TypeWalker {

  /// The specialization arguments to apply.
  private let args: [GenericParamType: ValType]

  public init(args: [GenericParamType: ValType]) {
    self.args = args
  }

  public override func willVisit(_ type: ValType) -> TypeWalker.Action {
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
  public let type: ValType

  init(context: Context, type: ValType) {
    self.type = type
    super.init(context: context, flags: type.flags)
  }

  public override var canonical: ValType {
    return self[.isCanonical]
      ? self
      : type.canonical.kind
  }

  public override var dealiased: ValType {
    return self[.hasAlias]
      ? type.dealiased.kind
      : self
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? type.uncontextualized.kind
      : self
  }

  public override var isCopyable: Bool { true }

  public override func matches(
    with other: ValType,
    reconcilingWith reconcile: (ValType, ValType) -> Bool
  ) -> Bool {
    if self == other { return true }

    guard let that = other as? KindType else { return reconcile(self, other) }
    return type.matches(with: that.type, reconcilingWith: reconcile)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.dealiased {
    case let that as KindType:
      return type.isSubtype(of: that.type)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? KindType else { return false }
    return type.isEqual(to: that.type)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(KindType.self))
    type.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { stringify(type) + "::Kind" }

}

/// A built-in type.
///
/// Built-in types are not "defined" anywhere. They are created on demand by the AST context.
public class BuiltinType: ValType {

  /// The name of the type.
  public let name: String

  init(context: Context, name: String) {
    self.name = name
    super.init(context: context, flags: .isCanonical)
  }

  public override var isCopyable: Bool { true }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(name)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { name }

}

/// A built-in literal type.
public protocol BuiltinLiteral {}

/// A built-in integer literal type.
///
/// This type is used during type checking to infer the type of an literal expression.
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
/// This type represents the target's integer types. These can be of any bitwidth and do not
/// specify signedness.
///
/// - Note: This type does *not* correspond to Val's `Int`. The latter is an actual Val type,
///   defined in the standard library, which wrap a built-in integer type.
public final class BuiltinIntType: BuiltinType {

  /// The number of bits in the binary representation of values of this type.
  public let bitWidth: Int

  init(context: Context, name: String, bitWidth: Int) {
    assert(bitWidth > 0)
    self.bitWidth = bitWidth
    super.init(context: context, name: name)
  }

  public override func hash(into hasher: inout Hasher) {
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

  /// The module corresponding to this type.
  public unowned let module: ModuleDecl

  init(context: Context, module: ModuleDecl) {
    self.module = module
    super.init(context: context, flags: .isCanonical)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(module))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { module.name }

}

/// A nominal type.
public class NominalType: ValType {

  /// The declaration of this nominal type.
  public unowned let decl: GenericTypeDecl

  init(context: Context, decl: GenericTypeDecl, flags: Flags) {
    self.decl = decl
    super.init(context: context, flags: flags)
  }

  public override func lookup(member memberName: String) -> LookupResult {
    return decl.lookup(qualified: memberName)
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch other.dealiased {
    case let that as ViewType:
      return decl.conformanceTable[that]?.state == .checked
    case let that as ViewCompositionType:
      return that.views.allSatisfy({ decl.conformanceTable[$0]?.state == .checked })
    case let that:
      return super.isSubtype(of: that)
    }
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override var description: String { decl.name }

}

/// A product type, representing a collection of labeled value members.
public final class ProductType: NominalType {

  init(context: Context, decl: ProductTypeDecl) {
    super.init(context: context, decl: decl, flags: .isCanonical)
  }

  public override var isCopyable: Bool {
    return decl.conformanceTable[context.copyableType] != nil
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ProductType else { return false }
    return self.decl === that.decl
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A view type.
public final class ViewType: NominalType {

  init(context: Context, decl: ViewTypeDecl) {
    super.init(context: context, decl: decl, flags: .isCanonical)
  }

  public override var isCopyable: Bool {
    return decl.conformanceTable[context.copyableType] != nil
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ViewType else { return false }
    return self.decl === that.decl
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  /// Determines if a view must appear before another one in a canonical composition, based on
  /// module and name.
  fileprivate static func precedes(lhs: ViewType, rhs: ViewType) -> Bool {
    guard lhs !== rhs else { return false }

    if lhs.decl.rootDeclSpace === rhs.decl.rootDeclSpace {
      return lhs.decl.name.lexicographicallyPrecedes(rhs.decl.name)
    } else {
      return lhs.decl.rootDeclSpace.name.lexicographicallyPrecedes(rhs.decl.rootDeclSpace.name)
    }
  }

}

/// A type alias denoting a possibly generic type expression.
public final class AliasType: NominalType {

  init(context: Context, decl: AliasTypeDecl) {
    assert(decl.state >= .realized, "can't create alias type from unrealized declaration")

    let flags: Flags
    if let nominalType = decl.aliasedSign.type as? NominalType {
      flags = nominalType.flags.merged(with: .hasAlias)
    } else {
      flags = [.isCanonical, .hasAlias]
    }

    super.init(context: context, decl: decl, flags: flags)
  }

  public override var isCopyable: Bool {
    return canonical.isCopyable
  }

  public override var canonical: ValType {
    guard let aliasedDecl = (decl as! AliasTypeDecl).aliasedDecl else {
      assert(self[.isCanonical])
      return self
    }

    assert(!self[.isCanonical])
    return aliasedDecl.instanceType.canonical
  }

  public override var dealiased: ValType {
    return (decl as! AliasTypeDecl).realizeAliasedType().dealiased
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? AliasType else { return false }
    return self.decl === that.decl
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A view composition type.
///
/// This is the type of an existential package whose witness is known to conform to a set of views.
/// If this set is empty, then the witness can be any (synchronous) type (a.k.a., `Any`).
public final class ViewCompositionType: ValType {

  /// The views that are part of the compositio.
  public let views: [ViewType]

  init(context: Context, views: [ViewType]) {
    self.views = views

    // Determine canonicity.
    var flags: Flags
    switch views.count {
    case 0:
      // This is the `Any` type.
      flags = .isCanonical

    case 1:
      // The canonical form of a composition with a unique view is the view itself.
      flags = views[0].flags.removing(.isCanonical)

    default:
      // The composition is canonical if the views are "sorted".
      // FIXME: We should also remove duplicate views.
      flags = .merge(views.map({ $0.flags }))
      for i in 1 ..< views.count {
        guard ViewType.precedes(lhs: views[i - 1], rhs: views[i]) else {
          flags = flags.removing(.isCanonical)
          break
        }
      }
    }

    super.init(context: context, flags: flags)
  }

  public override var isCopyable: Bool {
    // `Any` is not copyable!
    return !views.isEmpty && views.allSatisfy({ $0.isCopyable })
  }

  public override var canonical: ValType {
    if self[.isCanonical] {
      return self
    } else if views.count == 1 {
      return views[0]
    } else {
      return context.viewCompositionType(views.sorted(by: ViewType.precedes(lhs:rhs:)))
    }
  }

  public override func isSubtype(of other: ValType) -> Bool {
    switch views.count {
    case 0:
      return self === other.dealiased
    case 1:
      return views[0].isSubtype(of: other)
    default:
      return views.allSatisfy({ $0.isSubtype(of: other) })
    }
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? ViewCompositionType,
          views.count == that.views.count
    else { return false }

    for (lhs, rhs) in zip(views, that.views) {
      guard lhs.isEqual(to: rhs) else { return false }
    }
    return true
  }

  public override func hash(into hasher: inout Hasher) {
    for view in views {
      view.hash(into: &hasher)
    }
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    if views.isEmpty {
      return "Any"
    } else {
      return views.map(stringify(_:)).joined(separator: " & ")
    }
  }

}

/// A union type (e.g. `A | B`).
public final class UnionType: ValType {

  /// The members of the union.
  ///
  /// Uniqueness of each element is not guaranteed, unless the union type is canonical.
  public let elems: [ValType]

  init(context: Context, elems: [ValType]) {
    assert(
      elems.allSatisfy({ !($0 is TypeVar) }),
      "unconstrained type variables cannot occur in union type")

    self.elems = elems

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

    super.init(context: context, flags: flags)
  }

  public override var isCopyable: Bool {
    return elems.allSatisfy({ $0.isCopyable })
  }

  public override var canonical: ValType {
    if self[.isCanonical] {
      return self
    } else if elems.count == 1 {
      return elems[0].canonical
    }

    var filtered: [ValType] = []
    for elem in elems {
      // Compute the index of the element in the backing array.
      let canonical = elem.canonical
      let index = filtered.sortedInsertionIndex(of: canonical, sortedBy: { a, b in
        ObjectIdentifier(a) < ObjectIdentifier(b)
      })

      // Skip duplicates.
      if (index == 0) || (index >= filtered.endIndex) || (filtered[index] != canonical) {
        filtered.insert(canonical, at: index)
      }
    }

    if filtered.count == 1 {
      return filtered[0]
    } else {
      return context.unionType(filtered)
    }
  }

  public override var dealiased: ValType {
    return UnionType
      .create(unionOf: elems.map({ $0.dealiased }), in: context)
      .canonical
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

    switch other.dealiased {
    case let that as UnionType:
      return elems.allSatisfy(that.elems.contains)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  public override func isEqual(to other: ValType) -> Bool {
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

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String {
    if elems.isEmpty {
      return "Nothing"
    } else {
      return elems.map(stringify(_:)).joined(separator: " | ")
    }
  }

  /// Creates `UnionType` with the given collection unless it contains a single element; in thise
  /// case, returns it as is.
  ///
  /// - Parameters:
  ///   - types: A collection of types.
  ///   - context: The context in which the union is formed.
  public static func create<C>(unionOf types: C, in context: Context) -> ValType
  where C: Collection, C.Element == ValType
  {
    if types.isEmpty {
      return context.nothingType
    } else if types.count == 1 {
      return types.first!
    } else {
      return context.unionType(types)
    }
  }

}

/// A type whose generic parameters have been bound.
public final class BoundGenericType: NominalType {

  /// The arguments provided for the underyling type's generic parameters.
  public let args: [ValType]

  init(context: Context, decl: GenericTypeDecl, args: [ValType]) {
    self.args = args
    super.init(context: context, decl: decl, flags: Flags.merge(args.map({ $0.flags })))
  }

  /// A dictionary mapping the generic type parameters of the underlying type to their argument.
  public var bindings: [GenericParamType: ValType] {
    let env = decl.genericEnv!
    return Dictionary(zip(env.params, args), uniquingKeysWith: { lhs, _ in lhs })
  }

  public override var isCopyable: Bool {
    return decl.conformanceTable[context.copyableType] != nil
  }

  public override var canonical: ValType {
    return self[.isCanonical]
      ? self
      : context.boundGenericType(decl: decl, args: args.map({ $0.canonical }))
  }

  public override var dealiased: ValType {
    let dealiasedArgs = args.map({ $0.dealiased })

    if let aliasDecl = decl as? AliasTypeDecl {
      let underylingType = aliasDecl.realizeAliasedType().dealiased
      assert(!(underylingType is AliasType))

      let subst = Dictionary(
        zip(decl.genericEnv!.params, dealiasedArgs),
        uniquingKeysWith: { lhs, _ in lhs })
      return underylingType.specialized(with: subst).dealiased
    }

    return context.boundGenericType(decl: decl, args: dealiasedArgs)
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? context.boundGenericType(decl: decl, args: args.map({ $0.uncontextualized }))
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

  public override func isEqual(to other: ValType) -> Bool {
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

  /// The declaration of this generic parameter type.
  public unowned let decl: GenericParamDecl

  init(context: Context, decl: GenericParamDecl) {
    self.decl = decl
    super.init(context: context, flags: [.isCanonical, .hasTypeParams])
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

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? GenericParamType else { return false }
    return self.decl === that.decl
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(decl))
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { decl.name }

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

  init(context: Context, interface: GenericParamType, base: ValType) {
    precondition(
      base is GenericParamType || base is AssocType || base is SkolemType || base is TypeVar,
      "illegal base for associated type")

    self.base = base
    self.interface = interface
    super.init(context: context, flags: base.flags)
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

    let key = context.assocType(interface: interface, base: decl.receiverType)
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

    let key = context.assocType(interface: interface, base: decl.receiverType)
    guard let conformances = genericEnv.conformances(of: key) else { return result }
    for conf in conformances {
      if let member = conf.viewDecl.typeMemberTable[memberName] {
        result.append(member)
      }
    }

    return result
  }

  public override var canonical: ValType {
    switch base.canonical {
    case is GenericParamType, is SkolemType, is TypeVar:
      assert(self[.isCanonical])
      return self

    case is AssocType:
      return context.assocType(interface: interface, base: base)

    default:
      fatalError("unreachable")
    }
  }

  public override var dealiased: ValType {
    return context.assocType(interface: interface, base: base.dealiased)
  }

  public override var uncontextualized: ValType {
    return context.assocType(
      interface: interface,
      base: base.uncontextualized)
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? AssocType else { return false }
    return self.base.isEqual(to: that.base) && self.interface.isEqual(to: that.interface)
  }

  public override func hash(into hasher: inout Hasher) {
    interface.hash(into: &hasher)
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
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

  init(context: Context, interface: GenericParamType, genericEnv: GenericEnv) {
    self.interface = interface
    self.genericEnv = genericEnv
    super.init(context: context, flags: [.isCanonical, .hasSkolems])
  }

  public override var isCopyable: Bool {
    return genericEnv.conformance(of: self, to: context.copyableType) != nil
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

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? SkolemType else { return false }
    return self.interface.isEqual(to: that.interface)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(SkolemType.self))
    interface.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "$" + stringify(interface) }

}

/// The dynamic type of an opened existential at runtime.
public final class WitnessType: ValType {

  /// The existential type that this type is witnessing.
  public unowned let interface: ValType

  init(context: Context, interface: ValType) {
    self.interface = interface
    super.init(context: context, flags: [.isCanonical])
  }

  /// The stable identity of this witness.
  public var id: Int { Int(bitPattern: ObjectIdentifier(self)) }

  public override var isCopyable: Bool {
    return interface.isCopyable
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? WitnessType else { return false }
    return self.interface.isEqual(to: that.interface)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(WitnessType.self))
    interface.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
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

  init(context: Context, elems: [Elem]) {
    self.elems = elems

    // Compute the type's flags.
    var flags = Flags.merge(elems.map({ $0.type.flags }))
    if elems.count == 0 {
      flags = .isCanonical
    } else if (elems.count == 1) && (elems[0].label == nil) && !elems[0].type.isUnit {
      flags = flags.removing(.isCanonical)
    }

    super.init(context: context, flags: flags)
  }

  public override var isCopyable: Bool {
    return elems.allSatisfy({ $0.type.isCopyable })
  }

  /// The canonical form of the tuple.
  ///
  /// A tuple with only one element is canonical if and only if that element has a label or if that
  /// element is the unit type (i.e., `(()) != ()`).
  public override var canonical: ValType {
    if self[.isCanonical] {
      return self
    }
    if (elems.count == 1) && (elems[0].label == nil) {
      assert(!elems[0].type.isUnit)
      return elems[0].type.canonical
    }
    return context.tupleType(elems.map({ elem in
      Elem(label: elem.label, type: elem.type.canonical)
    }))
  }

  public override var dealiased: ValType {
    let newTupleType = context.tupleType(elems.map({ elem in
      Elem(label: elem.label, type: elem.type.dealiased)
    }))
    return newTupleType.canonical
  }

  public override var uncontextualized: ValType {
    if !self[.hasSkolems] {
      return self
    }
    return context.tupleType(elems.map({ elem in
      Elem(label: elem.label, type: elem.type.uncontextualized)
    }))
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
    switch other.dealiased {
    case let that as TupleType:
      guard elems.count == that.elems.count else { return false }
      return zip(elems, that.elems).allSatisfy({ (a, b) -> Bool in
        (a.label == b.label) && a.type.isSubtype(of: b.type)
      })

    case let that:
      return super.isSubtype(of: that)
    }
  }

  public override func isEqual(to other: ValType) -> Bool {
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

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
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
      self.type = rawType.context.funParamType(policy: policy, rawType: rawType)
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

  init(context: Context, params: [Param], retType: ValType) {
    self.params = params
    self.retType = retType

    let flags = Flags.merge(params.map({ $0.type.flags }))
    super.init(context: context, flags: flags.merged(with: retType.flags))
  }

  public override var isCopyable: Bool { true }

  public override var canonical: ValType {
    if self[.isCanonical] {
      return self
    } else {
      return context.funType(
        params: params.map({ $0.map({ $0.canonical }) }),
        retType: retType.canonical)
    }
  }

  public override var dealiased: ValType {
    return context.funType(
      params: params.map({ $0.map({ $0.dealiased }) }),
      retType: retType.dealiased)
  }

  public override var uncontextualized: ValType {
    if self[.hasSkolems] {
      return self
    } else {
      return context.funType(
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
    switch other.dealiased {
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

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? FunType else { return false }
    return (params == that.params) && retType.isEqual(to: that.retType)
  }

  public override func hash(into hasher: inout Hasher) {
    for param in params {
      param.hash(into: &hasher)
    }
    retType.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
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

  init(policy: PassingPolicy, rawType: ValType) {
    self.policy = policy
    self.rawType = rawType
    super.init(context: rawType.context, flags: rawType.flags)
  }

  public override var isCopyable: Bool { rawType.isCopyable }

  public override var canonical: ValType {
    return self[.isCanonical]
      ? self
      : context.funParamType(policy: policy, rawType: rawType.canonical)
  }

  public override var dealiased: ValType {
    return context.funParamType(policy: policy, rawType: rawType.dealiased)
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? self
      : context.funParamType(policy: policy, rawType: rawType.uncontextualized)
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

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? FunParamType else { return false }
    return (policy == that.policy) && rawType.isEqual(to: that.rawType)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(policy)
    rawType.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "\(policy) \(rawType)" }

}

/// An asynchronous type (e.g., `async Int`).
public final class AsyncType: ValType {

  /// A type.
  public let base: ValType

  init(context: Context, base: ValType) {
    self.base = base
    super.init(context: context, flags: base.flags.union(with: .hasAsync))
  }

  public override var isCopyable: Bool { false }

  public override var canonical: ValType {
    return self[.isCanonical]
      ? self
      : context.asyncType(of: base.canonical)
    // FIXME: We might add more equivalence classes (e.g., `async (a: T) == (a: async T)`).
  }

  public override var dealiased: ValType {
    return self[.hasAlias]
      ? context.asyncType(of: base.dealiased)
      : self
  }

  public override var uncontextualized: ValType {
    return self[.hasSkolems]
      ? context.asyncType(of: base.uncontextualized)
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
    switch other.dealiased {
    case let that as AsyncType:
      return base.isSubtype(of: that.base)
    case let that:
      return super.isSubtype(of: that)
    }
  }

  public override func isEqual(to other: ValType) -> Bool {
    guard let that = other as? AsyncType else { return false }
    return base.isEqual(to: that.base)
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(AsyncType.self))
    base.hash(into: &hasher)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "async \(stringify(base))" }

}

/// An unresolved type.
///
/// This is used internally to denote the type of an unresolved declaration reference.
public final class UnresolvedType: ValType {

  init(context: Context) {
    super.init(context: context, flags: [.isCanonical, .hasUnresolved])
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
    super.init(context: context, flags: [.isCanonical, .hasErrors])
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

}

/// A type variable.
public final class TypeVar: ValType, Hashable {

  /// The variable's identifier.
  public let id: Int

  /// The optional node representing the sub-expression with which the variable is associated.
  public private(set) weak var node: Node?

  public init(context: Context, node: Node? = nil) {
    self.id = TypeVar.idFactory.makeID()
    self.node = node
    super.init(context: context, flags: [.isCanonical, .hasVariables])
  }

  public override func hash(into hasher: inout Hasher) {
    hasher.combine(id)
  }

  public override func accept<V>(_ visitor: V) -> V.Result where V: TypeVisitor {
    visitor.visit(self)
  }

  public override var description: String { "τ\(id)" }

  private static var idFactory = AutoIncrementFactory(start: 1)

}
