import Basic

/// A node representing the declaration of one or more entities.
public protocol Decl: Node {

  /// The innermost parent in which this declaration resides.
  var parentDeclSpace: DeclSpace? { get }

  /// The (semantic) state of the declaration.
  var state: DeclState { get }

  /// Sets the state of this declaration.
  func setState(_ newState: DeclState)

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A declaration visitor.
  func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor

}

/// The state of a declaration, as it goes through type checking.
public enum DeclState: Int, Comparable {

  /// The declaration was just parsed; no semantic information is available.
  case parsed

  /// The type of the declaration is available for type inference, but the declaration has not been
  /// type checked yet.
  ///
  /// For a `TypeDecl` or a `ValueDecl`, the `type` property of the declaration has been realized.
  /// For a `TypeExtDec`, the extension has been bound.
  case realized

  /// The declaration has been scheduled for type checking. Any attempt to transition back to this
  /// state should be interpreted as a circular dependency.
  case typeCheckRequested

  /// Type checking has been completed; the declaration is semantically well-formed.
  case typeChecked

  /// The declaration has been found to be ill-formed; it should be ignored by all subsequent
  /// semantic analysis phases, without producing any further diagnostic.
  ///
  /// - Note: `invalid` must have the largest raw value, so that `.invalid < validState` always
  ///   answer `false` for any arbitrary valid state.
  case invalid

  /// Returns a Boolean value indicating whether one state is more "advanced" than the other.
  public static func < (lhs: DeclState, rhs: DeclState) -> Bool {
    return lhs.rawValue < rhs.rawValue
  }

}

/// A type or a value declaration.
public protocol TypeOrValueDecl: Decl {

  /// The name of the declaration.
  var name: String { get }

  /// The semantic type of the declaration, outside of its generic context.
  var type: ValType { get }

  /// A flag indicating whether the declaration is overloadable.
  var isOverloadable: Bool { get }

}

/// A type declaration.
public protocol TypeDecl: TypeOrValueDecl, DeclSpace {

  /// The type of instances of the declared type.
  ///
  /// For the type of the declaration itself, use `type`, which returns a kind.
  var instanceType: ValType { get }

}

extension TypeDecl {

  public var instanceType: ValType {
    return (type as! KindType).type
  }

}

/// A type declaration that can have a generic clause.
public protocol GenericTypeDecl: TypeDecl, GenericDeclSpace {

  /// The generic clause of the declaration.
  var genericClause: GenericClause? { get }

}

/// A named declaration.
public protocol ValueDecl: TypeOrValueDecl {

  /// A flag indicating whether the declaration describes the member of a type.
  var isMember: Bool { get }

  /// The type of the declaration.
  var type: ValType { get set }

  /// Realizes the semantic type of the value being declared.
  func realize() -> ValType

}

extension ValueDecl {

  /// Contextualize the type of this declaration from the given use site.
  ///
  /// - Parameters:
  ///   - useSite: The declaration space from which the declaration is being referred.
  ///   - args: A dictionary containing specialization arguments for generic type parameters.
  ///   - handleConstraint: A closure that accepts contextualized contraint prototypes. It is not
  ///     called unless the contextualized type contains opened existentials for which there exist
  ///     type requirements.
  public func contextualize(
    from useSite: DeclSpace,
    args: [GenericParamType: ValType] = [:],
    processingContraintsWith handleConstraint: (GenericEnv.ConstraintPrototype) -> Void = { _ in }
  ) -> ValType {
    // Realize the declaration's type.
    var genericType = realize()

    // Specialize the generic parameters for which arguments have been provided.
    if !args.isEmpty {
      genericType = genericType.specialized(with: args)
    }

    guard genericType.props.contains(.hasTypeParams) else { return genericType }

    // If the declaration is its own generic environment, then we must contextualize it externally,
    // regardless of the use-site. This situation corresponds to a "fresh" use of a generic
    // declaration within its own space (e.g., a recursive call to a generic function).
    if let gds = self as? GenericDeclSpace {
      guard let env = gds.prepareGenericEnv() else {
        return type.context.errorType
      }

      // Adjust the use site depending on the type of declaration.
      var adjustedSite: DeclSpace
      if gds is CtorDecl {
        // Constructors are contextualized from outside of their type declaration.
        adjustedSite = gds.spacesUpToRoot.first(where: { $0 is TypeDecl })!.parentDeclSpace!
      } else {
        adjustedSite = gds
      }
      if adjustedSite.isDescendant(of: useSite) {
        adjustedSite = useSite
      }

      return env.contextualize(
        genericType, from: adjustedSite, processingContraintsWith: handleConstraint)
    }

    // Find the innermost generic space, relative to this declaration. We can assume there's one,
    // otherwise `realize()` would have failed to resolve the decl.
    guard let env = parentDeclSpace!.innermostGenericSpace!.prepareGenericEnv() else {
      return type.context.errorType
    }
    return env.contextualize(
      genericType, from: useSite, processingContraintsWith: handleConstraint)
  }

}

/// A declaration that consists of a pattern and an optional initializer for the variables declared
/// in this pattern.
public final class PatternBindingDecl: Decl {

  public init(
    isMutable       : Bool,
    pattern         : Pattern,
    typeSign        : TypeRepr?,
    initializer     : Expr?,
    declKeywordRange: SourceRange,
    range           : SourceRange
  ) {
    self.isMutable = isMutable
    self.pattern = pattern
    self.sign = typeSign
    self.initializer = initializer
    self.declKeywordRange = declKeywordRange
    self.range = range
  }

  /// A flag indicating whether the declared variables are mutable.
  public var isMutable: Bool

  /// A flag indicating whether the declaration describes member variables.
  public var isMember: Bool {
    return (parentDeclSpace is NominalTypeDecl || parentDeclSpace is TypeExtDecl)
  }

  /// The pattern being bound.
  public var pattern: Pattern

  /// The signature of the pattern.
  public var sign: TypeRepr?

  /// The initializer for the variables declared by the pattern.
  public var initializer: Expr?

  /// The source range of this declaration `val` or `var` keyword.
  public var declKeywordRange: SourceRange

  public var range: SourceRange

  public weak var parentDeclSpace: DeclSpace?

  public private(set) var state = DeclState.parsed

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  /// The declaration of all variables declared in this pattern.
  public var varDecls: [VarDecl] = []

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A variable declaration.
public final class VarDecl: ValueDecl {

  public init(name: String, type: ValType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  public var name: String

  /// The pattern binding declaration that introduces this variable declaration.
  public weak var patternBindingDecl: PatternBindingDecl?

  public var range: SourceRange

  /// The backend of the variable.
  public var backend: VarBackend = .storage

  /// A flag indicating whether the variable has storage.
  public var hasStorage: Bool { backend == .storage }

  /// A flag indicating whether the variable is mutable.
  public var isMutable: Bool { patternBindingDecl?.isMutable ?? false }

  /// A flag indicating whether the variable is member of a pattern binding declared as a member.
  public var isMember: Bool { patternBindingDecl?.isMember ?? false }

  public var isOverloadable: Bool { false }

  public weak var parentDeclSpace: DeclSpace?

  public private(set) var state = DeclState.parsed

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public var type: ValType

  public func realize() -> ValType {
    if state >= .realized { return type }

    // Variable declarations can't realize on their own. Their type is defined when the enclosing
    // pattern binding declaration is type checked.
    preconditionFailure("variable declarations can't realize on their own")
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The "backend" of a variable declaration, describing how its value is stored and retrieved.
///
/// In the future, we may use this enum to represent computed and lazy properties.
public enum VarBackend {

  /// The value of the variable is stored in memory.
  case storage

}

/// The base class for function declarations.
public class BaseFunDecl: ValueDecl, GenericDeclSpace {

  public init(
    name          : String,
    declModifiers : [DeclModifier] = [],
    genericClause : GenericClause? = nil,
    params        : [FunParamDecl] = [],
    retTypeSign   : TypeRepr?      = nil,
    body          : BraceStmt?     = nil,
    type          : ValType,
    range         : SourceRange
  ) {
    self.name = name
    self.declModifiers = declModifiers
    self.genericClause = genericClause
    self.params = params
    self.retSign = retTypeSign
    self.body = body
    self.type = type
    self.range = range

    // Process the function modifiers.
    self.props = FunDeclProps()
    for modifier in declModifiers {
      switch modifier.kind {
      case .mut   : props.formUnion(.isMutating)
      case .static: props.formUnion(.isStatic)
      default     : continue
      }
    }
  }

  // MARK: Source properties

  /// The name of the function (empty for anonymous functions).
  public var name: String

  /// The local discriminator for the function.
  ///
  /// This is the index of the function in the sequence of anonymous declarations in the parent
  /// declaration space.
  public var discriminator = 0

  /// The semantic properties of the declaration.
  public var props: FunDeclProps

  public var isMember: Bool { props.contains(.isMember) }

  /// The declaration modifiers of the function.
  ///
  /// This array only contains the declaration modifiers as found in source. It may not accurately
  /// describe the relevant bits in `props`, nor vice-versa.
  public var declModifiers: [DeclModifier]

  /// The generic clause of the declaration.
  public var genericClause: GenericClause?

  /// The parameters of the function.
  public var params: [FunParamDecl]

  /// The signature of the function's return type.
  public var retSign: TypeRepr?

  /// The body of the function.
  public var body: BraceStmt?

  public var range: SourceRange

  // MARK: Implicit declarations

  /// The implicit declaration of the `self` parameter for member functions.
  ///
  /// - Note: Accessing this property will trap if the function declaration is in an extension that
  ///   hasn't been bound to a nominal type yet.
  public lazy var selfDecl: FunParamDecl? = {
    guard props.contains(.isMember) || (self is CtorDecl) else { return nil }

    var selfDecl: NominalTypeDecl
    switch parentDeclSpace {
    case let typeDecl as NominalTypeDecl:
      // The declaration is in the body of a nominal type.
      selfDecl = typeDecl

    case let typeExtDecl as TypeExtDecl:
      // The declaration is in the body of a type extension.
      guard let extendedDecl = typeExtDecl.extendedDecl else {
        let decl = FunParamDecl(
          name: "self", externalName: "self", type: type.context.errorType, range: .invalid)
        decl.setState(.invalid)
        return decl
      }
      selfDecl = extendedDecl

    default:
      fatalError("unreachable")
    }

    let selfType: ValType
    if let productDecl = selfDecl as? ProductTypeDecl, productDecl.hasOwnGenericParams {
      selfType = type.context.boundGenericType(
        decl: productDecl,
        args: productDecl.genericClause!.params.map({ $0.instanceType }))
    } else {
      // FIXME: `ViewTypeDecl`s too will probably conform to `GenericDeclSpace` eventually.
      selfType = selfDecl.instanceType
    }

    let decl = FunParamDecl(
      name: "self", externalName: "self",
      type: props.contains(.isMutating)
        ? type.context.inoutType(of: selfType)
        : selfType,
      range: .invalid)
    decl.parentDeclSpace = self
    decl.setState(.typeChecked)
    return decl
  }()

  // MARK: Name Lookup

  public weak var parentDeclSpace: DeclSpace?

  public var isOverloadable: Bool { true }

  /// Looks up for declarations that match the given name, directly enclosed within the function
  /// declaration space.
  ///
  /// The returned type declarations are the function's generic parameters. The value declarations
  /// are its explicit and implicit parameters. The declarations scoped within the function's body
  /// are **not** included in either of these sets. Those reside in a nested space.
  public func lookup(qualified name: String) -> LookupResult {
    let types  = genericClause.map({ clause in clause.params.filter({ $0.name == name }) }) ?? []
    var values = params.filter({ $0.name == name })
    if name == "self", let selfDecl = self.selfDecl {
      values.append(selfDecl)
    }

    return LookupResult(types: types, values: values)
  }

  // MARK: Semantic properties

  public private(set) var state = DeclState.parsed

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    assert((newState != .typeCheckRequested) || (state >= .realized),
           "type checking requested before the function signature was realized")

    state = newState
  }

  /// The "applied" type of the function.
  ///
  /// This property must be kept synchronized with the function type implicitly described by the
  /// parameter list and return signature. Setting its value directly is discouraged, you should
  /// use `realize()` instead.
  ///
  /// - Note: Member functions accept the receiver (i.e., `self`) as an implicit first parameter.
  ///   This means that a call to a method `T -> U` is actually an application of an *unapplied*
  ///   function `(Self, T) -> U`, where `Self` is the receiver's type. This property denotes the
  ///   *applied* type, which should be understood as the return type the unapplied function's
  ///   partial application.
  public var type: ValType

  /// The "unapplied" type of the function.
  ///
  /// For member functions, this is the type of the function extended with an implicit receiver
  /// parameter. For other functions, this is equal to `type`.
  public var unappliedType: ValType {
    guard props.contains(.isMember) else { return type }

    let funType = type as! FunType
    var paramTypeElems = (funType.paramType as? TupleType)?.elems
      ?? [TupleType.Elem(type: funType.paramType)]
    paramTypeElems.insert(TupleType.Elem(label: "self", type: selfDecl!.type), at: 0)

    return type.context.funType(
      paramType: type.context.tupleType(paramTypeElems),
      retType: funType.retType)
  }

  /// Realizes the "applied" type of the function from its signature.
  ///
  /// For member functions, this is the type of the function extended with an implicit receiver
  /// parameter. For other functions, this is equal to `type`.
  public func realize() -> ValType {
    if state >= .realized { return type }

    // Realize the parameters.
    var paramTypeElems: [TupleType.Elem] = []
    for param in params {
      _ = param.realize()
      paramTypeElems.append(TupleType.Elem(label: param.externalName, type: param.type))
    }
    let paramType = type.context.tupleType(paramTypeElems)

    // Realize the return type.
    let retType: ValType
    if let sign = retSign {
      retType = sign.realize(unqualifiedFrom: self)
    } else {
      retType = type.context.unitType
    }

    type = type.context.funType(paramType: paramType, retType: retType)
    setState(.realized)
    return type
  }

  public var hasOwnGenericParams: Bool { genericClause != nil }

  public var genericEnv: GenericEnv?

  public func prepareGenericEnv() -> GenericEnv? {
    if let env = genericEnv { return env }

    if let clause = genericClause {
      genericEnv = GenericEnv(
        space: self,
        params: clause.params.map({ $0.instanceType as! GenericParamType }),
        typeReqs: clause.typeReqs,
        context: type.context)
      guard genericEnv != nil else {
        setState(.invalid)
        return nil
      }
    } else {
      genericEnv = GenericEnv(space: self)
    }

    return genericEnv
  }

  // MARK: Misc.

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

  /// A set representing various properties of a function declaration.
  public struct FunDeclProps: OptionSet {

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }

    public let rawValue: Int

    /// Indicates whether the function is a member of a type.
    ///
    /// - Note: Constructors are *not* considered to be member functions.
    public static let isMember   = FunDeclProps(rawValue: 1 << 0)

    /// Indicates whether the function is mutating its receiver.
    public static let isMutating = FunDeclProps(rawValue: 1 << 1)

    /// Indicates whether the function is static.
    public static let isStatic   = FunDeclProps(rawValue: 1 << 2)

  }

}

/// A function declaration.
public final class FunDecl: BaseFunDecl {

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A constructor declaration.
public final class CtorDecl: BaseFunDecl {

  public init(
    declModifiers : [DeclModifier] = [],
    genericClause : GenericClause? = nil,
    params        : [FunParamDecl] = [],
    body          : BraceStmt?     = nil,
    type          : ValType,
    range         : SourceRange
  ) {
    super.init(
      name: "new",
      declModifiers: declModifiers,
      genericClause: genericClause,
      params: params,
      body: body,
      type: type,
      range: range)
    props.insert(.isMutating)
  }

  public override func realize() -> ValType {
    if state >= .realized { return type }

    var paramTypeElems: [TupleType.Elem] = []
    for param in params {
      _ = param.realize()
      paramTypeElems.append(TupleType.Elem(label: param.externalName, type: param.type))
    }
    let paramType = type.context.tupleType(paramTypeElems)
    let retType = (selfDecl!.type as! InoutType).base

    type = type.context.funType(paramType: paramType, retType: retType)
    setState(.realized)
    return type
  }

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a function parameter.
public final class FunParamDecl: ValueDecl {

  public init(
    name        : String,
    externalName: String?   = nil,
    typeSign    : TypeRepr? = nil,
    type        : ValType,
    range       : SourceRange
  ) {
    self.name = name
    self.externalName = externalName
    self.sign = typeSign
    self.type = type
    self.range = range
  }

  /// The internal name of the parameter.
  public var name: String

  /// The external name of the parameter.
  public var externalName: String?

  /// The signature of the parameter's type.
  public var sign: TypeRepr?

  public var range: SourceRange

  public weak var parentDeclSpace: DeclSpace?

  public var isMember: Bool { false }

  public var isOverloadable: Bool { false }

  public private(set) var state = DeclState.parsed

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public var type: ValType

  public func realize() -> ValType {
    if state >= .realized { return type }

    if let sign = self.sign {
      type = sign.realize(unqualifiedFrom: parentDeclSpace!)
    } else {
      preconditionFailure("cannot realize parameter declaration without a signature")
    }

    setState(.realized)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The base class for nominal type declarations.
public class NominalTypeDecl: TypeDecl, DeclSpace {

  public init(
    name        : String,
    inheritances: [UnqualTypeRepr] = [],
    members     : [Decl]           = [],
    type        : ValType,
    range       : SourceRange
  ) {
    self.name = name
    self.inheritances = inheritances
    self.members = members
    self.type = type
    self.range = range
  }

  /// The name of the type.
  public var name: String

  /// The views to which the type should conform.
  public var inheritances: [TypeRepr]

  /// The member declarations of the type.
  ///
  /// This is the list of explicit declarations found directly within the type declaration. Use
  /// `allTypeAndValueMembers` to enumerate all member declarations, including those that are
  /// declared in extensions, inherited by conformance or synthetized.
  public var members: [Decl]

  public var range: SourceRange

  // MARK: Name lookup

  public weak var parentDeclSpace: DeclSpace?

  public var isOverloadable: Bool { false }

  /// An internal lookup table keeping track of value members.
  var valueMemberTable: [String: [ValueDecl]]?

  /// An internal lookup table keeping track of type members.
  var typeMemberTable: [String: TypeDecl]?

  // Updates or initializes the member lookup tables.
  func updateMemberTables() {
    guard valueMemberTable == nil else { return }

    // Initialize type lookup tables.
    valueMemberTable = [:]
    typeMemberTable  = [:]

    /// Fills the member lookup table with the given declarations.
    func fill<S>(members: S) where S: Sequence, S.Element == Decl {
      for decl in members where decl.state != .invalid {
        switch decl {
        case let valueDecl as ValueDecl:
          valueMemberTable![valueDecl.name, default: []].append(valueDecl)

        case let pbDecl as PatternBindingDecl:
          for pattern in pbDecl.pattern.namedPatterns {
            valueMemberTable![pattern.decl.name, default: []].append(pattern.decl)
          }

        case let typeDecl as TypeDecl:
          // Check for invalid redeclarations.
          // FIXME: Shadow declaration inherited declarations.
          guard typeMemberTable![typeDecl.name] == nil else {
            type.context.report(
              .duplicateDeclaration(symbol: typeDecl.name, range: typeDecl.range))
            typeDecl.setState(.invalid)
            continue
          }

          typeMemberTable![typeDecl.name] = typeDecl

        default:
          continue
        }
      }
    }

    // Populate the lookup table with generic parameters.
    if let clause = (self as? ProductTypeDecl)?.genericClause {
      fill(members: clause.params)
    }

    // Populate the lookup table with members.
    fill(members: members)

    // Populate the lookup table with members declared in extensions.
    for module in type.context.modules.values {
      for extDecl in module.extensions(of: self) {
        fill(members: extDecl.members)
      }
    }

    // Populate the lookup table with members inherited by conformance.
    for conformance in conformances {
      fill(members: conformance.viewDecl.allTypeAndValueMembers.lazy.map({ $0.decl }))
    }
  }

  public func lookup(qualified name: String) -> LookupResult {
    updateMemberTables()
    return LookupResult(
      types : typeMemberTable![name].map({ [$0] }) ?? [],
      values: valueMemberTable![name] ?? [])
  }

  /// A sequence with all members in this type.
  public var allTypeAndValueMembers: MemberIterator {
    updateMemberTables()
    return MemberIterator(decl: self)
  }

  /// An iterator over all the member declarations of a type.
  public struct MemberIterator: IteratorProtocol, Sequence {

    public typealias Element = (name: String, decl: TypeOrValueDecl)

    fileprivate init(decl: NominalTypeDecl) {
      self.decl = decl
      self.valueMemberIndex = (decl.valueMemberTable!.startIndex, 0)
      self.typeMemberIndex = decl.typeMemberTable!.startIndex
    }

    fileprivate unowned let decl: NominalTypeDecl

    fileprivate var valueMemberIndex: (Dictionary<String, [ValueDecl]>.Index, Int)

    fileprivate var typeMemberIndex: Dictionary<String, TypeDecl>.Index

    public mutating func next() -> (name: String, decl: TypeOrValueDecl)? {
      // Iterate over value members.
      let valueTable = decl.valueMemberTable!
      if valueMemberIndex.0 < valueTable.endIndex {
        // Extract the next member.
        let (name, bucket) = valueTable[valueMemberIndex.0]
        let member = bucket[valueMemberIndex.1]

        // Update the value member index.
        let i = valueMemberIndex.1 + 1
        valueMemberIndex = (i >= bucket.endIndex)
          ? (valueTable.index(after: valueMemberIndex.0), 0)
          : (valueMemberIndex.0, i)

        return (name, member)
      }

      // Iterate over type members.
      let typeTable = decl.typeMemberTable!
      if typeMemberIndex < typeTable.endIndex {
        let (name, member) = typeTable[typeMemberIndex]
        typeMemberIndex = typeTable.index(after: typeMemberIndex)
        return (name, member)
      }

      // We reached the end of the sequence.
      return nil
    }

  }

  // MARK: Semantic properties

  public private(set) var state = DeclState.realized

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  /// The resolved type of the declaration.
  ///
  /// - Important: This should only be set at the time of the node's creation.
  public var type: ValType

  /// An internal table keeping track of the views to which the declared type conforms.
  var conformanceTable: ConformanceLookupTable?

  // Updates or initializes the conformance lookup table.
  func updateConformanceTable() {
    if conformanceTable == nil {
      conformanceTable = ConformanceLookupTable()
      for repr in inheritances {
        let reprType = repr.realize(unqualifiedFrom: parentDeclSpace!)
        if let viewType = reprType as? ViewType {
          conformanceTable!.insert(viewType, range: repr.range)
        }
      }

      // FIXME: Insert inherited conformance.
      // FIXME: Insert conformance declared in extensions.
    }
  }

  /// Returns the information describing the type's conformance to the specified view, or `nil` if
  /// it does not conform.
  public func conformance(to viewType: ViewType) -> ViewConformance? {
    updateConformanceTable()
    return conformanceTable![viewType]
  }

  /// The set of conformances for this type.
  public var conformances: [ViewConformance] {
    updateConformanceTable()
    return conformanceTable!.conformances
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A product type declaration.
public final class ProductTypeDecl: NominalTypeDecl, GenericTypeDecl {

  public init(
    name          : String,
    genericClause : GenericClause?   = nil,
    inheritances  : [UnqualTypeRepr] = [],
    members       : [Decl]           = [],
    type          : ValType,
    range         : SourceRange
  ) {
    self.genericClause = genericClause
    super.init(name: name, inheritances: inheritances, members: members, type: type, range: range)
  }

  /// The generic clause of the declaration.
  public var genericClause: GenericClause?

  public var hasOwnGenericParams: Bool { genericClause != nil }

  public var genericEnv: GenericEnv?

  public func prepareGenericEnv() -> GenericEnv? {
    if let env = genericEnv { return env }

    if let clause = genericClause {
      genericEnv = GenericEnv(
        space: self,
        params: clause.params.map({ $0.instanceType as! GenericParamType }),
        typeReqs: clause.typeReqs,
        context: type.context)
      guard genericEnv != nil else {
        setState(.invalid)
        return nil
      }
    } else {
      genericEnv = GenericEnv(space: self)
    }

    return genericEnv
  }

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A view type declaration.
public final class ViewTypeDecl: NominalTypeDecl {

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a generic parameter.
public final class GenericParamDecl: TypeDecl {

  public init(name: String, type: ValType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  public var name: String

  public var range: SourceRange

  public weak var parentDeclSpace: DeclSpace?

  public var isOverloadable: Bool { false }

  public func lookup(qualified name: String) -> LookupResult {
    return LookupResult()
  }

  public private(set) var state = DeclState.realized

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public var type: ValType

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A type extension declaration.
///
/// This is not a `TypeDecl`, as it does not define any type. An extension merely represents a set
/// of declarations that should be "added" to a type.
public final class TypeExtDecl: Decl, DeclSpace {

  public init(extendedIdent: IdentTypeRepr, members: [Decl], range: SourceRange) {
    self.extendedIdent = extendedIdent
    self.members = members
    self.range = range
  }

  /// The identifier of the type being extended.
  public var extendedIdent: IdentTypeRepr

  /// The member declarations of the type.
  public var members: [Decl]

  public var range: SourceRange

  // MARK: Name lookup

  /// The innermost parent in which this declaration resides.
  ///
  /// This refers to the declaration space in which the extension was parsed until it is bound.
  /// Then, this refers to the extended declaration, virtually nesting the extension within the
  /// the declaration space of the type it extends.
  public weak var parentDeclSpace: DeclSpace?

  public func lookup(qualified name: String) -> LookupResult {
    // Bind the extension and forward the lookup to the extended type.
    return computeExtendedDecl()?.lookup(qualified: name) ?? LookupResult()
  }

  // MARK: Semantic properties

  public private(set) var state = DeclState.parsed

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  /// The declaration of the extended type.
  public var extendedDecl: NominalTypeDecl? {
    return computeExtendedDecl()
  }

  /// The underlying storage for `extendedDecl`.
  private var _extendedDecl: NominalTypeDecl?

  /// Computes the declaration that is extended by this extension.
  public func computeExtendedDecl() -> NominalTypeDecl? {
    guard state != .invalid else { return nil }
    if state >= .realized {
      return _extendedDecl
    }

    let type = extendedIdent.realize(unqualifiedFrom: parentDeclSpace!)
    guard !(type is ErrorType) else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      state = .invalid
      return nil
    }

    guard let decl = (type as? NominalType)?.decl else {
      type.context.report(.nonNominalExtension(type, range: extendedIdent.range))
      state = .invalid
      return nil
    }

    parentDeclSpace = decl
    state = .realized
    return decl
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}
