import Basic

/// A node representing the declaration of one or more entities.
public protocol Decl: Node {

  /// The innermost parent in which this declaration resides.
  var parentDeclSpace: DeclSpace? { get }

  /// A flag that indicates whether this declaration is semantically ill-formed.
  var isInvalid: Bool { get }

  /// Mark the declaration invalid.
  ///
  /// Once marked, the declaration will be ignored by subsequent semantic analysis phases, without
  /// producing any further diagnostic.
  func setInvalid()

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A declaration visitor.
  func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor

}

extension Decl {

  /// A debug identifier for the declaration.
  public var debugID: String {
    var components: [String] = []
    var next: Decl? = self

    outer:while let node = next {
      switch node {
      case let delc as Module:
        components.append(delc.name)

      case let decl as VarDecl:
        components.append(decl.name)

      case let decl as FunParamDecl:
        components.append(decl.name)

      case let decl as BaseFunDecl:
        let sign = decl.params.map({ ($0.externalName ?? "_") + ":" }).joined()
        let name = decl.name.isEmpty ? "_" : decl.name
        components.append("\(name)(\(sign))")

      case let decl as NominalTypeDecl:
        components.append(decl.name)

      case let decl as TypeExtDecl:
        components.append(contentsOf: decl.extendedIdent.components.map({ $0.name }))
        break

      default:
        let id = String(Int(bitPattern: ObjectIdentifier(node)), radix: 36)
        components.append("\(type(of: node))@\(id)")
      }

      var parent = node.parentDeclSpace
      while parent != nil {
        if let decl = parent! as? Decl {
          next = decl
          continue outer
        } else {
          parent = parent!.parentDeclSpace
        }
      }
      break
    }

    return components.reversed().joined(separator: "::")
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

/// A named declaration.
public protocol ValueDecl: TypeOrValueDecl {

  /// The type of the declaration.
  var type: ValType { get set }

  /// Realizes the semantic type of the value being declared.
  func realize() -> ValType

}

extension ValueDecl {

  /// Contextualize the type of this declaration from the given use site.
  ///
  /// - Parameter useSite: The declaration space from which the declaration is being referred.
  public func contextualize(from useSite: DeclSpace) -> ValType {
    let genericType = realize()
    guard genericType.props.contains(.hasTypeParams) else { return genericType }

    // If the declaration is its own generic environment, then we must contextualize it externally,
    // regardless of the use-site. This situation corresponds to a "fresh" use of a generic
    // declaration within its own space (e.g., a recursive call to a generic function).
    if let gds = self as? GenericDeclSpace {
      guard let env = gds.prepareGenericEnv() else {
        return type.context.errorType
      }
      return env.contextualize(genericType, from: gds)
    }

    // Find the innermost generic space, relative to this declaration. We can assume there's one,
    // otherwise `realize()` would have failed to resolve the decl.
    guard let env = parentDeclSpace!.innermostGenericSpace!.prepareGenericEnv() else {
      return type.context.errorType
    }
    return env.contextualize(genericType, from: useSite)
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

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
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

  /// A flag indicating whether the variable is mutable.
  public var isMutable: Bool {
    return patternBindingDecl?.isMutable ?? false
  }

  public weak var parentDeclSpace: DeclSpace?

  public var isOverloadable: Bool { false }

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
  }

  public var type: ValType

  public func realize() -> ValType {
    if (type is UnresolvedType) || (type is TypeVar) {
      return type
    }

    // Create a fresh type variable.
    type = TypeVar(context: type.context, node: self)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

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

  /// The semantic properties of the declaration.
  public var props: FunDeclProps

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

    let selfType: ValType
    switch parentDeclSpace {
    case let typeDecl as NominalTypeDecl:
      // The declaration is in the body of a nominal type.
      selfType = typeDecl.instanceType

    case let typeExtDecl as TypeExtDecl:
      // The declaration is in the body of a type extension.
      if let extendedType = typeExtDecl.extendedDecl?.instanceType {
        selfType = extendedType
      } else {
        selfType = type.context.unresolvedType
      }

    default:
      fatalError("unreachable")
    }

    let decl = FunParamDecl(
      name: "self", externalName: "self",
      type: props.contains(.isMutating)
        ? type.context.inoutType(of: selfType)
        : selfType,
      range: .invalid)
    decl.parentDeclSpace = self
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

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
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
    guard type is UnresolvedType else { return type }

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
        setInvalid()
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
    guard type is UnresolvedType else { return type }

    var paramTypeElems: [TupleType.Elem] = []
    for param in params {
      _ = param.realize()
      paramTypeElems.append(TupleType.Elem(label: param.externalName, type: param.type))
    }
    let paramType = type.context.tupleType(paramTypeElems)
    let retType = (selfDecl!.type as! InoutType).base

    type = type.context.funType(paramType: paramType, retType: retType)
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

  public var isOverloadable: Bool { false }

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
  }

  public var type: ValType

  public func realize() -> ValType {
    guard type is UnresolvedType else { return type }

    if let sign = self.sign {
      type = sign.realize(unqualifiedFrom: parentDeclSpace!)
    } else {
      type = TypeVar(context: type.context, node: self)
    }
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
  public var members: [Decl]

  public var range: SourceRange

  // MARK: Name lookup

  public weak var parentDeclSpace: DeclSpace?

  public var isOverloadable: Bool { false }

  /// An internal lookup table keeping track of type members.
  var typeMemberTable: [String: TypeDecl]?

  /// An internal lookup table keeping track of value members.
  var valueMemberTable: [String: [ValueDecl]]?

  // Updates or initializes the member lookup tables.
  func updateMemberTables() {
    guard valueMemberTable == nil else { return }

    // Initialize type lookup tables.
    typeMemberTable  = [:]
    valueMemberTable = [:]

    /// Fills the member lookup table with the given declarations.
    func fill(members: [Decl]) {
      for decl in members where !decl.isInvalid {
        switch decl {
        case let typeDecl as TypeDecl:
          // Check for invalid redeclarations.
          guard typeMemberTable![typeDecl.name] == nil else {
            type.context.report(
              .duplicateDeclaration(symbol: typeDecl.name, range: typeDecl.range))
            typeDecl.setInvalid()
            continue
          }

          typeMemberTable![typeDecl.name] = typeDecl

        case let valueDecl as ValueDecl:
          valueMemberTable![valueDecl.name, default: []].append(valueDecl)

        case let pbDecl as PatternBindingDecl:
          for pattern in pbDecl.pattern.namedPatterns {
            valueMemberTable![pattern.decl.name, default: []].append(pattern.decl)
          }

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

    // FIXME: Populate the lookup table with members inherited by conformance.
  }

  public func lookup(qualified name: String) -> LookupResult {
    updateMemberTables()
    return LookupResult(
      types : typeMemberTable![name].map({ [$0] }) ?? [],
      values: valueMemberTable![name] ?? [])
  }

  // MARK: Semantic properties

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
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
        if let viewType = repr.type as? ViewType {
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
public final class ProductTypeDecl: NominalTypeDecl, GenericDeclSpace {

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
        setInvalid()
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

  public private(set) var isInvalid = false

  public func setInvalid() {
    isInvalid = true
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

  public var isInvalid: Bool { state == .invalid }

  public func setInvalid() {
    state = .invalid
  }

  /// The declaration of the extended type.
  public var extendedDecl: NominalTypeDecl? {
    return computeExtendedDecl()
  }

  /// Computes the declaration that is extended by this extension.
  public func computeExtendedDecl() -> NominalTypeDecl? {
    guard state != .invalid else { return nil }
    if case .bound(let decl) = state {
      return decl
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
    state = .bound(decl)
    return decl
  }

  /// The binding state of the declaration.
  public var state = State.parsed

  /// The compilation state of a type extension.
  public enum State: Equatable {

    /// The declaration was parsed.
    case parsed

    /// The declaration was bound to the type it extends.
    case bound(NominalTypeDecl)

    /// The declaration is invalid and can't be bound to any type.
    ///
    /// The compiler should emit a diagnostic when assigning this state. All further attempt to use
    /// the contents of this extension should be ignored and not re-diagnosed.
    case invalid

    public static func == (lhs: State, rhs: State) -> Bool {
      switch (lhs, rhs) {
      case (.parsed , .parsed)              : return true
      case (.invalid, .invalid)             : return true
      case (.bound(let d1), .bound(let d2)) : return d1 === d2
      default: return false
      }
    }

  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}
