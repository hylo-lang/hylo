import Basic

/// A node representing the declaration of one or more entities.
public protocol Decl: Node {

  /// The innermost parent in which this declaration resides.
  var parentDeclSpace: DeclSpace? { get }

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

      case let decl as AbstractFunDecl:
        let sign = decl.params.map({ ($0.externalName ?? "_") + ":" }).joined()
        let name = decl.name.isEmpty ? "_" : decl.name
        components.append("\(name)(\(sign))")

      case let decl as AbstractNominalTypeDecl:
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

  /// The type of the declaration.
  var type: ValType { get }

  /// A flag indicating whether the declaration is overloadable.
  var isOverloadable: Bool { get }

}

/// A type declaration.
public protocol TypeDecl: TypeOrValueDecl, DeclSpace {

  /// The type of instances of the declared type.
  var instanceType: ValType { get }

}

extension TypeDecl {

  public var instanceType: ValType {
    return (type as! KindType).type
  }

  public var isOverloadable: Bool { true }

}

/// A named declaration.
public protocol ValueDecl: TypeOrValueDecl {

  /// The type of the declaration.
  var type: ValType { get set }

  /// Realizes the semantic type of the value being declared.
  func realize() -> ValType

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

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

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

  /// A flag indicating whether the variable is mutable.
  public var isMutable: Bool {
    return patternBindingDecl?.isMutable ?? false
  }

  public var isOverloadable: Bool { false }

  public var type: ValType

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public func realize() -> ValType {
    guard type is UnresolvedType else { return type }
    type = TypeVar(context: type.context, node: self)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The base class for function declarations.
public class AbstractFunDecl: ValueDecl, DeclSpace {

  public init(
    name          : String,
    declModifiers : [DeclModifier]     = [],
    genericParams : [GenericParamDecl] = [],
    params        : [FunParamDecl]     = [],
    retTypeSign   : TypeRepr?          = nil,
    body          : BraceStmt?         = nil,
    type          : ValType,
    range         : SourceRange
  ) {
    self.name = name
    self.declModifiers = declModifiers
    self.genericParams = genericParams
    self.params = params
    self.retSign = retTypeSign
    self.body = body
    self.type = type
    self.range = range

    self.props = FunDeclProps()
    for modifier in declModifiers {
      switch modifier.kind {
      case .mut   : props.formUnion(.isMutating)
      case .static: props.formUnion(.isStatic)
      default     : continue
      }
    }
  }

  /// The name of the function (empty for anonymous functions).
  public var name: String

  /// The declaration modifiers of the function.
  ///
  /// - Note: Setting this property after initialization does not automatically updates `props`.
  public var declModifiers: [DeclModifier]

  /// The generic type parameters of the function.
  public var genericParams: [GenericParamDecl]

  /// The parameters of the function.
  public var params: [FunParamDecl]

  /// The signature of the function's return type.
  public var retSign: TypeRepr?

  /// The implicit declaration of the `self` parameter for member functions.
  ///
  /// - Note: Accessing this property will trap if the function declaration is in an extension that
  ///   hasn't been bound to a nominal type yet.
  public lazy var selfDecl: FunParamDecl? = {
    guard props.contains(.isMember) || (self is CtorDecl) else { return nil }

    let selfType: ValType
    switch parentDeclSpace {
    case let typeDecl as AbstractNominalTypeDecl:
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

  /// The body of the function.
  public var body: BraceStmt?

  /// Looks up for declarations that match the given name, directly enclosed within the function
  /// declaration space.
  ///
  /// The returned type declarations are the function's generic parameters. The value declarations
  /// are its explicit and implicit parameters. The declarations scoped within the function's body
  /// are **not** included in either of these sets. Those reside in a nested space.
  public func lookup(qualified name: String) -> LookupResult {
    let types  = genericParams.filter({ $0.name == name })
    var values = params.filter({ $0.name == name })
    if name == "self", let selfDecl = self.selfDecl {
      values.append(selfDecl)
    }

    return LookupResult(types: types, values: values)
  }

  /// The semantic properties of the declaration.
  public var props: FunDeclProps

  public var isOverloadable: Bool { true }

  /// The "applied" type of the function.
  ///
  /// This property must be kept synchronized with the function type implicitly described by the
  /// parameter list and return signature. Setting its value directly is discouraged, you should
  /// use the method `recomputeAppliedType(in:)` instead.
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

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  /// Realizes the "unapplied" type of the function from its signature.
  ///
  /// For member functions, this is the type of the function extended with an implicit receiver
  /// parameter. For other functions, this is equal to `type`.
  public func realize() -> ValType {
    guard type is UnresolvedType else { return type }

    var paramTypeElems: [TupleType.Elem] = []
    for param in params {
      _ = param.realize()
      paramTypeElems.append(TupleType.Elem(label: param.externalName, type: param.type))
    }
    let paramType = type.context.tupleType(paramTypeElems)

    let retType: ValType
    if let sign = retSign {
      retType = sign.realize(unqualifiedFrom: self)
    } else {
      retType = type.context.unitType
    }

    type = type.context.funType(paramType: paramType, retType: retType)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

  /// A set representing the properties of a function declaration.
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
public final class FunDecl: AbstractFunDecl {

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A constructor declaration.
public final class CtorDecl: AbstractFunDecl {

  public init(
    declModifiers : [DeclModifier]     = [],
    genericParams : [GenericParamDecl] = [],
    params        : [FunParamDecl]     = [],
    body          : BraceStmt?         = nil,
    type          : ValType,
    range         : SourceRange
  ) {
    super.init(
      name: "new",
      declModifiers: declModifiers,
      genericParams: genericParams,
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

  public var isOverloadable: Bool { false }

  public var type: ValType

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

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
public class AbstractNominalTypeDecl: TypeDecl, DeclSpace {

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

  /// The resolved type of the declaration.
  ///
  /// - Important: This should only be set at the time of the node's creation.
  public var type: ValType

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

  // MARK: Member lookup

  /// An internal lookup table keeping track of type members.
  var typeMemberTable: [String: [TypeDecl]]?

  /// An internal lookup table keeping track of value members.
  var valueMemberTable: [String: [ValueDecl]]?

  // Updates or initializes the member lookup tables.
  func updateMemberTables() {
    guard valueMemberTable == nil else { return }

    // Create the lookup tables.
    typeMemberTable  = [:]
    valueMemberTable = [:]

    /// Fills the member lookup table with the given declarations.
    func fill(members: [Decl]) {
      for decl in members {
        switch decl {
        case let typeDecl as TypeDecl:
          typeMemberTable![typeDecl.name, default: []].append(typeDecl)

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

    fill(members: members)
    for module in type.context.modules.values {
      for extDecl in module.extensions(of: self) {
        fill(members: extDecl.members)
      }
    }

    // FIXME: Insert members inherited by conformance.
  }

  public func lookup(qualified name: String) -> LookupResult {
    updateMemberTables()
    return LookupResult(
      types : typeMemberTable![name]  ?? [],
      values: valueMemberTable![name] ?? [])
  }

  // MARK: Conformance metadata

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

}

/// A product type declaration.
public final class ProductTypeDecl: AbstractNominalTypeDecl {

  public override func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A view type declaration.
public final class ViewTypeDecl: AbstractNominalTypeDecl {

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

  public var type: ValType

  public weak var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public func lookup(qualified name: String) -> LookupResult {
    return LookupResult()
  }

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

  /// The declaration of the extended type.
  ///
  /// - Note: Accessing this property before extension binding will trap, in order to catch invalid
  ///   attempts to perform premature type-checking.
  public var extendedDecl: AbstractNominalTypeDecl? {
    switch state {
    case .bound(let decl) : return decl
    case .invalid         : return nil
    default: fatalError("premature type checking")
    }
  }

  /// The member declarations of the type.
  public var members: [Decl]

  public var parentDeclSpace: DeclSpace?

  public var range: SourceRange

  public func bind() -> AbstractNominalTypeDecl? {
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

    state = .bound(decl)
    return decl
  }

  public func lookup(qualified name: String) -> LookupResult {
    // Bind the extension and forward the lookup to the extended type.
    return bind()?.lookup(qualified: name) ?? LookupResult()
  }

  public func accept<V>(_ visitor: V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

  /// The binding state of the declaration.
  public var state = State.parsed

  /// The compilation state of a type extension.
  public enum State: Equatable {

    /// The declaration was parsed.
    case parsed

    /// The declaration was bound to the type it extends.
    case bound(AbstractNominalTypeDecl)

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

}
