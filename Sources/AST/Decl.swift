import Basic
import OrderedCollections

/// A node representing the declaration of one or more entities.
public protocol Decl: Node {

  /// The innermost parent in which this declaration resides.
  var parentDeclSpace: DeclSpace? { get }

  /// The (semantic) state of the declaration.
  var state: DeclState { get }

  /// Sets the state of this declaration.
  ///
  /// - Parameter newState: The new state of the declaration. `newState` must be a valid successor
  ///   of the declaration's current state.
  func setState(_ newState: DeclState)

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A declaration visitor.
  func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor

}

/// The state of a declaration, as it goes through type checking.
public enum DeclState: Int, Comparable {

  /// The declaration was just parsed; no semantic information is available.
  case parsed

  /// The type of the declaration has been requested and is being realized.
  case realizationRequested

  /// The type of the declaration is available for type inference, but the declaration has not been
  /// type checked yet.
  ///
  /// The precise meaning of this state depends on the declaration.
  /// * On type declarations, function declarations, and parameter declarations, it indicates that
  ///   the `type` property has been realized.
  /// * On extension declarations, it indicates that the extended type has been bound.
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

  /// The semantic type of the declaration, uncontextualized.
  var type: ValType { get }

  /// A flag indicating whether the declaration is overloadable.
  var isOverloadable: Bool { get }

}

/// A type declaration.
public protocol TypeDecl: TypeOrValueDecl {

  /// The (unbound) type of instances of the declared type.
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

  /// A flag indicating whether the declaration describes the member of a type.
  var isMember: Bool { get }

  /// The type of the declaration.
  var type: ValType { get set }

}

/// A module import declaration.
public final class ImportDecl: Decl {

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  public var state = DeclState.parsed

  /// The name of the module being imported.
  public var name: String

  public init(name: String) {
    self.name = name
  }

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A declaration that consists of a pattern and an optional initializer for the variables declared
/// in this pattern.
public final class PatternBindingDecl: Decl {

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  public var state = DeclState.parsed

  /// The declaration modifiers of the declaration.
  ///
  /// This array only contains the modifiers specified at the creation of the declaration. It may
  /// not accurately match the corresponding semantic properties of the declaration.
  public let modifiers: [DeclModifier]

  /// A flag indicating whether the declared variables are mutable.
  public var isMutable: Bool

  /// The pattern being bound.
  public var pattern: Pattern

  /// The declaration of each variable introduced by the pattern.
  ///
  /// This property is essentially a cache gathering the declaration of each named sub-pattern.
  public var varDecls: [VarDecl] = []

  /// The signature of the pattern.
  public var sign: Sign?

  /// The initializer for the variables declared by the pattern.
  public var initializer: Expr?

  /// The source range of the `val` or `var` keyword at the start of the declaration.
  public var introRange: SourceRange?

  public init(
    modifiers: [DeclModifier] = [],
    isMutable: Bool,
    pattern: Pattern,
    sign: Sign?,
    initializer: Expr?
  ) {
    self.modifiers = modifiers
    self.isMutable = isMutable
    self.pattern = pattern
    self.sign = sign
    self.initializer = initializer
  }

  /// A flag indicating whether the declaration describes member variables.
  public var isMember: Bool {
    return (parentDeclSpace is NominalTypeDecl || parentDeclSpace is TypeExtnDecl)
  }

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A variable declaration.
///
/// Variable declarations are not top-level AST nodes. Instead, they result from constructions that
/// bind named patterns. For instance, `val (foo, bar) = f()` is parsed as a declaration that binds
/// a tuple pattern to an expression. The tuple pattern contains two variable declarations, one for
/// each named pattern.
public final class VarDecl: ValueDecl {

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  public var state = DeclState.parsed

  public var type: ValType

  /// The variable's identifier.
  public var ident: Ident

  /// The pattern binding declaration that introduces this variable declaration, if any.
  ///
  /// This is `nil` if the variable being declared is introduced by a case statement or by an
  /// explicit capture declaration.
  public weak var patternBindingDecl: PatternBindingDecl?

  /// The backend of the variable.
  public var backend = VarBackend.storage

  /// A flag indicating whether the variable has storage.
  public var hasStorage: Bool { backend == .storage }

  /// A flag indicating whether the variable is mutable.
  public var isMutable = false

  public init(ident: Ident, type: ValType) {
    self.ident = ident
    self.type = type
  }

  public var name: String { ident.name }

  public var isOverloadable: Bool { false }

  public var isMember: Bool { patternBindingDecl?.isMember ?? false }

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
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

/// The base class for generic type or value declarations.
public class BaseGenericDecl: GenericDeclSpace {

  public weak var parentDeclSpace: DeclSpace?

  public var genericEnv: GenericEnv?

  /// The (semantic) state of the declaration.
  public private(set) var state: DeclState

  /// The semantic type of the declaration, outside of its generic context.
  public var type: ValType

  /// The generic clause of the declaration.
  public var genericClause: GenericClause?

  fileprivate init(type: ValType, state: DeclState) {
    self.type = type
    self.state = state
  }

  public var hasOwnGenericParams: Bool { genericClause != nil }

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

  /// Sets the state of this declaration.
  ///
  /// - Parameter newState: The new state of the declaration. `newState` must be a valid successor
  ///   of the declaration's current state.
  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func lookup(qualified name: String) -> LookupResult {
    fatalError("unreachable")
  }

}

/// The base class for function declarations.
public class BaseFunDecl: BaseGenericDecl, ValueDecl {

  /// A set representing various properties of a function declaration.
  public struct FunDeclProps: OptionSet {

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }

    public let rawValue: Int

    public static let isMember      = FunDeclProps(rawValue: 1 << 0)
    public static let isMutating    = FunDeclProps(rawValue: 1 << 1)
    public static let isStatic      = FunDeclProps(rawValue: 1 << 2)
    public static let isBuiltin     = FunDeclProps(rawValue: 1 << 3)
    public static let isSynthesized = FunDeclProps(rawValue: 1 << 4)

  }

  public init(
    ident: Ident? = nil,
    modifiers: [DeclModifier] = [],
    params: [FunParamDecl] = [],
    retTypeSign: Sign? = nil,
    type: ValType
  ) {
    self.ident = ident
    self.modifiers = modifiers
    self.params = params
    self.retSign = retTypeSign

    // Process the function modifiers.
    self.props = FunDeclProps()
    for modifier in modifiers {
      switch modifier.kind {
      case .mut: props.formUnion(.isMutating)
      case .static: props.formUnion(.isStatic)
      default: continue
      }
    }

    super.init(type: type, state: .parsed)
  }

  // MARK: Source properties

  public var range: SourceRange?

  /// The source range of function introducer at the start of the declaration.
  public var introRange: SourceRange?

  /// The identifier of the function, if any.
  public var ident: Ident?

  /// The name of the function (empty for anonymous functions).
  public var name: String { ident?.name ?? "" }

  /// The declaration modifiers of the function.
  ///
  /// This array only contains the declaration modifiers specified explicitly with the function
  /// declaration. It may not accurately describe the relevant bits in `props`, nor vice-versa.
  public let modifiers: [DeclModifier]

  /// The explicit capture list of the function.
  ///
  /// This property only contains the captures declared explicitly in the function's capture list.
  /// Use `computeAllCaptures(recompute:)` to get the exhaustive list of captured declarations
  /// after name resolution completed.
  public var explicitCaptures: [CaptureDecl] = []

  /// The parameters of the function.
  public var params: [FunParamDecl]

  /// The signature of the function's return type.
  public var retSign: Sign?

  /// The body of the function.
  public var body: BraceStmt?

  /// The expression that consitutes the body of the function, if that body only contains a single
  /// expression, or a single return statement (e.g., `bar` in `fun foo() { ret bar }`).
  public var singleExprBody: Expr? {
    guard let body = body else { return nil }

    if body.stmts.count == 1 {
      switch body.stmts[0] {
      case let expr as Expr:
        return expr
      case let stmt as RetStmt:
        return stmt.value ?? TupleExpr(elems: [], type: type.context.unitType)
      default:
        return nil
      }
    } else if body.stmts.isEmpty {
      return TupleExpr(elems: [], type: type.context.unitType)
    } else {
      return nil
    }
  }

  /// The local discriminator for the function.
  ///
  /// This is the index of the function in the sequence of anonymous declarations in the parent
  /// declaration space.
  public var discriminator = 0

  /// The semantic properties of the declaration.
  public var props: FunDeclProps

  /// Indicates whether the function is nested in the declaration of another function.
  public var isNested: Bool {
    guard !isStatic && !isMember else { return false }

    return !(parentDeclSpace is SourceUnit)
      && !isStatic
      && !isMember
      && !(self is CtorDecl)
  }

  /// Indicates whether the function is a member of a type.
  ///
  /// - Note: Constructors are *not* considered to be member functions.
  public var isMember: Bool { props.contains(.isMember) }

  /// Indicates whether the function is mutating its receiver.
  public var isMutating: Bool { props.contains(.isMutating) }

  /// Indicates whether the function is static.
  public var isStatic: Bool { props.contains(.isStatic) }

  /// Indicates whether the function is built-in.
  public var isBuiltin: Bool { props.contains(.isBuiltin) }

  /// Indicates whether the declaration is synthesized.
  ///
  /// Synthesized declarations are created during semantic analysis. The may not necessarily have
  /// a well-formed syntactic representation (e.g., a synthesized method declaration does need to
  /// have a body).
  public var isSynthesized: Bool { props.contains(.isSynthesized) }

  // MARK: Implicit declarations

  /// The implicit declaration of the `self` parameter for member functions.
  ///
  /// - Note: Accessing this property will trap if the function declaration is in an extension that
  ///   hasn't been bound to a nominal type yet.
  public private(set) lazy var selfDecl: FunParamDecl? = {
    // Only constructors and member methods have a implicit `self` parameter.
    guard isMember || (self is CtorDecl) else { return nil }

    // Compute the type of `self` in the context of the function.
    let receiverType: ValType
    switch parentDeclSpace {
    case let typeDecl as NominalTypeDecl:
      // The function is declared in the body of a nominal type.
      receiverType = typeDecl.receiverType

    case let extnDecl as TypeExtnDecl:
      // The function is declared in the body of a type extension.
      guard let typeDecl = extnDecl.extendedDecl else {
        let decl = FunParamDecl(name: "self", externalName: nil, type: type.context.errorType)
        decl.setState(.invalid)
        return decl
      }
      receiverType = typeDecl.receiverType

    default:
      fatalError("unreachable")
    }

    let decl = FunParamDecl(
      name: "self",
      externalName: nil,
      type: isMutating ? type.context.inoutType(of: receiverType) : receiverType)
    decl.parentDeclSpace = self
    decl.setState(.typeChecked)
    return decl
  }()

  /// A cache that stores the capture table.
  private var captureTable: CaptureTable?

  /// Returns the set of all declarations that are captured in the function's closure, explicitly
  /// implicitly (i.e., without an explicit declaration in function's capture list).
  ///
  /// - Parameter recompute: A flag that indicates whether to use cached results for, if available.
  ///
  /// - Note: Implicit captures are identified using resolved declaration references only. Hence,
  ///   this method should not be called before type checking is complete.
  public func computeAllCaptures(recomputeImplicit: Bool = false) -> CaptureTable {
    if let table = self.captureTable, !recomputeImplicit {
      return table
    }

    // Non-nested functions cannot capture any declaration.
    guard isNested else {
      captureTable = CaptureTable()
      return captureTable!
    }

    // Initialize the capture table with the list of explicit captures.
    var collector = CaptureCollector(relativeTo: parentDeclSpace)
    for capture in explicitCaptures {
      guard let decl = capture.capturedDecl else { continue }
      collector.table[decl] = CaptureTable.Value(referredDecl: capture, type: capture.type)
    }

    // Compute the set of implicit captures.
    collector.walk(decl: self)
    captureTable = collector.table

    return collector.table
  }

  // MARK: Name Lookup

  public var isOverloadable: Bool { true }

  /// Looks up for declarations that match the given name, directly enclosed within the function
  /// declaration space.
  ///
  /// The returned type declarations are the function's generic parameters. The value declarations
  /// are its explicit and implicit parameters. The declarations scoped within the function's body
  /// are **not** included in either of these sets. Those reside in a nested space.
  public override func lookup(qualified name: String) -> LookupResult {
    let types = genericClause.map({ clause in clause.params.filter({ $0.name == name }) }) ?? []

    var values: [ValueDecl] = params.filter({ $0.name == name })
    if name == "self", let selfDecl = self.selfDecl {
      values.append(selfDecl)
    }
    if !explicitCaptures.isEmpty {
      values.append(contentsOf: explicitCaptures.filter({ $0.name == name }))
    }

    return LookupResult(types: types, values: values)
  }

  // MARK: Semantic properties

  public override func setState(_ newState: DeclState) {
    assert((newState != .typeCheckRequested) || (state >= .realized),
           "type checking requested before the function signature was realized")
    super.setState(newState)
  }

  /// The "unapplied" type of the function.
  ///
  /// Member functions accept a receiver (i.e., `self`) as an implicit first parameter. Hence, a
  /// a call to a method `T -> U` is actually a call to an *unapplied* function `(Self, T) -> U`,
  /// where `Self` is the receiver's type. This property denotes the *applied* type, which should
  /// be understood as the return type the unapplied function's partial application. The unapplied
  /// type of a member function is the type of its declaration, extended with an implicit receiver
  /// parameter. For non-member functions, it is equal the declaration's type.
  ///
  /// This property must be kept synchronized with the function type implicitly described by the
  /// parameter list and return signature. Setting its value directly is discouraged, you should
  /// use `realize()` instead.
  public var unappliedType: ValType {
    guard isMember else { return type }

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

  // MARK: Visitation

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A function declaration.
public final class FunDecl: BaseFunDecl {

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A constructor declaration.
public final class CtorDecl: BaseFunDecl {

  public init(modifiers: [DeclModifier] = [], params: [FunParamDecl] = [], type: ValType) {
    super.init(
      ident: Ident(name: "new"),
      modifiers: modifiers,
      params: params,
      type: type)
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

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a capture in a function's capture list.
public final class CaptureDecl: ValueDecl {

  /// The semantics of a capture.
  public enum Semantics {

    /// The declaration is captured as an immutable copy.
    case `val`

    /// The declaration is captured as a mutable copy.
    case `var`

    /// The declaration is captured as an exclusive mutable reference.
    case `mut`

  }

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  public var state = DeclState.parsed

  public var type: ValType

  /// The semantics of the capture.
  public var semantics: Semantics

  /// The identifier of the declaration being captured.
  public var ident: Ident

  /// The declaration being captured.
  public weak var capturedDecl: ValueDecl?

  public init(semantics: Semantics, ident: Ident, type: ValType, range: SourceRange?) {
    self.semantics = semantics
    self.ident = ident
    self.type = type
    self.range = range
  }

  public var name: String { ident.name }

  public var isMember: Bool { false }

  public var isOverloadable: Bool { false }

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V : DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a function parameter.
public final class FunParamDecl: ValueDecl {

  public var range: SourceRange?

  public weak var parentDeclSpace: DeclSpace?

  public var state = DeclState.parsed

  public var type: ValType

  /// The internal name of the parameter.
  public var name: String

  /// The external name of the parameter.
  public var externalName: String?

  /// The signature of the parameter's type.
  public var sign: Sign?

  public init(name: String, externalName: String? = nil, typeSign: Sign? = nil, type: ValType) {
    self.name = name
    self.externalName = externalName
    self.sign = typeSign
    self.type = type
  }

  public var isOverloadable: Bool { false }

  public var isMember: Bool { false }

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

  public func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A type declaration that can have explicit or implicit generic parameters.
public class GenericTypeDecl: BaseGenericDecl, TypeDecl {

  public var range: SourceRange?

  /// The source range of type introducer at the start of the declaration.
  public var introRange: SourceRange?

  /// The name of the type.
  public var name: String = ""

  /// The views to which the type should conform.
  public var inheritances: [Sign] = []

  /// The internal cache backing `valueMemberTable`.
  fileprivate var _valueMemberTable: [String: [ValueDecl]] = [:]

  /// The internal cache backing `typeMemberTable`.
  fileprivate var _typeMemberTable: [String: TypeDecl] = [:]

  /// The internal cache backing `conformanceTable`.
  fileprivate var _conformanceTable: ReferenceTable<ViewType, ViewConformance> = [:]

  /// The context generation for which the member tables were last updated.
  fileprivate var memberTablesGeneration = -1

  /// The context generation for which `_conformanceTable` was last updated.
  fileprivate var conformanceTableGeneration = -1

  fileprivate init(name: String, type: ValType, state: DeclState) {
    self.name = name
    super.init(type: type, state: state)
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

  // MARK: Name lookup

  /// An iterator over all the member declarations of a type.
  public struct MemberIterator: IteratorProtocol, Sequence {

    public typealias Element = (name: String, decl: TypeOrValueDecl)

    fileprivate init(decl: GenericTypeDecl) {
      self.decl = decl
      self.valueMemberIndex = (decl._valueMemberTable.startIndex, 0)
      self.typeMemberIndex = decl._typeMemberTable.startIndex
    }

    fileprivate unowned let decl: GenericTypeDecl

    fileprivate var valueMemberIndex: (Dictionary<String, [ValueDecl]>.Index, Int)

    fileprivate var typeMemberIndex: Dictionary<String, TypeDecl>.Index

    public mutating func next() -> (name: String, decl: TypeOrValueDecl)? {
      // Iterate over value members.
      let valueTable = decl._valueMemberTable
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
      let typeTable = decl._typeMemberTable
      if typeMemberIndex < typeTable.endIndex {
        let (name, member) = typeTable[typeMemberIndex]
        typeMemberIndex = typeTable.index(after: typeMemberIndex)
        return (name, member)
      }

      // We reached the end of the sequence.
      return nil
    }

  }

  public var isOverloadable: Bool { false }

  /// A lookup table keeping track of value members.
  public var valueMemberTable: [String: [ValueDecl]] {
    updateMemberTables()
    return _valueMemberTable
  }

  /// A lookup table keeping track of type members.
  public var typeMemberTable: [String: TypeDecl] {
    updateMemberTables()
    return _typeMemberTable
  }

  public override func lookup(qualified name: String) -> LookupResult {
    updateMemberTables()
    return LookupResult(
      types : _typeMemberTable[name].map({ [$0] }) ?? [],
      values: _valueMemberTable[name] ?? [])
  }

  // Updates or initializes the member lookup tables.
  public func updateMemberTables() {
    guard memberTablesGeneration < type.context.generation else { return }

    // Initialize the lookup tables with the direct members of the type.
    if memberTablesGeneration < 0 {
      // Populate the lookup table with generic parameters.
      if let clause = genericClause {
        fill(members: clause.params)
      }

      // Populate the lookup table with direct members.
      fill(members: directMembers())
    }

    // Populate the lookup table with members declared in extensions.
    for module in type.context.modules.values where module.generation > memberTablesGeneration {
      for extDecl in module.extensions(of: self) {
        fill(members: extDecl.members)
      }
    }

    // Populate the lookup table with concrete members inherited by conformance.
    // FIXME: Insert members that have a default implementation, or are defined in extensions.
    // let newConfs = updateConformanceTable()
    // for conformance in newConfs {
    //   fill(members: conformance.viewDecl.members)
    // }

    memberTablesGeneration = type.context.generation
  }

  /// Returns the member declarations that resides directly within the type's declaration space.
  fileprivate func directMembers() -> [Decl] {
    return []
  }

  /// Fills the member lookup table with the given declarations.
  private func fill<S>(members: S) where S: Sequence, S.Element == Decl {
    for decl in members where decl.state != .invalid {
      switch decl {
      case let decl as ValueDecl:
        // FIXME: Handle invalid redeclarations of non-overloadable symbols.
        _valueMemberTable[decl.name, default: []].append(decl)

      case let decl as PatternBindingDecl:
        // FIXME: Handle invalid redeclarations of non-overloadable symbols.
        for pattern in decl.pattern.namedPatterns {
          _valueMemberTable[pattern.decl.name, default: []].append(pattern.decl)
        }

      case let decl as TypeDecl:
        // Check for invalid redeclarations.
        guard _typeMemberTable[decl.name] == nil else {
          type.context.report(.duplicateDeclaration(symbol: decl.name, range: decl.range))
          decl.setState(.invalid)
          continue
        }
        _typeMemberTable[decl.name] = decl

      default:
        break
      }
    }
  }

  /// A sequence with all members in this type.
  public var allTypeAndValueMembers: MemberIterator {
    updateMemberTables()
    return MemberIterator(decl: self)
  }

  // MARK: View conformance

  /// A lookup table keeping track of the views to which the declared type conforms.
  public var conformanceTable: ReferenceTable<ViewType, ViewConformance> {
    get {
      updateConformanceTable()
      return _conformanceTable
    }
    set {
      _conformanceTable = newValue
    }
    _modify {
      updateConformanceTable()
      yield &_conformanceTable
    }
  }

  /// Updates or initializes the conformance lookup table.
  ///
  /// - Returns: The new conformances that have been created.
  @discardableResult
  public func updateConformanceTable() -> [ViewConformance] {
    guard conformanceTableGeneration < type.context.generation else { return [] }

    // FIXME: Insert inherited conformance.
    // FIXME: Insert conformance declared in extensions.

    // Initialize the conformance table with the inheritance clause of the type declaration.
    var newConfs: [ViewConformance] = []
    if conformanceTableGeneration < 0 {
      for sign in inheritances {
        let signType = sign.realize(unqualifiedFrom: parentDeclSpace!)
        if let viewType = signType as? ViewType {
          let conf = ViewConformance(viewDecl: viewType.decl as! ViewTypeDecl, range: sign.range)
          _conformanceTable[viewType] = conf
          newConfs.append(conf)
        }
      }
    }

    conformanceTableGeneration = type.context.generation
    return newConfs
  }

  /// Returns whether the declared type conforms to the given view.
  ///
  /// - Parameter viewType: A view.
  /// - Returns: `true` if there exists a type checked conformance between the declared type and
  ///   the given view; otherwise, `false`.
  public func conforms(to viewType: ViewType) -> Bool {
    updateConformanceTable()
    guard let conformance = _conformanceTable[viewType] else { return false }
    return conformance.state == .checked
  }

  /// Returns the declaration that witnesses the given requirement.
  ///
  /// - Parameter requirement: A value requirement, defined in a conformed view.
  public func witness(for requirement: ValueDecl) -> ValueDecl? {
    for conformance in conformanceTable.values {
      if let decl = conformance.witness(for: requirement) {
        return decl
      }
    }
    return nil
  }

  /// Returns the declaration that witnesses the given requirement.
  ///
  /// - Parameter requirement: A type requirement, defined in a conformed view.
  public func witness(for requirement: TypeDecl) -> TypeDecl? {
    for conformance in conformanceTable.values {
      if let decl = conformance.witness(for: requirement) {
        return decl
      }
    }
    return nil
  }

  // MARK: Semantic properties

  /// The uncontextualized type of a reference to `self` within the context of this type.
  public var receiverType: ValType { fatalError("unreachable") }

  // MARK: Memory layout

  /// The list of properties that have storage in this type.
  public var storedVars: [VarDecl] { [] }

}

/// The base class for nominal type declarations.
public class NominalTypeDecl: GenericTypeDecl {

  /// The "direct" member declarations of the type.
  ///
  /// This collection contains the member declarations found directly within to scope of this type
  /// declaration. This may include synthesized members, created during the semantic analysis, for
  /// which a concrete source representation does not exist.
  ///
  /// Do not use this property for qualified name lookups. It does not contain members declared or
  /// synthesized in extensions, or inherited by conformance. You should use `lookup(qualified:)`
  /// for specific lookups, or `allTypeAndValueMembers` to enumerate all member declarations.
  public var members: [Decl] = []

  public init(name: String, type: ValType) {
    super.init(name: name, type: type, state: .realized)
  }

  fileprivate override func directMembers() -> [Decl] {
    return members
  }

  /// The list of properties that have storage in this type.
  public override var storedVars: [VarDecl] {
    var result: [VarDecl] = []
    for case let pbDecl as PatternBindingDecl in members {
      // FIXME: Filter out computed properties.
      result.append(contentsOf: pbDecl.varDecls)
    }
    return result
  }

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A product type declaration.
public final class ProductTypeDecl: NominalTypeDecl {

  /// The uncontextualized type of a reference to `self` within the context of this type.
  ///
  /// If the type declaration introduces its own generic type parameters, this wraps `instanceType`
  /// within a `BoundGenericType` where each parameter is bound to itself. Otherwise, this is the
  /// same as `instanceType`.
  public override var receiverType: ValType {
    if let clause = genericClause {
      // The instance type is generic (e.g., `Foo<Bar>`).
      return type.context.boundGenericType(
        decl: self,
        args: clause.params.map({ $0.instanceType }))
    } else {
      return instanceType
    }
  }

  public override func updateMemberTables() {
    super.updateMemberTables()

    // Synthetize default constructors if necessary.
    if _valueMemberTable["new"] == nil {
      let context = type.context

      // Create a constructor declaration.
      let ctor = CtorDecl(type: context.unresolvedType)
      ctor.range = introRange
      ctor.parentDeclSpace = self
      ctor.props.insert(.isSynthesized)

      // Create a parameter for each stored property.
      ctor.params = storedVars.map({ (varDecl: VarDecl) -> FunParamDecl in
        let param = FunParamDecl(
          name: varDecl.name,
          externalName: varDecl.name,
          typeSign: nil,
          type: context.unresolvedType)
        param.parentDeclSpace = ctor
        return param
      })

      // Insert the synthetized declaration into the member's table.
      _valueMemberTable["new"] = [ctor]
      members.append(ctor)
    }
  }

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A view type declaration.
///
/// A view declaration delimits a generic space with at least one type parameter `Self`, denoting
/// its conforming type existentially, as well as additional parameters for each abstract type
/// declared in the view. These are synthesized when the view's generic environment is prepared.
///
/// Conceptually, a view `V` has an implicit clause `<Self, A1, ... where Self: V, A1: Q1, ...>`,
/// where `A1, ...` are `V`'s abstract types and `Q1, ....` are the views to which these abstract
/// types conform.
public final class ViewTypeDecl: NominalTypeDecl {

  /// The implicit declaration of the `Self` generic type parameter.
  public lazy var selfTypeDecl: GenericParamDecl = {
    let paramDecl = GenericParamDecl(name: "Self", type: type.context.unresolvedType)
    paramDecl.range = introRange
    paramDecl.parentDeclSpace = self
    paramDecl.type = type.context.genericParamType(decl: paramDecl).kind
    paramDecl.setState(.typeChecked)
    return paramDecl
  }()

  /// The uncontextualized type of a reference to `self` within the context of this type.
  public override var receiverType: ValType { selfTypeDecl.instanceType }

  /// Always returns `true` -- views have a unique, implicit generic type parameter `Self`.
  public override var hasOwnGenericParams: Bool { true }

  public override func updateMemberTables() {
    super.updateMemberTables()

    // Add the synthesized generic parameters to the type member table.
    if _typeMemberTable["Self"] == nil {
      _typeMemberTable["Self"] = selfTypeDecl
    }

    assert(_typeMemberTable["Self"] === selfTypeDecl)
  }

  /// Prepares the view's generic environment.
  ///
  /// This method synthesize the type requirements on `Self` and completes the generic environment
  /// with the abstract types declared in the view.
  public override func prepareGenericEnv() -> GenericEnv? {
    if let env = genericEnv { return env }

    // Synthesize the requirement `Self: V`.
    let selfType = selfTypeDecl.instanceType as! GenericParamType
    let selfReq = TypeReq(
      kind: .conformance,
      lhs: BareIdentSign(ident: Ident(name: "Self"), type: selfType),
      rhs: BareIdentSign(ident: Ident(name: name), type: instanceType),
      range: selfTypeDecl.range)

    // Collect the abstract types of the view, and their requirements.
    var abstractTypes: [GenericParamType] = []
    var abstractReqs: [TypeReq] = []

    for case let decl as AbstractTypeDecl in members {
      // The declaration should have a generic parameter type.
      guard let type = decl.instanceType as? GenericParamType else {
        assert(decl.state == .invalid)
        continue
      }
      abstractTypes.append(type)

      // Desugar the inheritance clause as regular type requirements.
      let inheritanceReqs = decl.inheritances.map({ (sign) -> TypeReq in
        return TypeReq(
          kind: .conformance,
          lhs: BareIdentSign(ident: Ident(name: decl.name, range: decl.range), type: type),
          rhs: sign,
          range: sign.range)
      })
      abstractReqs.append(contentsOf: inheritanceReqs)
      abstractReqs.append(contentsOf: decl.typeReqs)
    }

    // Create and return the view's generic environment.
    genericEnv = GenericEnv(
      space: self,
      params: [selfType] + abstractTypes,
      typeReqs: [selfReq] + abstractReqs,
      context: type.context)
    return genericEnv
  }

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A type alias declaration.
///
/// An alias declaration denotes either a "true" alias, or a type definition. The former merely
/// introduces a synonym for an existing nominal type (e.g., `type Num = Int`), while the latter
/// gives a name to a type expression (e.g., `type Handler = Event -> Unit`).
///
/// A type definition is not a mere syntactic sugar. Instead, it is treated as a proper type for
/// which one may define view conformances and declare type extensions. These only apply on the
/// type definition and have no effect on the underlying expression.
public final class AliasTypeDecl: GenericTypeDecl {

  /// The signature of the aliased type.
  public var aliasedSign: Sign

  public init(name: String, aliasedSign: Sign, type: ValType) {
    self.aliasedSign = aliasedSign
    super.init(name: name, type: type, state: .parsed)
  }

  /// If the declaration is a "true" type alias, the aliased declaration. Otherwise, `nil`.
  ///
  /// - Warning: Do not call this property before parsing is completed. Computing it requires the
  ///   type realization of the aliased signature, which may trigger unqualified name lookups to
  ///   resolve type symbols.
  public var aliasedDecl: GenericTypeDecl? {
    switch realizeAliasedType() {
    case let type as NominalType:
      return type.decl
    case let type as BoundGenericType:
      return type.decl

    default:
      return nil
    }
  }

  /// The uncontextualized type of a reference to `self` within the context of this type.
  ///
  /// If the type declaration introduces its own generic type parameters, this wraps `instanceType`
  /// within a `BoundGenericType` where each parameter is bound to itself. Otherwise, this is the
  /// same as `instanceType`.
  public override var receiverType: ValType {
    if let clause = genericClause {
      // The instance type is generic (e.g., `Foo<Bar>`).
      return type.context.boundGenericType(
        decl: self,
        args: clause.params.map({ $0.instanceType }))
    } else {
      return instanceType
    }
  }

  /// Realizes the semantic type of the declaration.
  public func realize() -> KindType {
    guard state < .realized else { return type as! KindType }
    setState(.realizationRequested)

    // Realize the aliased signature.
    let context = type.context
    let aliasedType = aliasedSign.realize(unqualifiedFrom: self)
    setState(.realized)
    type = context.aliasType(decl: self).kind

    guard !aliasedType.hasErrors else {
      setState(.invalid)
      return type as! KindType
    }

    // Complain if the signature references the declaration itself.
    // FIXME: Detect circular alias declarations.

    // Complain if the aliased signature describes an in-out type.
    if aliasedType is InoutType {
      context.report(.invalidMutatingTypeModifier(range: aliasedSign.range))
      setState(.invalid)
      return type as! KindType
    }

    // If the declaration denotes a "true" alias, complain if it declares additional conformances.
    // These should be expressed with an extension of the aliased type.
    if !inheritances.isEmpty && (aliasedType is NominalType) {
      context.report(.newConformanceOnNominalTypeAlias(range: inheritances[0].range))
    }

    return type as! KindType
  }

  /// Realizes the aliased type expression.
  public func realizeAliasedType() -> ValType {
    _ = realize()
    return aliasedSign.type
  }

  public override func lookup(qualified name: String) -> LookupResult {
    // Name lookups require the declaration to be realized, so that we can search into possible
    // extensions. However, realizing the aliased signature will trigger name lookups into this
    // declaration, causing infinite recursion. That's because the signature's identifiers may
    // refer to generic parameters. To break the cycle, we should restrict the search space to
    // to the generic clause until the declaration is fully realized. This is fine, because the
    // types referred by the aliased signature can't be declared within the aliased type.
    if state == .realizationRequested {
      guard let param = genericClause?.params.first(where: { $0.name == name }) else {
        return LookupResult()
      }
      return LookupResult(types: [param], values: [])
    }

    // Realize the aliased signature.
    _ = realizeAliasedType()
    guard state >= .realized else { return LookupResult() }

    // Search within the declaration and its conformances.
    var results = super.lookup(qualified: name)

    // Complete the results with the members of the declared type.
    let aliasedResults = aliasedSign.type.lookup(member: name)
    results.append(contentsOf: aliasedResults)

    return results
  }

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a generic parameter.
public class GenericParamDecl: TypeDecl {

  public final var range: SourceRange?

  public final var parentDeclSpace: DeclSpace?

  public final var state = DeclState.parsed

  public final var type: ValType

  public final var name: String

  public init(name: String, type: ValType) {
    self.name = name
    self.type = type
  }

  public final var isOverloadable: Bool { false }

  public final func setState(_ newState: DeclState) {
    assert(newState >= state)
    state = newState
  }

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of an abstract type in a view.
public final class AbstractTypeDecl: GenericParamDecl {

  /// The views to which the abstract type conforms.
  public var inheritances: [Sign] = []

  /// The type requirements associated with the abstract type.
  ///
  /// Type requirements are not scoped by the declaration. They apply globally on all abstract
  /// types declared within the view declaration.
  public var typeReqs: [TypeReq] = []

  public override func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}

/// A type extension declaration.
///
/// This is not a `TypeDecl`, as it does not define any type. An extension merely represents a set
/// of declarations that should be "added" to a type.
public final class TypeExtnDecl: Decl, DeclSpace {

  public var range: SourceRange?

  /// The identifier of the type being extended.
  public var extendedIdent: IdentSign

  /// The member declarations of the type.
  public var members: [Decl]

  public init(extendedIdent: IdentSign, members: [Decl]) {
    self.extendedIdent = extendedIdent
    self.members = members
  }

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
  public lazy var extendedDecl: GenericTypeDecl? = computeExtendedDecl()

  /// Computes the declaration that is extended by this extension.
  public func computeExtendedDecl() -> GenericTypeDecl? {
    guard state != .invalid else { return nil }

    let type = extendedIdent.realize(unqualifiedFrom: parentDeclSpace!)
    guard !type.isError else {
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

  public func accept<V>(_ visitor: inout V) -> V.DeclResult where V: DeclVisitor {
    return visitor.visit(self)
  }

}
