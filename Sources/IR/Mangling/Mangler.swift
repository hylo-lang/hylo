import FrontEnd
import Utils

/// Hylo's mangling algorithm.
struct Mangler {

  /// The type of the stream to which data is written.
  typealias Output = String

  /// The identity of a mangled symbol.
  private enum Symbol: Hashable {

    /// A declaration or lexical scope.
    case node(AnyNodeID)

    /// A canonical type.
    case type(AnyType)

  }

  /// The program defining the symbols being defined.
  private let program: TypedProgram

  /// A table mapping mangled strings to their position in the string lookup table.
  private var stringPosition: [String: Int] = [:]

  /// A table mapping mangled symbols to their position in the symbol lookup table.
  private var symbolPosition: [Symbol: Int] = [:]

  /// The innermost scope being mangled, if any.
  private var qualification: AnyScopeID?

  /// A table mapping known symbols to their reserved mangled identifier.
  private var reserved: [Symbol: ReservedSymbol] = [:]

  /// Creates an instance mangling symbols defined in `programs`.
  init(_ program: TypedProgram) {
    self.program = program
    initializeReservedSymbols()
  }

  /// Initializes the table of reserved symbols.
  private mutating func initializeReservedSymbols() {
    reserved[.type(.any)] = .any
    reserved[.type(.never)] = .never
    reserved[.type(.void)] = .void
    if program.ast.coreModuleIsLoaded {
      reserved[.node(AnyNodeID(program.ast.coreLibrary!))] = .hylo
      registerReservedCoreType("Bool", as: .bool)
      registerReservedCoreType("Int", as: .int)
      registerReservedCoreType("Float64", as: .float64)
      registerReservedCoreType("String", as: .string)
    }
  }

  /// Extends `self.reserved` to associate the core type named `n` to the reserved symbol `s`.
  private mutating func registerReservedCoreType(_ n: String, as s: ReservedSymbol) {
    reserved[.node(AnyNodeID(program.ast.coreType(n)!.decl))] = s
  }

  /// Returns the mangled representation of `s`.
  mutating func mangled(type s: AnyType) -> Output {
    mangled(s, manglingWith: { (me, s, o) in me.append(type: s, to: &o) })
  }

  /// Returns the mangled representation of `s`.
  mutating func mangled<T: DeclID>(decl s: T) -> Output {
    mangled(s, manglingWith: { (me, s, o) in me.append(decl: s, to: &o) })
  }

  /// Returns the mangled representation of `s`
  mutating func mangled(function s: Function.ID) -> Output {
    mangled(s, manglingWith: { (me, s, o) in me.append(function: s, to: &o) })
  }

  /// Returns the mangled representation of `s`
  mutating func mangled(table s: WitnessTable) -> Output {
    mangled(s, manglingWith: { (me, s, o) in me.append(table: s, to: &o) })
  }

  /// Returns the mangled representation of `s`, calling `mangle` to compute it.
  private mutating func mangled<T>(
    _ s: T, manglingWith mangle: (inout Self, T, inout Output) -> Void
  ) -> Output {
    var c = stringPosition
    var p = symbolPosition
    var q = qualification
    defer {
      swap(&c, &stringPosition)
      swap(&p, &symbolPosition)
      swap(&q, &qualification)
    }

    var output = ""
    mangle(&self, s, &output)
    return output
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append<T: DeclID>(decl d: T, to output: inout Output) {
    let s = Symbol.node(.init(d))
    if appendIf(reservedOrRecorded: s, to: &output) { return }

    appendQualification(of: d, to: &output)
    if let s = AnyScopeID(d) {
      append(scope: s, to: &output)
      return
    }

    switch d.kind {
    case AssociatedTypeDecl.self:
      append(entity: AssociatedTypeDecl.ID(d)!, to: &output)
    case AssociatedValueDecl.self:
      append(entity: AssociatedValueDecl.ID(d)!, to: &output)
    case BindingDecl.self:
      append(entity: BindingDecl.ID(d)!, to: &output)
    case ImportDecl.self:
      append(entity: ImportDecl.ID(d)!, to: &output)
    case GenericParameterDecl.self:
      append(entity: GenericParameterDecl.ID(d)!, to: &output)
    case ParameterDecl.self:
      append(entity: ParameterDecl.ID(d)!, to: &output)
    case VarDecl.self:
      append(entity: VarDecl.ID(d)!, to: &output)
    default:
      unexpected(d, in: program.ast)
    }

    symbolPosition[.node(AnyNodeID(d))] = symbolPosition.count
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append<T: SingleEntityDecl>(entity d: T.ID, to output: inout Output) {
    append(operator: .init(for: T.self), to: &output)
    append(string: program.ast[d].baseName, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(entity d: BindingDecl.ID, to output: inout Output) {
    append(operator: .bindingDecl, to: &output)
    append(items: program.ast.names(in: program[d].pattern), to: &output) { (me, n, o) in
      me.append(decl: me.program.ast[n.pattern].decl, to: &o)
    }
  }

  /// Writes the mangled qualification of `n` to `output`.
  private mutating func appendQualification<T: NodeIDProtocol>(of n: T, to output: inout Output) {
    // Modules have no qualification.
    if n.kind == ModuleDecl.self { return }

    // Find the prefix of the qualification that should be mangled as a reference.
    var qs: [AnyScopeID] = []
    for s in program.scopes(from: program[n].scope) {
      // Anonymous scopes corresponding to the body of a function aren't mangled.
      if let d = BraceStmt.ID(s), program.isCallableBody(d) {
        continue
      } else if appendIf(reservedOrRecorded: .node(.init(s)), to: &output) {
        break
      } else if s == qualification {
        append(operator: .lookupRelative, to: &output)
        break
      } else {
        qs.append(s)
      }
    }

    // Write the mangled representation of the qualification's suffix.
    for s in qs.reversed() {
      append(scope: s, to: &output)
    }
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(scope s: AnyScopeID, to output: inout Output) {
    let n = Symbol.node(.init(s))
    if appendIf(reservedOrRecorded: n, to: &output) { return }

    let q = qualification
    qualification = s
    switch s.kind {
    case BraceStmt.self:
      append(anonymous: s, to: &output)
    case ConditionalExpr.self:
      append(anonymous: s, to: &output)
    case ConditionalStmt.self:
      append(anonymous: s, to: &output)
    case ConformanceDecl.self:
      append(conformance: ConformanceDecl.ID(s)!, to: &output)
    case ExtensionDecl.self:
      append(extension: ExtensionDecl.ID(s)!, to: &output)
    case ForStmt.self:
      append(anonymous: s, to: &output)
    case FunctionDecl.self:
      append(function: FunctionDecl.ID(s)!, to: &output)
    case InitializerDecl.self:
      append(initializer: InitializerDecl.ID(s)!, to: &output)
    case MatchCase.self:
      append(anonymous: s, to: &output)
    case MethodDecl.self:
      append(methodDecl: MethodDecl.ID(s)!, to: &output)
    case MethodImpl.self:
      append(methodImpl: MethodImpl.ID(s)!, to: &output)
    case ModuleDecl.self:
      append(entity: ModuleDecl.ID(s)!, to: &output)
    case NamespaceDecl.self:
      append(entity: NamespaceDecl.ID(s)!, to: &output)
    case ProductTypeDecl.self:
      append(entity: ProductTypeDecl.ID(s)!, to: &output)
    case SubscriptDecl.self:
      append(subscriptDecl: SubscriptDecl.ID(s)!, to: &output)
    case SubscriptImpl.self:
      append(subscriptImpl: SubscriptImpl.ID(s)!, to: &output)
    case TraitDecl.self:
      append(entity: TraitDecl.ID(s)!, to: &output)
    case TranslationUnit.self:
      append(translationUnit: TranslationUnit.ID(s)!, to: &output)
    case TypeAliasDecl.self:
      append(entity: TypeAliasDecl.ID(s)!, to: &output)
    case WhileStmt.self:
      append(anonymous: s, to: &output)
    default:
      unexpected(s, in: program.ast)
    }
    qualification = q
    symbolPosition[n] = symbolPosition.count
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(anonymous d: AnyScopeID, to output: inout Output) {
    append(operator: .anonymousScope, to: &output)
    append(integer: Int(d.rawValue.bits), to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(conformance d: ConformanceDecl.ID, to output: inout Output) {
    append(operator: .conformanceDecl, to: &output)
    append(typeOf: d, to: &output)

    if let c = program.ast[d].whereClause {
      append(whereClause: c.value, to: &output)
    } else {
      append(operator: .endOfSequence, to: &output)
    }
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(extension d: ExtensionDecl.ID, to output: inout Output) {
    append(operator: .extensionDecl, to: &output)
    append(typeOf: d, to: &output)

    if let c = program.ast[d].whereClause {
      append(whereClause: c.value, to: &output)
    } else {
      append(operator: .endOfSequence, to: &output)
    }
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(function d: FunctionDecl.ID, to output: inout Output) {
    // If the function is anonymous, just encode a unique ID.
    guard let n = program.ast.name(of: d) else {
      append(anonymous: AnyScopeID(d), to: &output)
      return
    }

    if program.ast[d].isStatic {
      append(operator: .staticFunctionDecl, to: &output)
    } else {
      append(operator: .functionDecl, to: &output)
    }

    append(name: n, to: &output)
    append(integer: program.ast[d].genericParameters.count, to: &output)
    append(typeOf: d, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(initializer d: InitializerDecl.ID, to output: inout Output) {
    // There's at most one memberwise initializer per product type declaration.
    if program.ast[d].isMemberwise {
      append(operator: .memberwiseInitializerDecl, to: &output)
      return
    }

    // Other initializers are mangled like static member functions.
    append(operator: .staticFunctionDecl, to: &output)
    append(name: Name(stem: "init"), to: &output)
    append(integer: program.ast[d].genericParameters.count, to: &output)
    append(typeOf: d, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(methodDecl d: MethodDecl.ID, to output: inout Output) {
    append(operator: .methodDecl, to: &output)
    append(string: program.ast[d].identifier.value, to: &output)
    append(typeOf: d, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(methodImpl d: MethodImpl.ID, to output: inout Output) {
    append(operator: .methodImpl, to: &output)
    append(base64Digit: program.ast[d].introducer.value, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func append(subscriptDecl d: SubscriptDecl.ID, to output: inout Output) {
    if program.ast[d].isProperty {
      append(operator: .propertyDecl, to: &output)
      append(string: program.ast[d].identifier?.value ?? "", to: &output)
    } else {
      append(operator: .subscriptDecl, to: &output)
      append(string: program.ast[d].identifier?.value ?? "", to: &output)
      append(integer: program.ast[d].genericParameters.count, to: &output)
    }
    append(typeOf: d, to: &output)
  }

  /// Writes the mangled representation of `u` to `output`.
  private mutating func append(subscriptImpl d: SubscriptImpl.ID, to output: inout Output) {
    append(operator: .subscriptImpl, to: &output)
    append(base64Digit: program.ast[d].introducer.value, to: &output)
  }

  /// Writes the mangled representation of `u` to `output`.
  private mutating func append(translationUnit u: TranslationUnit.ID, to output: inout Output) {
    // Note: assumes all files in a module have a different base name.
    append(operator: .translatonUnit, to: &output)
    append(string: program.ast[u].site.file.baseName, to: &output)
  }

  /// Writes the mangled representation of `clause` to `output`.
  private mutating func append(whereClause clause: WhereClause, to output: inout Output) {
    append(operator: .whereClause, to: &output)
    append(unordered: clause.constraints, to: &output) { (me, c, o) in
      me.append(constraint: c.value, to: &o)
    }
  }

  /// Writes the mangled representation of `c` to `output`.
  private mutating func append(constraint c: WhereClause.ConstraintExpr, to output: inout Output) {
    switch c {
    case .value:
      UNIMPLEMENTED()

    case .bound(let lhs, let rhs):
      append(operator: .conformanceConstraint, to: &output)
      append(typeOf: lhs, to: &output)
      append(items: rhs, to: &output) { (me, r, o) in me.append(typeOf: r, to: &o) }

    case .equality(let lhs, let rhs):
      append(operator: .equalityConstraint, to: &output)
      append(typeOf: lhs, to: &output)
      append(typeOf: rhs, to: &output)
    }
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(function s: Function.ID, to output: inout Output) {
    switch s.value {
    case .lowered(let d):
      append(decl: d, to: &output)
    case .monomorphized(let f, let a):
      append(monomorphized: f, for: a, to: &output)
    case .synthesized(let d):
      append(synthesized: d, to: &output)
    case .existentialized:
      UNIMPLEMENTED()
    }
  }

  /// Writes the mangled representation of `s` monomorphized for `arguments` to `output`.
  private mutating func append(
    monomorphized s: Function.ID, for arguments: GenericArguments, to output: inout Output
  ) {
    append(operator: .monomorphizedFunctionDecl, to: &output)
    append(function: s, to: &output)
    append(specialization: arguments, to: &output)
  }

  /// Writes the mangled representation of `z` to `output`.
  private mutating func append(specialization z: GenericArguments, to output: inout Output) {
    append(items: z.sorted(by: \.key.rawValue), to: &output) { (me, a, o) in
      me.append(value: a.value, to: &o)
    }
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(synthesized s: SynthesizedFunctionDecl, to output: inout Output) {
    append(operator: .synthesizedFunctionDecl, to: &output)
    append(synthesizedKind: s.kind, to: &output)
    appendQualification(of: s.scope, to: &output)
    append(scope: s.scope, to: &output)
    append(operator: .endOfSequence, to: &output)
    append(type: ^s.type, to: &output)
  }

  /// Writes the mangled representation of `k` to `output`.
  private mutating func append(
    synthesizedKind k: SynthesizedFunctionDecl.Kind, to output: inout Output
  ) {
    switch k {
    case .deinitialize:
      append(base64Didit: 0, to: &output)
    case .moveInitialization:
      append(base64Didit: 1, to: &output)
    case .moveAssignment:
      append(base64Didit: 2, to: &output)
    case .copy:
      append(base64Didit: 3, to: &output)
    case .equal:
      append(base64Didit: 4, to: &output)
    case .globalInitialization(let d):
      append(base64Didit: 5, to: &output)
      append(entity: d, to: &output)
    case .autoclosure(let e):
      append(base64Didit: 6, to: &output)
      // To allow using multiple autoclosures in the same scope, also write the expression ID.
      append(integer: Int(e.rawValue.bits), to: &output)
    }
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(table s: WitnessTable, to output: inout Output) {
    append(operator: .witnessTable, to: &output)
    append(scope: s.scope, to: &output)
    append(type: program.canonical(s.witness, in: s.scope), to: &output)
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(value s: CompileTimeValue, to output: inout Output) {
    switch s {
    case .type(let t):
      append(type: t, to: &output)
    case .term(let t):
      append(term: t, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  mutating func append(term symbol: AnyTerm, to output: inout Output) {
    switch symbol.base {
    case let t as ConcreteTerm:
      let v = (t.value as? Int) ?? UNIMPLEMENTED()
      append(integer: v, to: &output)
    case let t as GenericTermParameter:
      append(entity: t.decl, to: &output)
    default:
      UNIMPLEMENTED()
    }
  }

  /// Writes the mangled representation of `d`'s type to `output`.
  private mutating func append<T: DeclID>(typeOf d: T, to output: inout Output) {
    append(type: program.canonical(typeOf: d), to: &output)
  }

  /// Writes the mangled representation of `e`'s type to `output`.
  private mutating func append<T: ExprID>(typeOf e: T, to output: inout Output) {
    append(type: program.canonical(typeOf: e), to: &output)
  }

  /// Writes the mangled representation of `s` to `output`.
  private mutating func append(type s: AnyType, to output: inout Output) {
    let n = Symbol.type(s)
    if appendIf(reservedOrRecorded: n, to: &output) { return }

    assert(s.isCanonical)
    switch s.base {
    case let t as ArrowType:
      append(arrow: t, to: &output)
    case let t as AssociatedTypeType:
      append(associatedType: t, to: &output)
    case let t as BoundGenericType:
      append(boundGenericType: t, to: &output)
    case let t as BufferType:
      append(buffer: t, to: &output)
    case let t as BuiltinType:
      append(builtin: t, to: &output)
    case let t as ExistentialType:
      append(existential: t, to: &output)
    case let t as GenericTypeParameterType:
      append(genericTypeParameter: t, to: &output)
    case let t as MethodType:
      append(method: t, to: &output)
    case let t as MetatypeType:
      append(metatype: t, to: &output)
    case let t as ParameterType:
      append(parameter: t, to: &output)
    case let t as ProductType:
      append(product: t, to: &output)
    case let t as RemoteType:
      append(remote: t, to: &output)
    case let t as SubscriptType:
      append(subscript: t, to: &output)
    case let t as TraitType:
      append(trait: t, to: &output)
    case let t as TupleType:
      append(tuple: t, to: &output)
    case let t as UnionType:
      append(union: t, to: &output)
    default:
      unreachable()
    }
    symbolPosition[n] = symbolPosition.count
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(arrow t: ArrowType, to output: inout Output) {
    append(operator: .arrowType, to: &output)
    append(type: t.environment, to: &output)
    append(items: t.inputs, to: &output) { (me, i, o) in
      me.append(string: i.label ?? "", to: &o)
      me.append(type: i.type, to: &o)
    }
    append(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(associatedType t: AssociatedTypeType, to output: inout Output) {
    append(operator: .associatedType, to: &output)
    append(decl: t.decl, to: &output)
    append(type: t.domain, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(boundGenericType t: BoundGenericType, to output: inout Output) {
    append(operator: .boundGenericType, to: &output)
    append(type: t.base, to: &output)
    append(items: t.arguments.values, to: &output) { (me, v, o) in
      me.append(value: v, to: &o)
    }
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(buffer t: BufferType, to output: inout Output) {
    append(operator: .bufferType, to: &output)
    append(type: t.element, to: &output)
    append(term: t.count, to: &output)
  }

  /// Writes the mangled representation of `z` to `output`.
  private mutating func append(builtin t: BuiltinType, to output: inout Output) {
    switch t {
    case .i(let width):
      append(operator: .builtinIntegerType, to: &output)
      append(integer: width, to: &output)
    case .word:
      append(operator: .builtinWordType, to: &output)
    case .float16:
      append(operator: .builtinFloatType, to: &output)
      append(integer: 16, to: &output)
    case .float32:
      append(operator: .builtinFloatType, to: &output)
      append(integer: 32, to: &output)
    case .float64:
      append(operator: .builtinFloatType, to: &output)
      append(integer: 64, to: &output)
    case .float128:
      append(operator: .builtinFloatType, to: &output)
      append(integer: 128, to: &output)
    case .ptr:
      append(operator: .builtinPointerType, to: &output)
    case .module:
      append(operator: .builtinModuleType, to: &output)
    }
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(existential t: ExistentialType, to output: inout Output) {
    switch t.interface {
    case .metatype:
      append(operator: .existentialMetatype, to: &output)

    case .generic(let interface):
      append(operator: .existentialGenericType, to: &output)
      append(type: interface, to: &output)

    case .traits(let interface):
      append(operator: .existentialTraitType, to: &output)
      append(unordered: interface, to: &output) { (me, e, o) in
        me.append(type: ^e, to: &o)
      }
    }
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(
    genericTypeParameter t: GenericTypeParameterType, to output: inout Output
  ) {
    append(operator: .genericTypeParameterType, to: &output)
    append(decl: AnyDeclID(t.decl), to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(method t: MethodType, to output: inout Output) {
    append(operator: .methodType, to: &output)
    append(base64Digit: t.capabilities, to: &output)
    append(type: t.receiver, to: &output)
    append(items: t.inputs, to: &output) { (me, i, o) in
      me.append(string: i.label ?? "", to: &o)
      me.append(type: i.type, to: &o)
    }
    append(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(metatype t: MetatypeType, to output: inout Output) {
    append(operator: .metatypeType, to: &output)
    append(type: t.instance, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(parameter t: ParameterType, to output: inout Output) {
    append(operator: .parameterType, to: &output)
    append(base64Digit: t.access, to: &output)
    append(type: t.bareType, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(product t: ProductType, to output: inout Output) {
    append(operator: .productType, to: &output)
    append(decl: AnyDeclID(t.decl), to: &output)
    append(operator: .endOfSequence, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(remote t: RemoteType, to output: inout Output) {
    append(operator: .remoteType, to: &output)
    append(base64Digit: t.access, to: &output)
    append(type: t.bareType, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(subscript t: SubscriptType, to output: inout Output) {
    append(operator: .subscriptType, to: &output)
    append(base64Digit: t.capabilities, to: &output)
    append(type: t.environment, to: &output)
    append(items: t.inputs, to: &output) { (me, i, o) in
      me.append(string: i.label ?? "", to: &o)
      me.append(type: i.type, to: &o)
    }
    append(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(tuple t: TupleType, to output: inout Output) {
    append(operator: .tupleType, to: &output)
    append(items: t.elements, to: &output) { (me, e, o) in
      me.append(string: e.label ?? "", to: &o)
      me.append(type: e.type, to: &o)
    }
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(trait t: TraitType, to output: inout Output) {
    append(operator: .traitType, to: &output)
    append(decl: AnyDeclID(t.decl), to: &output)
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func append(union t: UnionType, to output: inout Output) {
    append(operator: .unionType, to: &output)
    append(unordered: t.elements, to: &output) { (me, e, o) in
      me.append(type: e, to: &o)
    }
  }

  /// Writes the mangled representation of `name` to `output`.
  private mutating func append(name: Name, to output: inout Output) {
    // Only encode notation and introducer; labels are encoded in types.
    var tag: UInt8 = 0
    if name.notation != nil { tag = 1 }
    if name.introducer != nil { tag = tag | 2 }

    append(base64Didit: tag, to: &output)
    if let n = name.notation {
      append(base64Digit: n, to: &output)
    }
    if let i = name.introducer {
      append(base64Digit: i, to: &output)
    }
    append(string: name.stem, to: &output)
  }

  /// Writes `string` to `output`, prefixed by its length encoded as a variable-length integer.
  private mutating func append<T: StringProtocol>(string: T, to output: inout Output) {
    let s = String(string)

    if s.isEmpty {
      append(integer: 0, to: &output)
    } else if let n = stringPosition[s] {
      append(integer: 1, to: &output)
      append(integer: n, to: &output)
    } else {
      append(integer: s.count + 2, to: &output)
      string.write(to: &output)
      stringPosition[s] = stringPosition.count
    }
  }

  /// Writes `v` encoded as a variable-length integer to `output`.
  private func append(integer v: Int, to output: inout Output) {
    Base64VarUInt(v).write(to: &output)
  }

  /// Writes the raw value of `v` encoded as a base 64 digit to `output`.
  private func append<T: RawRepresentable<UInt8>>(base64Digit v: T, to output: inout Output) {
    append(base64Didit: v.rawValue, to: &output)
  }

  /// Writes `v` encoded as a base 64 digit to `output`.
  private func append(base64Didit v: UInt8, to output: inout Output) {
    Base64Digit(rawValue: v)!.description.write(to: &output)
  }

  /// Writes `o` to `output`.
  private func append(operator o: ManglingOperator, to output: inout Output) {
    o.write(to: &output)
  }

  /// Writes the mangled representation of `items`, calling `appendItem` to mangle each individual
  /// element to `output`.
  private mutating func append<T: Collection>(
    items: T, to output: inout Output,
    appendingEachWith appendItem: (inout Self, T.Element, inout Output) -> Void
  ) {
    append(integer: items.count, to: &output)
    for i in items {
      appendItem(&self, i, &output)
    }
  }

  /// Writes the mangled representation of `items`, which is an unordered set, calling `mangleItem`
  /// to mangle each individual element.
  private func append<T: Collection>(
    unordered items: T, to output: inout Output,
    manglingElementsWith mangleItem: (inout Self, T.Element, inout Output) -> Void
  ) {
    append(integer: items.count, to: &output)
    var mangled: [String] = []
    for e in items {
      // Copy `self` to share the symbol looking table built so far.
      var m = self
      var s = ""
      mangleItem(&m, e, &s)
      let i = mangled.partitioningIndex(where: { s < $0 })
      mangled.insert(s, at: i)
    }
    mangled.joined().write(to: &output)
  }

  /// If `s` is reserved or has already been inserted in the symbol lookup table, writes a lookup
  /// reference to it and returns `true`. Otherwise, returns `false`.
  private func appendIf(reservedOrRecorded s: Symbol, to output: inout Output) -> Bool {
    appendIf(reserved: s, to: &output) || appendIf(recorded: s, to: &output)
  }

  /// Writes a lookup reference to `s` and returns `true` iff `s` is a reserved symbol. Otherwise,
  /// returns `false` without modifying `output`.
  private func appendIf(reserved s: Symbol, to output: inout Output) -> Bool {
    if let r = reserved[s] {
      append(operator: .reserved, to: &output)
      r.write(to: &output)
      return true
    } else {
      return false
    }
  }

  /// Writes a lookup reference to `s` and returns `true` iff `s` in the lookup table. Otherwise,
  /// returns `false` without modifying `output`.
  private func appendIf(recorded s: Symbol, to output: inout Output) -> Bool {
    if let p = symbolPosition[s] {
      append(operator: .lookup, to: &output)
      append(integer: p, to: &output)
      return true
    } else {
      return false
    }
  }

}
