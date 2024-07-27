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

  /// The scope in which names are mangled.
  private let scopeOfUse: AnyScopeID

  /// A table mapping mangled symbols to their position in the symbol lookup table.
  private var symbolID: [Symbol: Int] = [:]

  /// The ID of the next symbol inserted in the symbol lookup table.
  private var nextSymbolID = 0

  /// A table mapping known symbols to their reserved mangled identifier.
  private var reserved: [Symbol: ReservedSymbol] = [
    .type(.any): .any,
    .type(.void): .void,
    .type(.never): .never,
  ]

  /// Creates an instance mangling symbols defined in `programs` in `scopeOfUse`.
  init(_ program: TypedProgram, in scopeOfUse: AnyScopeID) {
    self.program = program
    self.scopeOfUse = scopeOfUse

    if program.ast.coreModuleIsLoaded {
      self.reserved[.node(AnyNodeID(program.ast.coreLibrary!))] = .hylo
      register(coreType: "Bool", as: .bool)
      register(coreType: "Int", as: .int)
      register(coreType: "Float64", as: .float64)
      register(coreType: "String", as: .string)
    }
  }

  /// Associates `coreType` to `r`.
  private mutating func register(coreType: String, as r: ReservedSymbol) {
    let d = AnyNodeID(program.ast.coreType(coreType)!.decl)
    reserved[.node(d)] = r
  }

  /// Writes the mangled representation of `d` to `output`.
  mutating func mangle<T: DeclID>(decl d: T, to output: inout Output) {
    if let m = ModuleDecl.ID(d) {
      write(scope: AnyScopeID(m), to: &output)
      return
    }

    if writeLookup(.node(AnyNodeID(d)), to: &output) {
      return
    }

    writeQualification(of: d, to: &output)

    if let s = AnyScopeID(d) {
      write(scope: s, to: &output)
      return
    }

    switch d.kind {
    case AssociatedTypeDecl.self:
      write(entity: AssociatedTypeDecl.ID(d)!, to: &output)
    case AssociatedValueDecl.self:
      write(entity: AssociatedValueDecl.ID(d)!, to: &output)
    case BindingDecl.self:
      write(entity: BindingDecl.ID(d)!, to: &output)
    case ImportDecl.self:
      write(entity: ImportDecl.ID(d)!, to: &output)
    case GenericParameterDecl.self:
      write(entity: GenericParameterDecl.ID(d)!, to: &output)
    case ParameterDecl.self:
      write(entity: ParameterDecl.ID(d)!, to: &output)
    case VarDecl.self:
      write(entity: VarDecl.ID(d)!, to: &output)
    default:
      unexpected(d, in: program.ast)
    }

    symbolID[.node(AnyNodeID(d))] = nextSymbolID
    nextSymbolID += 1
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(entity d: BindingDecl.ID, to output: inout Output) {
    let n = program.ast.names(in: program[d].pattern).first!
    let v = program.ast[n.pattern].decl
    mangle(decl: v, to: &output)
  }

  /// Writes the mangled qualification of `n` to `output`.
  private mutating func writeQualification<T: NodeIDProtocol>(of n: T, to output: inout Output) {
    var qualification: [AnyScopeID] = []
    for s in program.scopes(from: program[n].scope) {
      if writeLookup(.node(AnyNodeID(s)), to: &output) {
        break
      } else {
        qualification.append(s)
      }
    }

    for s in qualification.reversed() {
      // Anonymous scopes corresponding to the body of a function aren't mangled.
      if let p = BraceStmt.ID(s), program.isCallableBody(p) {
        continue
      }
      write(scope: s, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(scope symbol: AnyScopeID, to output: inout Output) {
    if writeLookup(.node(AnyNodeID(symbol)), to: &output) {
      return
    }

    symbolID[.node(AnyNodeID(symbol))] = nextSymbolID
    nextSymbolID += 1

    switch symbol.kind {
    case BraceStmt.self:
      write(anonymousScope: symbol, to: &output)
    case ConditionalExpr.self:
      write(anonymousScope: symbol, to: &output)
    case ConditionalStmt.self:
      write(anonymousScope: symbol, to: &output)
    case ConformanceDecl.self:
      write(conformance: ConformanceDecl.ID(symbol)!, to: &output)
    case ExtensionDecl.self:
      write(extension: ExtensionDecl.ID(symbol)!, to: &output)
    case ForStmt.self:
      write(anonymousScope: symbol, to: &output)
    case FunctionDecl.self:
      write(function: FunctionDecl.ID(symbol)!, to: &output)
    case InitializerDecl.self:
      write(initializer: InitializerDecl.ID(symbol)!, to: &output)
    case MatchCase.self:
      write(anonymousScope: symbol, to: &output)
    case MethodDecl.self:
      write(methodDecl: MethodDecl.ID(symbol)!, to: &output)
    case MethodImpl.self:
      write(methodImpl: MethodImpl.ID(symbol)!, to: &output)
    case ModuleDecl.self:
      write(entity: ModuleDecl.ID(symbol)!, to: &output)
    case NamespaceDecl.self:
      write(entity: NamespaceDecl.ID(symbol)!, to: &output)
    case ProductTypeDecl.self:
      write(entity: ProductTypeDecl.ID(symbol)!, to: &output)
    case SubscriptDecl.self:
      write(subscriptDecl: SubscriptDecl.ID(symbol)!, to: &output)
    case SubscriptImpl.self:
      write(subscriptImpl: SubscriptImpl.ID(symbol)!, to: &output)
    case TraitDecl.self:
      write(entity: TraitDecl.ID(symbol)!, to: &output)
    case TranslationUnit.self:
      write(translationUnit: TranslationUnit.ID(symbol)!, to: &output)
    case TypeAliasDecl.self:
      write(entity: TypeAliasDecl.ID(symbol)!, to: &output)
    case WhileStmt.self:
      write(anonymousScope: symbol, to: &output)
    default:
      unexpected(symbol, in: program.ast)
    }
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write<T: SingleEntityDecl>(entity d: T.ID, to output: inout Output) {
    write(operator: .init(for: T.self), to: &output)
    write(string: program.ast[d].baseName, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(anonymousScope d: AnyScopeID, to output: inout Output) {
    write(operator: .anonymousScope, to: &output)
    write(integer: Int(d.rawValue.bits), to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(conformance d: ConformanceDecl.ID, to output: inout Output) {
    write(operator: .conformanceDecl, to: &output)
    mangle(type: program[d].type, to: &output)

    if let c = program.ast[d].whereClause {
      write(whereClause: c.value, to: &output)
    } else {
      write(operator: .endOfSequence, to: &output)
    }
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(extension d: ExtensionDecl.ID, to output: inout Output) {
    write(operator: .extensionDecl, to: &output)
    mangle(type: program[d].type, to: &output)

    if let c = program.ast[d].whereClause {
      write(whereClause: c.value, to: &output)
    } else {
      write(operator: .endOfSequence, to: &output)
    }
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(function d: FunctionDecl.ID, to output: inout Output) {
    // If the function is anonymous, just encode a unique ID.
    guard let n = program.ast.name(of: d) else {
      write(anonymousScope: AnyScopeID(d), to: &output)
      return
    }

    if program.ast[d].isStatic {
      write(operator: .staticFunctionDecl, to: &output)
    } else {
      write(operator: .functionDecl, to: &output)
    }

    write(name: n, to: &output)
    write(integer: program.ast[d].genericParameters.count, to: &output)
    mangle(type: program[d].type, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(initializer d: InitializerDecl.ID, to output: inout Output) {
    // There's at most one memberwise initializer per product type declaration.
    if program.ast[d].isMemberwise {
      write(operator: .memberwiseInitializerDecl, to: &output)
      return
    }

    // Other initializers are mangled like static member functions.
    write(operator: .staticFunctionDecl, to: &output)
    write(name: Name(stem: "init"), to: &output)
    write(integer: program.ast[d].genericParameters.count, to: &output)
    mangle(type: program[d].type, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(methodDecl d: MethodDecl.ID, to output: inout Output) {
    write(operator: .methodDecl, to: &output)
    write(string: program.ast[d].identifier.value, to: &output)
    mangle(type: program[d].type, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(methodImpl d: MethodImpl.ID, to output: inout Output) {
    write(operator: .methodImpl, to: &output)
    write(base64Digit: program.ast[d].introducer.value, to: &output)
  }

  /// Writes the mangled representation of `d` to `output`.
  private mutating func write(subscriptDecl d: SubscriptDecl.ID, to output: inout Output) {
    if program.ast[d].isProperty {
      write(operator: .propertyDecl, to: &output)
      write(string: program.ast[d].identifier?.value ?? "", to: &output)
    } else {
      write(operator: .subscriptDecl, to: &output)
      write(string: program.ast[d].identifier?.value ?? "", to: &output)
      write(integer: program.ast[d].genericParameters.count, to: &output)
    }

    mangle(type: program[d].type, to: &output)
  }

  /// Writes the mangled representation of `u` to `output`.
  private mutating func write(subscriptImpl d: SubscriptImpl.ID, to output: inout Output) {
    write(operator: .subscriptImpl, to: &output)
    write(base64Digit: program.ast[d].introducer.value, to: &output)
  }

  /// Writes the mangled representation of `u` to `output`.
  private mutating func write(translationUnit u: TranslationUnit.ID, to output: inout Output) {
    // Note: assumes all files in a module have a different base name.
    write(operator: .translatonUnit, to: &output)
    write(string: program.ast[u].site.file.baseName, to: &output)
  }

  /// Writes the mangled representation of `clause` to `output`.
  private mutating func write(whereClause clause: WhereClause, to output: inout Output) {
    write(operator: .whereClause, to: &output)
    write(set: clause.constraints, to: &output) { (m, c) -> String in
      var s = ""
      m.write(constraint: c.value, to: &s)
      return s
    }
  }

  /// Writes the mangled representation of `c` to `output`.
  private mutating func write(constraint c: WhereClause.ConstraintExpr, to output: inout Output) {
    switch c {
    case .value:
      UNIMPLEMENTED()

    case .bound(let lhs, let rhs):
      write(operator: .conformanceConstraint, to: &output)
      mangle(type: program[lhs].type, to: &output)
      write(integer: rhs.count, to: &output)
      for t in rhs {
        mangle(type: program[t].type, to: &output)
      }

    case .equality(let lhs, let rhs):
      write(operator: .equalityConstraint, to: &output)
      mangle(type: program[lhs].type, to: &output)
      mangle(type: program[rhs].type, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  mutating func mangle(function symbol: Function.ID, to output: inout Output) {
    switch symbol.value {
    case .lowered(let d):
      mangle(decl: d, to: &output)
    case .monomorphized(let f, let a):
      write(monomorphized: f, for: a, to: &output)
    case .synthesized(let d):
      write(synthesized: d, to: &output)
    case .existentialized:
      UNIMPLEMENTED()
    }
  }

  /// Writes the mangled representation of `symbol` monomorphized for `arguments` to `output`.
  private mutating func write(
    monomorphized symbol: Function.ID, for arguments: GenericArguments, to output: inout Output
  ) {
    write(operator: .monomorphizedFunctionDecl, to: &output)
    mangle(function: symbol, to: &output)
    write(specialization: arguments, to: &output)
  }

  /// Writes the mangled representation of `specialization` to `output`.
  private mutating func write(specialization: GenericArguments, to output: inout Output) {
    write(integer: specialization.count, to: &output)
    for (_, v) in specialization.sorted(by: \.key.rawValue) {
      mangle(value: v, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(
    synthesized symbol: SynthesizedFunctionDecl, to output: inout Output
  ) {
    write(operator: .synthesizedFunctionDecl, to: &output)
    write(synthesizedKind: symbol.kind, to: &output)

    if symbol.scope.kind != ModuleDecl.self {
      writeQualification(of: symbol.scope, to: &output)
    }
    write(scope: symbol.scope, to: &output)
    mangle(type: ^symbol.type, to: &output)
  }

  /// Writes the mangled representation of `k` to `output`.
  private mutating func write(
    synthesizedKind k: SynthesizedFunctionDecl.Kind, to output: inout Output
  ) {
    switch k {
    case .deinitialize:
      write(base64Didit: 0, to: &output)
    case .moveInitialization:
      write(base64Didit: 1, to: &output)
    case .moveAssignment:
      write(base64Didit: 2, to: &output)
    case .copy:
      write(base64Didit: 3, to: &output)
    case .globalInitialization(let d):
      write(base64Didit: 4, to: &output)
      write(entity: d, to: &output)
    case .autoclosure(let e):
      write(base64Didit: 5, to: &output)
      // To allow using multiple autoclosures in the same scope, also write the expression ID.
      write(integer: Int(e.rawValue.bits), to: &output)
    }
  }

  /// Writes the mangled representation of `r` to `output`.
  mutating func mangle(reference r: DeclReference, to output: inout Output) {
    switch r {
    case .direct(let d, let z):
      write(operator: .directDeclReference, to: &output)
      mangle(decl: d, to: &output)
      write(specialization: z, to: &output)

    default:
      UNIMPLEMENTED()
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  mutating func mangle(table symbol: WitnessTable, to output: inout Output) {
    write(operator: .witnessTable, to: &output)
    write(scope: symbol.scope, to: &output)
    mangle(type: symbol.witness, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  mutating func mangle(value symbol: CompileTimeValue, to output: inout Output) {
    switch symbol {
    case .type(let t):
      mangle(type: t, to: &output)
    case .compilerKnown(let v) where v is Int:
      write(integer: v as! Int, to: &output)
    default:
      UNIMPLEMENTED()
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  mutating func mangle(type symbol: AnyType, to output: inout Output) {
    let s = program.canonical(symbol, in: scopeOfUse)

    if writeLookup(.type(s), to: &output) {
      return
    }

    switch s.base {
    case let t as AssociatedTypeType:
      write(associatedType: t, to: &output)

    case let t as BoundGenericType:
      write(boundGenericType: t, to: &output)

    case let t as BufferType:
      write(bufferType: t, to: &output)

    case let t as BuiltinType:
      write(builtinType: t, to: &output)

    case let t as ExistentialType:
      write(existentialType: t, to: &output)

    case let t as GenericTypeParameterType:
      write(operator: .genericTypeParameterType, to: &output)
      mangle(decl: AnyDeclID(t.decl), to: &output)

    case let t as ArrowType:
      write(arrow: t, to: &output)

    case let t as MethodType:
      write(method: t, to: &output)

    case let t as MetatypeType:
      write(operator: .metatypeType, to: &output)
      mangle(type: t.instance, to: &output)

    case let t as ParameterType:
      write(operator: .parameterType, to: &output)
      write(base64Digit: t.access, to: &output)
      mangle(type: t.bareType, to: &output)

    case let t as ProductType:
      write(operator: .productType, to: &output)
      mangle(decl: AnyDeclID(t.decl), to: &output)

      // End of sequence required because `t.decl` is a scope.
      write(operator: .endOfSequence, to: &output)

    case let t as RemoteType:
      write(operator: .remoteType, to: &output)
      write(base64Digit: t.access, to: &output)
      mangle(type: t.bareType, to: &output)

    case let t as SubscriptType:
      write(subscriptType: t, to: &output)

    case let t as UnionType:
      write(unionType: t, to: &output)

    case let t as TraitType:
      write(operator: .traitType, to: &output)
      mangle(decl: AnyDeclID(t.decl), to: &output)

      // End of sequence required because `t.decl` is a scope.
      write(operator: .endOfSequence, to: &output)

    case let t as TupleType:
      write(tupleType: t, to: &output)

    default:
      unreachable()
    }

    symbolID[.type(s)] = nextSymbolID
    nextSymbolID += 1
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func write(associatedType t: AssociatedTypeType, to output: inout Output) {
    write(operator: .associatedType, to: &output)
    mangle(decl: t.decl, to: &output)
    mangle(type: t.domain, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(boundGenericType t: BoundGenericType, to output: inout Output) {
    write(operator: .boundGenericType, to: &output)
    mangle(type: t.base, to: &output)
    write(integer: t.arguments.count, to: &output)
    for u in t.arguments.values {
      mangle(value: u, to: &output)
    }
  }

  /// Writes the mangled representation of `t` to `output`.
  private mutating func write(bufferType t: BufferType, to output: inout Output) {
    write(operator: .bufferType, to: &output)
    mangle(type: t.element, to: &output)
    mangle(value: t.count, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(builtinType t: BuiltinType, to output: inout Output) {
    switch t {
    case .i(let width):
      write(operator: .builtinIntegerType, to: &output)
      write(integer: width, to: &output)
    case .word:
      write(operator: .builtinWordType, to: &output)
    case .float16:
      write(operator: .builtinFloatType, to: &output)
      write(integer: 16, to: &output)
    case .float32:
      write(operator: .builtinFloatType, to: &output)
      write(integer: 32, to: &output)
    case .float64:
      write(operator: .builtinFloatType, to: &output)
      write(integer: 64, to: &output)
    case .float128:
      write(operator: .builtinFloatType, to: &output)
      write(integer: 128, to: &output)
    case .ptr:
      write(operator: .builtinPointerType, to: &output)
    case .module:
      write(operator: .builtinModuleType, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(existentialType t: ExistentialType, to output: inout Output) {
    switch t.interface {
    case .metatype:
      write(operator: .existentialMetatype, to: &output)

    case .generic(let interface):
      write(operator: .existentialGenericType, to: &output)
      mangle(type: interface, to: &output)

    case .traits(let interface):
      write(operator: .existentialTraitType, to: &output)
      write(set: interface, to: &output) { (m, e) -> String in
        var s = ""
        m.mangle(type: ^e, to: &s)
        return s
      }
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(arrow t: ArrowType, to output: inout Output) {
    write(operator: .arrowType, to: &output)
    mangle(type: t.environment, to: &output)

    write(integer: t.inputs.count, to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(type: i.type, to: &output)
    }

    mangle(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(method t: MethodType, to output: inout Output) {
    write(operator: .methodType, to: &output)
    write(base64Digit: t.capabilities, to: &output)
    mangle(type: t.receiver, to: &output)

    write(integer: t.inputs.count, to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(type: i.type, to: &output)
    }

    mangle(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(subscriptType t: SubscriptType, to output: inout Output) {
    write(operator: .subscriptType, to: &output)
    write(base64Digit: t.capabilities, to: &output)
    mangle(type: t.environment, to: &output)

    write(integer: t.inputs.count, to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(type: i.type, to: &output)
    }

    mangle(type: t.output, to: &output)
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(tupleType t: TupleType, to output: inout Output) {
    write(operator: .tupleType, to: &output)

    write(integer: t.elements.count, to: &output)
    for e in t.elements {
      write(string: e.label ?? "", to: &output)
      mangle(type: e.type, to: &output)
    }
  }

  /// Writes the mangled representation of `symbol` to `output`.
  private mutating func write(unionType t: UnionType, to output: inout Output) {
    write(operator: .unionType, to: &output)
    write(set: t.elements, to: &output) { (m, e) -> String in
      var s = ""
      m.mangle(type: e, to: &s)
      return s
    }
  }

  /// If `symbol` is reserved or has already been inserted in the symbol lookup table, writes a
  /// lookup reference to it and returns `true`; returns `false` otherwise.
  private func writeLookup(_ symbol: Symbol, to output: inout Output) -> Bool {
    if let r = reserved[symbol] {
      write(operator: .reserved, to: &output)
      r.write(to: &output)
      return true
    }

    if let i = symbolID[symbol] {
      write(operator: .lookup, to: &output)
      write(integer: i, to: &output)
      return true
    }

    return false
  }

  /// Writes the mangled representation of `name` to `output`.
  private func write(name: Name, to output: inout Output) {
    // Only encode notation and introducer; labels are encoded in types.
    var tag: UInt8 = 0
    if name.notation != nil { tag = 1 }
    if name.introducer != nil { tag = tag | 2 }

    write(base64Didit: tag, to: &output)
    if let n = name.notation {
      write(base64Digit: n, to: &output)
    }
    if let i = name.introducer {
      write(base64Digit: i, to: &output)
    }
    write(string: name.stem, to: &output)
  }

  /// Writes `string` to `output`, prefixed by its length encoded as a variable-length integer.
  private func write<T: StringProtocol>(string: T, to output: inout Output) {
    write(integer: string.count, to: &output)
    string.write(to: &output)
  }

  /// Writes `v` encoded as a variable-length integer to `output`.
  private func write(integer v: Int, to output: inout Output) {
    Base64VarUInt(v).write(to: &output)
  }

  /// Writes the raw value of `v` encoded as a base 64 digit to `output`.
  private func write<T: RawRepresentable>(
    base64Digit v: T, to output: inout Output
  ) where T.RawValue == UInt8 {
    write(base64Didit: v.rawValue, to: &output)
  }

  /// Writes `v` encoded as a base 64 digit to `output`.
  private func write(base64Didit v: UInt8, to output: inout Output) {
    Base64Digit(rawValue: v)!.description.write(to: &output)
  }

  /// Writes `o` to `output`.
  private func write(operator o: ManglingOperator, to output: inout Output) {
    o.write(to: &output)
  }

  /// Writes the mangled representation of `elements`, which is an unordered set, calling
  /// `mangleElement` to mangle individual elements.
  private mutating func write<S: Collection>(
    set elements: S, to output: inout Output,
    manglingElementsWith mangleElement: (inout Self, S.Element) -> String
  ) {
    write(integer: elements.count, to: &output)

    var mangled: [String] = []
    for e in elements {
      // Copy `self` to share the symbol looking table built so far.
      var m = self
      let s = mangleElement(&m, e)
      let i = mangled.partitioningIndex(where: { s < $0 })
      mangled.insert(s, at: i)
    }

    mangled.joined().write(to: &output)
  }

}
