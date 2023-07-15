import Utils

/// Val's mangling algorithm.
struct Mangler {

  typealias Output = String

  /// The identity of a mangled symbol.
  private enum Symbol: Hashable {

    /// A declaration or lexical scope.
    case node(AnyNodeID)

    /// A canonical type.
    case type(AnyType)

  }

  /// A table mapping mangled symbols to their position in the symbol lookup table.
  private var symbolID: [Symbol: Int] = [:]

  /// The ID of the next symbol inserted in the symbol lookup table.
  private var nextSymbolID = 0

  mutating func mangle(_ d: AnyDeclID, of program: TypedProgram, to output: inout Output) {
    if writeLookup(.node(AnyNodeID(d)), to: &output) {
      return
    }

    if d.kind != ModuleDecl.self {
      writeQualification(of: d, of: program, to: &output)
    }

    if let s = AnyScopeID(d) {
      write(scope: s, of: program, to: &output)
      return
    }

    switch d.kind {
    case AssociatedTypeDecl.self:
      write(entity: AssociatedTypeDecl.ID(d)!, of: program, to: &output)
    case AssociatedValueDecl.self:
      write(entity: AssociatedValueDecl.ID(d)!, of: program, to: &output)
    case ImportDecl.self:
      write(entity: ImportDecl.ID(d)!, of: program, to: &output)
    case GenericParameterDecl.self:
      write(entity: GenericParameterDecl.ID(d)!, of: program, to: &output)
    case ParameterDecl.self:
      write(entity: ParameterDecl.ID(d)!, of: program, to: &output)
    case VarDecl.self:
      write(entity: VarDecl.ID(d)!, of: program, to: &output)
    default:
      unexpected(d, in: program.ast)
    }

    symbolID[.node(AnyNodeID(d))] = nextSymbolID
    nextSymbolID += 1
  }

  /// Writes the mangled the qualification of `d` to `output`.
  mutating func writeQualification(
    of d: AnyDeclID, of program: TypedProgram, to output: inout Output
  ) {
    // Anonymous scopes corresponding to the body of a function aren't mangled.
    var parent = program.nodeToScope[d]!
    if parent.kind == BraceStmt.self {
      let grandParant = program.nodeToScope[parent]!
      switch grandParant.kind {
      case FunctionDecl.self, InitializerDecl.self, MethodImpl.self, SubscriptImpl.self:
        parent = grandParant
      default:
        break
      }
    }

    for p in program.scopes(from: parent).reversed() {
      write(scope: p, of: program, to: &output)
    }
  }

  private mutating func write(
    scope symbol: AnyScopeID, of program: TypedProgram, to output: inout Output
  ) {
    if writeLookup(.node(AnyNodeID(symbol)), to: &output) {
      return
    }

    switch symbol.kind {
    case FunctionDecl.self:
      write(function: FunctionDecl.ID(symbol)!, of: program, to: &output)
    case InitializerDecl.self:
      write(initializer: InitializerDecl.ID(symbol)!, of: program, to: &output)
    case ModuleDecl.self:
      write(entity: ModuleDecl.ID(symbol)!, of: program, to: &output)
    case NamespaceDecl.self:
      write(entity: NamespaceDecl.ID(symbol)!, of: program, to: &output)
    case ProductTypeDecl.self:
      write(entity: ProductTypeDecl.ID(symbol)!, of: program, to: &output)
    case SubscriptDecl.self:
      write(subscriptDecl: SubscriptDecl.ID(symbol)!, of: program, to: &output)
    case SubscriptImpl.self:
      write(subscriptImpl: SubscriptImpl.ID(symbol)!, of: program, to: &output)
    case TraitDecl.self:
      write(entity: TraitDecl.ID(symbol)!, of: program, to: &output)
    case TranslationUnit.self:
      write(translationUnit: TranslationUnit.ID(symbol)!, of: program, to: &output)
    case TypeAliasDecl.self:
      write(entity: TypeAliasDecl.ID(symbol)!, of: program, to: &output)
    default:
      unexpected(symbol, in: program.ast)
    }

    symbolID[.node(AnyNodeID(symbol))] = nextSymbolID
    nextSymbolID += 1
  }

  private mutating func write<T: SingleEntityDecl>(
    entity d: T.ID, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: T.manglingOperator, to: &output)
    write(string: program.ast[d].baseName, to: &output)
  }

  private mutating func write(
    translationUnit u: TranslationUnit.ID, of program: TypedProgram, to output: inout Output
  ) {
    // Note: assumes all files in a module have a different base name.
    write(operator: .translatonUnit, to: &output)
    write(string: program.ast[u].site.file.baseName, to: &output)
  }

  private mutating func write(
    function d: FunctionDecl.ID, of program: TypedProgram, to output: inout Output
  ) {
    // If the function is anonymous, just encode a unique ID.
    guard let n = Name(of: d, in: program.ast) else {
      write(operator: .anonymousFunctionDecl, to: &output)
      write(integer: d.rawValue, to: &output)
      return
    }

    if program.ast[d].isStatic {
      write(operator: .staticFunctionDecl, to: &output)
    } else {
      write(operator: .functionDecl, to: &output)
    }

    write(name: n, to: &output)
    write(integer: program.ast[d].genericParameters.count, to: &output)
    mangle(program.declTypes[d]!, of: program, to: &output)
  }

  private mutating func write(
    initializer d: InitializerDecl.ID, of program: TypedProgram, to output: inout Output
  ) {
    // There's at most one memberwise initializer per product type declaration.
    if program.ast[d].isMemberwise {
      write(operator: .memberwiseInitializerDecl, to: &output)
      return
    }

    // Other initializers are mangled like static member functions.
    write(operator: .staticFunctionDecl, to: &output)
    write(name: Name(stem: "init"), to: &output)
    write(integer: program.ast[d].genericParameters.count, to: &output)
    mangle(program.declTypes[d]!, of: program, to: &output)
  }

  private mutating func write(
    subscriptDecl d: SubscriptDecl.ID, of program: TypedProgram, to output: inout Output
  ) {
    if program.ast[d].isProperty {
      write(operator: .propertyDecl, to: &output)
      write(string: program.ast[d].identifier?.value ?? "", to: &output)
    } else {
      write(operator: .subscriptDecl, to: &output)
      write(string: program.ast[d].identifier?.value ?? "", to: &output)
      write(integer: program.ast[d].genericParameters.count, to: &output)
    }

    mangle(program.declTypes[d]!, of: program, to: &output)
  }

  private mutating func write(
    subscriptImpl d: SubscriptImpl.ID, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: .subscriptImpl, to: &output)
    write(base64Didit: program.ast[d].introducer.value, to: &output)
  }

  private mutating func mangle(
    _ symbol: any CompileTimeValue, of program: TypedProgram, to output: inout Output
  ) {
    if let t = symbol as? AnyType {
      mangle(t, of: program, to: &output)
    } else {
      fatalError("not implemented")
    }
  }

  private mutating func mangle(
    _ symbol: AnyType, of program: TypedProgram, to output: inout Output
  ) {
    if writeLookup(.type(symbol), to: &output) {
      return
    }

    assert(symbol[.isCanonical])
    switch symbol.base {
    case .void:
      writeReserved(.void, to: &output)

    case .never:
      writeReserved(.never, to: &output)

    case let t as BoundGenericType:
      write(boundGenericType: t, of: program, to: &output)

    case let t as LambdaType:
      write(lambda: t, of: program, to: &output)

    case let t as ParameterType:
      write(operator: .parameterType, to: &output)
      write(base64Didit: t.access, to: &output)
      mangle(t.bareType, of: program, to: &output)

    case let t as ProductType:
      write(operator: .productType, to: &output)
      mangle(AnyDeclID(t.decl), of: program, to: &output)
      write(operator: .endOfSequence, to: &output)

    case let t as RemoteType:
      write(operator: .remoteType, to: &output)
      write(base64Didit: t.access, to: &output)
      mangle(t.bareType, of: program, to: &output)

    case let t as SubscriptType:
      write(subscriptType: t, of: program, to: &output)

    case let t as SumType:
      write(sumType: t, of: program, to: &output)

    case let t as TupleType:
      write(tupleType: t, of: program, to: &output)

    default:
      unreachable()
    }

    symbolID[.type(symbol)] = nextSymbolID
    nextSymbolID += 1
  }

  private mutating func write(
    boundGenericType t: BoundGenericType, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: .boundGenericType, to: &output)
    mangle(t.base, of: program, to: &output)
    write(integer: t.arguments.count, to: &output)
    for u in t.arguments.values {
      mangle(u, of: program, to: &output)
    }
  }

  private mutating func write(
    lambda t: LambdaType, of program: TypedProgram, to output: inout Output
  ) {
    if t.environment == .void {
      write(operator: .thinLambdaType, to: &output)
    } else {
      write(operator: .lambdaType, to: &output)
      mangle(t.environment, of: program, to: &output)
    }

    write(integer: t.inputs.count, to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(i.type, of: program, to: &output)
    }

    mangle(t.output, of: program, to: &output)
  }

  private mutating func write(
    subscriptType t: SubscriptType, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: .subscriptType, to: &output)
    write(base64Didit: t.capabilities, to: &output)
    mangle(t.environment, of: program, to: &output)

    write(integer: t.inputs.count, to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(i.type, of: program, to: &output)
    }

    mangle(t.output, of: program, to: &output)
  }

  private mutating func write(
    sumType t: SumType, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: .sumType, to: &output)
    write(integer: t.elements.count, to: &output)

    var elements: [String] = []
    for e in t.elements {
      // Copy `self` to share the symbol looking table built so far.
      var m = self
      var s = ""
      m.mangle(e, of: program, to: &s)
      let i = elements.partitioningIndex(where: { s < $0 })
      elements.insert(s, at: i)
    }

    elements.joined().write(to: &output)
  }

  private mutating func write(
    tupleType t: TupleType, of program: TypedProgram, to output: inout Output
  ) {
    write(operator: .tupleType, to: &output)

    write(integer: t.elements.count, to: &output)
    for e in t.elements {
      write(string: e.label ?? "", to: &output)
      mangle(e.type, of: program, to: &output)
    }
  }

  private func writeReserved(_ s: ReservedSymbol, to output: inout Output) {
    write(operator: .reserved, to: &output)
    s.write(to: &output)
  }

  /// If `s` has already been mangled into `output`, writes a lookup reference to its first
  /// occurrence and returns true. Otherwise, returns `false`.
  private func writeLookup(_ s: Symbol, to output: inout Output) -> Bool {
    guard let i = symbolID[s] else {
      return false
    }

    write(operator: .lookup, to: &output)
    write(integer: i, to: &output)
    return true
  }

  private func write(name: Name, to output: inout Output) {
    // Only encode notation and introducer; labels are encoded in types.
    var tag: UInt8 = 0
    if name.notation != nil { tag = 1 }
    if name.introducer != nil { tag = tag | 2 }

    write(base64Didit: tag, to: &output)
    if let n = name.notation {
      write(base64Didit: n, to: &output)
    }
    if let i = name.introducer {
      write(base64Didit: i, to: &output)
    }
    write(string: name.stem, to: &output)
  }

  private func write(string: String, to output: inout Output) {
    write(integer: string.count, to: &output)
    string.write(to: &output)
  }

  /// Writes `v` encoded as a variable-length integer to `output`.
  private func write(integer v: Int, to output: inout Output) {
    Base64VarUInt(v).write(to: &output)
  }

  /// Writes the raw value of `v` encoded as a base 64 digit to `output`.
  private func write<T: RawRepresentable>(
    base64Didit v: T, to output: inout Output
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

}
