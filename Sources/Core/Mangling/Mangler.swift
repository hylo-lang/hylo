import Utils

/// Val's mangling algorithm.
struct Mangler<Output: TextOutputStream> {

  /// The identity of a mangled symbol.
  private enum Symbol: Hashable {

    /// A declaration or lexical scope.
    case node(AnyNodeID)

    /// A canonical type.
    case type(AnyType)

  }

  /// A table mapping mangled symbols to their position in the demangling lookup table.
  private var symbolID: [Symbol: Int] = [:]

  /// The ID of the next symbol insertedin the demandling lookup table.
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
    case ParameterDecl.self:
      write(entity: ParameterDecl.ID(d)!, of: program, to: &output)
    default:
      unreachable()
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
    case ModuleDecl.self:
      write(entity: ModuleDecl.ID(symbol)!, of: program, to: &output)
    case TranslationUnit.self:
      write(translationUnit: TranslationUnit.ID(symbol)!, of: program, to: &output)
    case NamespaceDecl.self:
      write(entity: NamespaceDecl.ID(symbol)!, of: program, to: &output)
    case FunctionDecl.self:
      write(function: FunctionDecl.ID(symbol)!, of: program, to: &output)
    case ProductTypeDecl.self:
      write(entity: ProductTypeDecl.ID(symbol)!, of: program, to: &output)
    default:
      unreachable()
    }

    symbolID[.node(AnyNodeID(symbol))] = nextSymbolID
    nextSymbolID += 1
  }

  private mutating func write<T: SingleEntityDecl>(
    entity d: T.ID, of program: TypedProgram, to output: inout Output
  ) {
    T.manglingOperator.write(to: &output)
    write(string: program.ast[d].baseName, to: &output)
  }

  private mutating func write(
    translationUnit u: TranslationUnit.ID, of program: TypedProgram, to output: inout Output
  ) {
    // Note: assumes all files in a module have a different base name.
    ManglingOperator.translatonUnit.write(to: &output)
    write(string: program.ast[u].site.file.baseName, to: &output)
  }

  private mutating func write(
    function d: FunctionDecl.ID, of program: TypedProgram, to output: inout Output
  ) {
    // If the function is anonymous, just encode a unique ID.
    guard let n = Name(of: d, in: program.ast) else {
      ManglingOperator.anonymousFunctionDecl.write(to: &output)
      Base64VarUInt(d.rawValue).write(to: &output)
      return
    }

    if program.ast[d].isStatic {
      ManglingOperator.staticFunctionDecl.write(to: &output)
    } else {
      ManglingOperator.functionDecl.write(to: &output)
    }

    write(name: n, to: &output)
    Base64VarUInt(program.ast[d].genericParameters.count).write(to: &output)
    mangle(program.declTypes[d]!, of: program, to: &output)
  }

  mutating func mangle(_ symbol: AnyType, of program: TypedProgram, to output: inout Output) {
    if writeLookup(.type(symbol), to: &output) {
      return
    }

    assert(symbol[.isCanonical])
    switch symbol.base {
    case let t as LambdaType:
      write(lambda: t, of: program, to: &output)

    case let t as ParameterType:
      ManglingOperator.parameterType.write(to: &output)
      Base64Digit(rawValue: t.access.rawValue)!.description.write(to: &output)
      mangle(t.bareType, of: program, to: &output)

    case let t as ProductType:
      ManglingOperator.productType.write(to: &output)
      mangle(AnyDeclID(t.decl), of: program, to: &output)
      ManglingOperator.endOfSequence.write(to: &output)

    case let t as SumType:
      if t == .never {
        writeReserved(.never, to: &output)
      } else {
        fatalError("not implemented")
      }

    case let t as TupleType:
      if t == .void {
        writeReserved(.void, to: &output)
      } else {
        fatalError("not implemented")
      }

    default:
      unreachable()
    }

    symbolID[.type(symbol)] = nextSymbolID
    nextSymbolID += 1
  }

  private mutating func write(
    lambda t: LambdaType, of program: TypedProgram, to output: inout Output
  ) {
    if t.environment == .void {
      ManglingOperator.thinLambdaType.write(to: &output)
    } else {
      ManglingOperator.lambdaType.write(to: &output)
      mangle(t.environment, of: program, to: &output)
    }

    Base64VarUInt(t.inputs.count).write(to: &output)
    for i in t.inputs {
      write(string: i.label ?? "", to: &output)
      mangle(i.type, of: program, to: &output)
    }
    mangle(t.output, of: program, to: &output)
  }

  private func writeReserved(_ s: ReservedSymbol, to output: inout Output) {
    ManglingOperator.reserved.write(to: &output)
    s.write(to: &output)
  }

  /// If `s` has already been mangled into `output`, writes a lookup reference to its first
  /// occurrence and returns true. Otherwise, returns `false`.
  private func writeLookup(_ s: Symbol, to output: inout Output) -> Bool {
    guard let i = symbolID[s] else {
      return false
    }

    ManglingOperator.lookup.write(to: &output)
    Base64VarUInt(i).write(to: &output)
    return true
  }

  private func write(name: Name, to output: inout Output) {
    // Only encode notation and introducer; labels are encoded in types.
    var tag: UInt8 = 0
    if name.notation != nil { tag = 1 }
    if name.introducer != nil { tag = tag | 2 }

    Base64Digit(rawValue: tag)!.description.write(to: &output)
    if let n = name.notation {
      Base64Digit(rawValue: n.rawValue)!.description.write(to: &output)
    }
    if let i = name.introducer {
      Base64Digit(rawValue: i.rawValue)!.description.write(to: &output)
    }
    write(string: name.stem, to: &output)
  }

  private func write(string: String, to output: inout Output) {
    Base64VarUInt(string.count).write(to: &output)
    string.write(to: &output)
  }

}
