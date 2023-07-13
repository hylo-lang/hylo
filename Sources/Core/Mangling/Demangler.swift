/// Val's demangling algorithm.
struct Demangler {

  /// The program in which the demangled symbols are defined.
  let program: TypedProgram

  /// The list of demangled symbols, in order of appearence.
  private var symbols: [Symbol] = []

  private var context: AnyScopeID? = nil

  /// Creates an instance demangling a symbol defined in `program`.
  init(program: TypedProgram) {
    self.program = program
  }

  /// Demangles a symbol from `stream`.
  mutating func demangle(from stream: inout Substring) -> Symbol? {
    context = nil

    var lastDemangled: Symbol? = nil
    while let o = takeOperator(from: &stream) {
      let demangled: Symbol?

      switch o {
      case .endOfSequence:
        demangled = nil
      case .functionDecl:
        demangled = takeFunction(from: &stream)
      case .lambdaType:
        fatalError()
      case .lookup:
        demangled = takeLookup(from: &stream)
      case .moduleDecl:
        demangled = takeModuleDecl(from: &stream)
      case .namespaceDecl:
        demangled = take(NamespaceDecl.self, from: &stream)
      case .parameterDecl:
        demangled = take(ParameterDecl.self, from: &stream)
      case .parameterType:
        demangled = takeParameterType(from: &stream)
      case .productType:
        demangled = takeProductType(from: &stream)
      case .productTypeDecl:
        demangled = take(ProductTypeDecl.self, from: &stream)
      case .thinLambdaType:
        demangled = takeThinLambdaType(from: &stream)
      case .translatonUnit:
        demangled = takeTranslationUnit(from: &stream)

      case .anonymousFunctionDecl:
        fatalError()
      case .staticFunctionDecl:
        fatalError()
      case .remoteType:
        fatalError()
      case .traitDecl:
        fatalError()
      case .associatedTypeDecl:
        fatalError()
      case .associatedValueDecl:
        fatalError()
      case .genericParameterDecl:
        fatalError()
      case .importDecl:
        fatalError()
      case .typealiasDecl:
        fatalError()
      case .varDecl:
        fatalError()
      }

      // End of sequence reached if `demangled` is `nil`.
      guard let d = demangled else { break }
      if o != .lookup {
        symbols.append(d)
      }

      // Update the context and look for the next symbol if we demangled a scope.
      if let n = d.node, let s = AnyScopeID(n) {
        lastDemangled = d
        context = s
        continue
      }

      // Otherise, we're done.
      return d
    }

    return lastDemangled
  }

  /// Demangles a node of type `T` from `stream`.
  private mutating func demangle<T: Node>(_: T.Type, from stream: inout Substring) -> T.ID? {
    guard case .node(let n) = demangle(from: &stream) else {
      return nil
    }
    return T.ID(n)
  }

  /// Demangles a type from `stream`.
  private mutating func demangleType(from stream: inout Substring) -> AnyType? {
    guard case .type(let t) = demangle(from: &stream) else {
      return nil
    }
    return t
  }

  /// Demangles an entity declaration of type `T` from `stream`.
  private mutating func take<T: SingleEntityDecl>(
    _ entity: T.Type, from stream: inout Substring
  ) -> Symbol? {
    guard
      let parent = context,
      let baseName = takeString(from: &stream)
    else { return nil }

    let d = program.scopeToDecls[parent, default: []].first { (d) in
      guard let n = (program.ast[d] as? T)?.baseName else { return false }
      return n == baseName
    }
    return d.map({ .node(AnyNodeID($0)) })
  }

  /// Demangles a function from `stream`.
  mutating func takeFunction(from stream: inout Substring) -> Symbol? {
    guard
      let parent = context,
      let name = takeName(from: &stream),
      let genericParameterCount = takeInteger(from: &stream),
      let type = LambdaType(demangleType(from: &stream))
    else { return nil }

    let d = program.scopeToDecls[parent, default: []].first { (d) in
      guard
        let f = program.ast[d] as? FunctionDecl,
        f.identifier?.value == name.stem,
        f.notation?.value == name.notation,
        f.genericParameters.count == genericParameterCount.rawValue
      else { return false }
      return program.declTypes[d]! == type
    }
    return d.map({ .node(AnyNodeID($0)) })
  }

  /// Demangles a reference to a symbol from `stream`.
  mutating func takeLookup(from stream: inout Substring) -> Symbol? {
    guard let position = takeInteger(from: &stream) else {
      return nil
    }
    return symbols[Int(position.rawValue)]
  }

  /// Demangles a module from `stream`.
  mutating func takeModuleDecl(from stream: inout Substring) -> Symbol? {
    guard
      let n = takeString(from: &stream),
      let d = program.ast.modules.first(where: { program.ast[$0].baseName == n })
    else { return nil }

    return .node(AnyNodeID(d))
  }

  /// Demangles a parameter type from `stream`.
  mutating func takeParameterType(from stream: inout Substring) -> Symbol? {
    guard
      let a = take(AccessEffect.self, from: &stream),
      let b = demangleType(from: &stream)
    else { return nil }

    let result = ParameterType(a, b)
    return .type(^result)
  }

  /// Demangles a product type from `stream`.
  mutating func takeProductType(from stream: inout Substring) -> Symbol? {
    guard let d = demangle(ProductTypeDecl.self, from: &stream) else {
      return nil
    }

    let result = ProductType(d, ast: program.ast)
    return .type(^result)
  }

  /// Demangles a thin lambda type from `stream`.
  mutating func takeThinLambdaType(from stream: inout Substring) -> Symbol? {
    guard let inputCount = takeInteger(from: &stream) else {
      return nil
    }

    var inputs: [CallableTypeParameter] = []
    inputs.reserveCapacity(Int(inputCount.rawValue))
    for _ in 0 ..< inputCount.rawValue {
      guard
        let l = takeString(from: &stream),
        let t = demangleType(from: &stream)
      else { return nil }
      inputs.append(.init(label: l.isEmpty ? nil : String(l), type: t))
    }

    guard let output = demangleType(from: &stream) else {
      return nil
    }

    let result = LambdaType(inputs: inputs, output: output)
    return .type(^result)
  }

  /// Demangles a translation unit from `stream`.
  mutating func takeTranslationUnit(from stream: inout Substring) -> Symbol? {
    guard
      let parent = context.flatMap(ModuleDecl.ID.init(_:)),
      let baseName = takeString(from: &stream)
    else { return nil }

    let u = program.ast[parent].sources.first { (u) in
      program.ast[u].site.file.baseName == baseName
    }
    return u.map({ .node(AnyNodeID($0)) })
  }

  /// If `stream` starts with a mangling operator, consumes and returns it. Otherwise, returns
  /// `nil` without mutating `stream`.
  func takeOperator(from stream: inout Substring) -> ManglingOperator? {
    if stream.isEmpty { return nil }

    var i = stream.startIndex
    if stream[i].isASCII && stream[i].isLowercase {
      i = stream.index(after: i)
      if i == stream.endIndex { return nil }
    }

    if let o = ManglingOperator(rawValue: String(stream[...i])) {
      stream = stream[stream.index(after: i)...]
      return o
    } else {
      return nil
    }
  }

  /// Assuming `stream` starts with a mangled name, consumes and returns it. Returns `nil` iff
  /// data seems corrupted
  func takeName(from stream: inout Substring) -> Name? {
    guard let tag = stream.popFirst().flatMap(Base64Digit.init(_:)) else {
      return nil
    }

    let notation: OperatorNotation?
    if (tag.rawValue & 1) != 0 {
      guard let n = take(OperatorNotation.self, from: &stream) else { return nil }
      notation = n
    } else {
      notation = nil
    }

    let introducer: AccessEffect?
    if (tag.rawValue & 2) != 0 {
      guard let i = take(AccessEffect.self, from: &stream) else { return nil }
      introducer = i
    } else {
      introducer = nil
    }

    guard let stem = takeString(from: &stream) else {
      return nil
    }

    return Name(stem: String(stem), labels: [], notation: notation, introducer: introducer)
  }

  /// Assuming `stream` starts with a mangled string, consumes and returns it. Returns `nil` iff
  ///the data seems corrupted
  func takeString(from stream: inout Substring) -> Substring? {
    guard let length = takeInteger(from: &stream) else {
      return nil
    }

    let j = stream.index(stream.startIndex, offsetBy: Int(length.rawValue))
    let r = stream[..<j]
    stream = stream[j...]
    return r
  }

  /// Assuming `stream` starts with a mangled integer, consumes and returns it. Returns `nil` iff
  /// data seems corrupted.
  func takeInteger(from stream: inout Substring) -> Base64VarUInt? {
    guard let (v, i) = Base64VarUInt.decode(from: stream) else {
      return nil
    }
    stream = stream[i...]
    return v
  }

  /// Assuming `stream` starts with a mangled `T`, consumes and returns it. Returns `nil` iff
  /// data seems corrupted
  func take<T: RawRepresentable>(
    _: T.Type, from stream: inout Substring
  ) -> T? where T.RawValue == UInt8 {
    guard let d = stream.popFirst().flatMap(Base64Digit.init(_:)) else {
      return nil
    }
    return T(rawValue: d.rawValue)
  }

}
