/// Val's demangling algorithm.
struct Demangler {

  /// The list of demangled symbols, in order of appearence (a.k.a. the demangling lookup table).
  private var symbols: [DemangledSymbol] = []

  /// Creates an instance with an empty lookup table.
  init() {}

  /// Demangles a symbol from `stream`.
  mutating func demangle(from stream: inout Substring) -> DemangledSymbol? {
    var qualification: DemangledEntity? = nil

    while let o = takeOperator(from: &stream) {
      let demangled: DemangledSymbol?

      switch o {
      case .endOfSequence:
        demangled = nil
      case .functionDecl:
        demangled = takeFunction(qualifiedBy: qualification, from: &stream)
      case .lambdaType:
        fatalError()
      case .lookup:
        demangled = takeLookup(from: &stream)
      case .moduleDecl:
        demangled = takeModuleDecl(from: &stream)
      case .namespaceDecl:
        demangled = takeEntity(NamespaceDecl.self, qualifiedBy: qualification, from: &stream)
      case .parameterDecl:
        demangled = takeEntity(ParameterDecl.self, qualifiedBy: qualification, from: &stream)
      case .parameterType:
        demangled = takeParameterType(from: &stream)
      case .productType:
        demangled = takeProductType(from: &stream)
      case .productTypeDecl:
        demangled = takeEntity(ProductTypeDecl.self, qualifiedBy: qualification, from: &stream)
      case .reserved:
        demangled = takeReserved(from: &stream)
      case .thinLambdaType:
        demangled = takeThinLambdaType(from: &stream)
      case .translatonUnit:
        demangled = takeEntity(TranslationUnit.self,qualifiedBy: qualification, from: &stream)

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
      if let e = d.entity, e.isScope {
        qualification = e
        continue
      }

      // Otherise, we're done.
      return d
    }

    if let e = qualification {
      return .entity(e)
    } else {
      return nil
    }
  }

  /// Demangles an entity of type `T` from `stream`.
  private mutating func demangleEntity<T: Node>(
    _: T.Type, from stream: inout Substring
  ) -> DemangledEntity? {
    guard
      case .entity(let e) = demangle(from: &stream),
      e.kind == T.self
    else { return nil }

    return e
  }

  /// Demangles a type from `stream`.
  private mutating func demangleType(from stream: inout Substring) -> DemangledType? {
    guard case .type(let t) = demangle(from: &stream) else {
      return nil
    }
    return t
  }

  /// Demangles an entity declaration of type `T` from `stream`.
  private mutating func takeEntity<T: Node>(
    _ entity: T.Type, qualifiedBy qualification: DemangledEntity?,
    from stream: inout Substring
  ) -> DemangledSymbol? {
    guard
      let q = qualification,
      let s = takeString(from: &stream)
    else { return nil }
    return .entity(.init(qualification: q, kind: NodeKind(T.self), name: Name(stem: String(s))))
  }

  /// Demangles a function from `stream`.
  mutating func takeFunction(
    qualifiedBy qualification: DemangledEntity?,
    from stream: inout Substring
  ) -> DemangledSymbol? {
    guard
      let q = qualification,
      let name = takeName(from: &stream),
      let genericParameterCount = takeInteger(from: &stream),
      let type = demangleType(from: &stream)
    else { return nil }

    let e = DemangledEntity(
      qualification: q,
      kind: NodeKind(FunctionDecl.self),
      name: name,
      genericArgumentLabels: .init(repeating: nil, count: Int(genericParameterCount.rawValue)),
      type: type)
    return .entity(e)
  }

  /// Demangles a reference to a symbol from `stream`.
  mutating func takeLookup(from stream: inout Substring) -> DemangledSymbol? {
    guard
      let position = takeInteger(from: &stream)
    else { return nil }
    return symbols[Int(position.rawValue)]
  }

  /// Demangles a reserved symbol from `stream`.
  mutating func takeReserved(from stream: inout Substring) -> DemangledSymbol? {
    guard let s = take(ReservedSymbol.self, from: &stream) else { return nil }
    switch s {
    case .never:
      return .type(.never)
    case .void:
      return .type(.void)
    }
  }

  /// Demangles a module from `stream`.
  mutating func takeModuleDecl(from stream: inout Substring) -> DemangledSymbol? {
    guard
      let s = takeString(from: &stream)
    else { return nil }

    let e = DemangledEntity(
      qualification: nil, kind: NodeKind(ModuleDecl.self), name: Name(stem: String(s)))
    return .entity(e)
  }

  /// Demangles a parameter type from `stream`.
  mutating func takeParameterType(from stream: inout Substring) -> DemangledSymbol? {
    guard
      let a = take(AccessEffect.self, from: &stream),
      let b = demangleType(from: &stream)
    else { return nil }
    return .type(.parameter(access: a, value: b))
  }

  /// Demangles a product type from `stream`.
  mutating func takeProductType(from stream: inout Substring) -> DemangledSymbol? {
    guard
      let e = demangleEntity(ProductTypeDecl.self, from: &stream)
    else { return nil }
    return .type(.product(e))
  }

  /// Demangles a thin lambda type from `stream`.
  mutating func takeThinLambdaType(from stream: inout Substring) -> DemangledSymbol? {
    guard
      let inputCount = takeInteger(from: &stream)
    else { return nil }

    var inputs: [DemangledType.Parameter] = []
    inputs.reserveCapacity(Int(inputCount.rawValue))
    for _ in 0 ..< inputCount.rawValue {
      guard
        let l = takeString(from: &stream),
        let t = demangleType(from: &stream)
      else { return nil }
      inputs.append(.init(label: l.isEmpty ? nil : String(l), type: t))
    }

    guard
      let output = demangleType(from: &stream)
    else { return nil }

    return .type(.lambda(effect: .let, environment: .void, inputs: inputs, output: output))
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
    guard let tag = takeBase64Digit(from: &stream) else {
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

  /// Assuming `stream` starts with a base 64 digit, consumes and returns it. Returns `nil` iff
  /// data seems corrupted
  func takeBase64Digit(from stream: inout Substring) -> Base64Digit? {
    stream.popFirst().flatMap(Base64Digit.init(_:))
  }

  /// Assuming `stream` starts with a mangled `T`, consumes and returns it. Returns `nil` iff
  /// data seems corrupted
  func take<T: RawRepresentable>(
    _: T.Type, from stream: inout Substring
  ) -> T? where T.RawValue == UInt8 {
    guard let d = takeBase64Digit(from: &stream) else {
      return nil
    }
    return T(rawValue: d.rawValue)
  }

}
