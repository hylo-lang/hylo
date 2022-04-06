/// The type responsible for realizing the types from their AST representation.
struct TypeRealizer_ {

  private struct CacheKey: Hashable {

    let sign: ObjectIdentifier

    let useSite: ObjectIdentifier

    init(sign: Sign, useSite: DeclSpace) {
      self.sign = ObjectIdentifier(sign)
      self.useSite = ObjectIdentifier(useSite)
    }

  }

  var diags: [Diag] = []

  private var useSite: DeclSpace?

  private var binder = NameBinder(modules: [:], stdlib: nil)

  private var cache: [CacheKey: ValType] = [:]

  init() {}

  /// Realizes the type denoted by `sign`.
  mutating func realize(_ sign: Sign, useSite: DeclSpace, binder: inout NameBinder) -> ValType {
    // Check the cache.
    let key = CacheKey(sign: sign, useSite: useSite)
    if let type = cache[key] {
      return type
    }

    // Configure the visitor the signature.
    self.useSite = useSite
    swap(&self.binder, &binder)
    defer { swap(&self.binder, &binder) }

    // Realize the signature and memoize the result.
    let type = sign.accept(&self)
    cache[key] = type
    return type
  }

  fileprivate mutating func realize(_ sign: Sign) -> ValType {
    // Check the cache.
    let key = CacheKey(sign: sign, useSite: useSite!)
    if let type = cache[key] {
      return type
    }

    // Realize the signature and memoize the result.
    let type = sign.accept(&self)
    cache[key] = type
    return type
  }

  /// Realizes a single, unqualified component name.
  fileprivate mutating func realize(unqualified node: NameSign) -> ValType {
    guard let decl = binder.resolve(node, unqualifiedFrom: useSite!) else {
      // The error has been diagnosed by the name binder.
      return .error
    }

    // If the signature refers to an abstract type. We must create an associated type encoding its
    // relationship to the defining view. In other words, if `A` is abstract, we realize `Self.A`
    // rather than just `A`.
    if let decl = decl as? AbstractTypeDecl {
      let viewDecl = decl.parentDeclSpace as! ViewTypeDecl
      let viewSelf = viewDecl.selfTypeDecl.instanceType
      return AssocType(interface: decl.instanceType as! GenericParamType, base: viewSelf)
    }

    return decl.instanceType
  }

}

extension TypeRealizer_: SignVisitor {

  typealias SignResult = ValType

  mutating func visit(_ node: TupleSign) -> ValType {
    var elems: [TupleType.Elem] = []
    for elem in node.elems {
      if (elem.label == nil) || !elems.contains(where: { $0.label == elem.label }) {
        elems.append(TupleType.Elem(label: elem.label, type: realize(elem.sign)))
      } else {
        diags.append(.duplicateTupleLabel(range: elem.range))
      }
    }
    return TupleType(elems)
  }

  mutating func visit(_ node: FunSign) -> ValType {
    let params = node.params.map({ param in
      FunType.Param(label: param.label, type: realize(param))
    })
    let retType = realize(node.retSign)
    return FunType(params: params, retType: retType)
  }

  mutating func visit(_ node: FunParamSign) -> ValType {
    FunParamType(policy: node.policy, rawType: realize(node.rawSign))
  }

  mutating func visit(_ node: AsyncSign) -> ValType {
    AsyncType(base: realize(node.base))
  }

  mutating func visit(_ node: UnionSign) -> ValType {
    UnionType(node.elems.map({ realize($0) }))
  }

  mutating func visit(_ node: ViewCompSign) -> ValType {
    var failed = false
    var elems: [ViewType] = []

    for elem in node.views {
      switch realize(elem) {
      case let type as ViewType:
        elems.append(type)

      case is ErrorType:
        // The error has already been diagnosed.
        failed = true

      case let type:
        // We didn't realize a view type.
        diags.append(.nonViewTypeConformanceRequirement(type: type, range: elem.range))
        failed = true
      }
    }

    return failed ? .error : ViewCompositionType(elems)
  }

  mutating func visit(_ node: BareNameSign) -> ValType {
    realize(unqualified: node)
  }

  mutating func visit(_ node: SpecializedNameSign) -> ValType {
    let type = realize(unqualified: node)

    guard let type = type as? NominalType,
          let clause = type.decl.genericClause
    else {
      diags.append(.cannotSpecializeNonGenericType(type, range: node.range))
      return .error
    }

    guard clause.params.count >= node.args.count else {
      diags.append(.tooManyTypeArguments(
        expected: clause.params.count, got: node.args.count, range: node.range))
      return .error
    }

    let args = node.args.map({ realize($0) })
    return BoundGenericType(decl: type.decl, args: args)
  }

  mutating func visit(_ node: MemberSign) -> ValType {
    _  = realize(node.base)
    return realize(unqualified: node)
  }

  mutating func visit(_ node: ErrorSign) -> ValType {
    .error
  }

}
