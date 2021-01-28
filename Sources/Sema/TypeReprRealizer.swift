import AST

extension TypeRepr {

  /// Realize the semantic type denoted by the representation.
  ///
  /// - Parameter space: The space in which the type representation resides. Note that the method
  ///   does not "recompute" the type realization if it is in the `realized` or `invalud` state.
  @discardableResult
  func realize(within space: DeclSpace) -> ValType? {
    switch self {
    case let this as UnqualTypeRepr   : return Sema.realize(this, within: space)
    case let this as CompoundTypeRepr : return Sema.realize(this, within: space)
    default: fatalError("unreachable")
    }
  }

}

func realize(_ typeRepr: UnqualTypeRepr, within space: DeclSpace) -> ValType? {
  guard typeRepr.state != .invalid else { return nil }
  guard typeRepr.state != .realized else { return typeRepr.type }

  let context = typeRepr.type.context

  let matches = space.lookup(unqualified: typeRepr.name, in: context).types
  guard !matches.isEmpty else {
    context.report(.cannotFind(type: typeRepr.name, range: typeRepr.range))
    typeRepr.state = .invalid
    return nil
  }

  // FIXME: Handle overloaded type decls.
  precondition(matches.count == 1, "overloaded type declarations are not supported yet")
  typeRepr.type = matches[0].instanceType
  typeRepr.state = .realized
  return typeRepr.type
}

func realize(_ typeRepr: CompoundTypeRepr, within space: DeclSpace) -> ValType? {
  guard typeRepr.state != .invalid else { return nil }
  guard typeRepr.state != .realized else { return typeRepr.type }

  let context = typeRepr.type.context
  let components = typeRepr.components

  // Realize the base component, unqualified.
  guard let baseType = components[0].realize(within: space) else {
    // The diagnostic is emitted by the failed attempt to realize the base.
    components.forEach({ $0.state = .invalid })
    return nil
  }

  // Handle built-ins.
  if baseType === context.builtin.instanceType {
    guard let builtinType = context.getBuiltinType(named: components[1].name) else {
      context.report(
        .cannotFind(builtin: components[1].name, range: components[1].range))
      components[2...].forEach({ $0.state = .invalid })
      return nil
    }
    components[1].type = builtinType
    components[1].state = .realized

    // Built-in symbols are not namespaces.
    guard components.count == 2 else {
      context.report(.builtinTypesAreNotNamespaces(range: components[1].range))
      components[2...].forEach({ $0.state = .invalid })
      return nil
    }

    return builtinType
  }

  // Realize each subsequent component using their predecessor as a qualified based.
  guard var baseDecl: TypeDecl = (baseType as? NominalType)?.decl else {
    context.report(
      .cannotFind(type: components[1].name, in: baseType, range: components[1].range))
    components[1...].forEach({ $0.state = .invalid })
    return nil
  }

  for i in 1 ..< components.count {
    let matches = baseDecl.lookup(qualified: components[i].name).types
    guard !matches.isEmpty else {
      context.report(
        .cannotFind(
          type: components[i].name, in: baseDecl.instanceType,
          range: components[i].range))
      components[i...].forEach({ $0.state = .invalid })
      return nil
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    components[i].type = matches[0].instanceType
    components[i].state = .realized

    baseDecl = matches[0]
  }

  return typeRepr.lastComponent.type
}
