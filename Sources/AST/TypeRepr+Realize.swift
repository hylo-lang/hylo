extension TypeRepr {

  /// Realize the semantic type denoted by the representation.
  ///
  /// - Parameter space: The space in which the type representation resides. Note that the method
  ///   does not "recompute" the type realization if it is in the `realized` or `invalud` state.
  @discardableResult
  public func realize(unqualifiedFrom space: DeclSpace) -> ValType {
    guard type is UnresolvedType else { return type }

    switch self {
    case let this as UnqualTypeRepr   : return AST.realize(this, within: space)
    case let this as CompoundTypeRepr : return AST.realize(this, within: space)
    default: fatalError("unreachable")
    }
  }

}

extension UnqualTypeRepr {

  public func realize(qualifiedFrom typeDecl: TypeDecl) -> ValType {
    let context = type.context

    let matches = typeDecl.lookup(qualified: name).types
    guard !matches.isEmpty else {
      context.report(.cannotFind(type: name, in: typeDecl.instanceType, range: range))
      type = context.errorType
      return type
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    type = matches[0].instanceType
    return type
  }

}

fileprivate func realize(_ typeRepr: UnqualTypeRepr, within space: DeclSpace) -> ValType {
  let context = typeRepr.type.context

  let matches = space.lookup(unqualified: typeRepr.name, in: context).types
  guard !matches.isEmpty else {
    context.report(.cannotFind(type: typeRepr.name, range: typeRepr.range))
    typeRepr.type = context.errorType
    return context.errorType
  }

  // FIXME: Handle overloaded type decls.
  precondition(matches.count == 1, "overloaded type declarations are not supported yet")
  typeRepr.type = matches[0].instanceType
  return typeRepr.type
}

fileprivate func realize(_ typeRepr: CompoundTypeRepr, within space: DeclSpace) -> ValType {
  let context = typeRepr.type.context
  let components = typeRepr.components

  // Realize the base component, unqualified.
  let baseType = components[0].realize(unqualifiedFrom: space)
  guard !(baseType is ErrorType) else {
    // The diagnostic is emitted by the failed attempt to realize the base.
    components.forEach({ $0.type = context.errorType })
    return context.errorType
  }

  // Handle built-ins.
  if baseType === context.builtin.instanceType {
    guard let builtinType = context.getBuiltinType(named: components[1].name) else {
      context.report(
        .cannotFind(builtin: components[1].name, range: components[1].range))
      components[2...].forEach({ $0.type = context.errorType })
      return context.errorType
    }
    components[1].type = builtinType

    // Built-in symbols are not namespaces.
    guard components.count == 2 else {
      context.report(.builtinTypesAreNotNamespaces(range: components[1].range))
      components[2...].forEach({ $0.type = context.errorType })
      return context.errorType
    }

    return builtinType
  }

  // Realize each subsequent component using their predecessor as a qualified based.
  guard var baseDecl: TypeDecl = (baseType as? NominalType)?.decl else {
    context.report(
      .cannotFind(type: components[1].name, in: baseType, range: components[1].range))
    components[1...].forEach({ $0.type = context.errorType })
    return context.errorType
  }

  for i in 1 ..< components.count {
    let matches = baseDecl.lookup(qualified: components[i].name).types
    guard !matches.isEmpty else {
      context.report(
        .cannotFind(
          type: components[i].name, in: baseDecl.instanceType,
          range: components[i].range))
      components[i...].forEach({ $0.type = context.errorType })
      return context.errorType
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    components[i].type = matches[0].instanceType

    baseDecl = matches[0]
  }

  return typeRepr.lastComponent.type
}
