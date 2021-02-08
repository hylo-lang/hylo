extension TypeRepr {

  /// Realize the semantic type denoted by the representation.
  ///
  /// - Parameter useSite: The declaration space in which the type representation resides.
  ///
  /// - Note:The method does not "recompute" the type realization if it is in the `realized` or
  ///   `invalid` state.
  @discardableResult
  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    guard type is UnresolvedType else { return type }

    switch self {
    case let this as ComponentTypeRepr: return AST.realize(this, from: useSite)
    case let this as CompoundTypeRepr : return AST.realize(this, from: useSite)
    default: fatalError("unreachable")
    }
  }

}

extension ComponentTypeRepr {

  /// Resolves the type declaration to which this component refers.
  ///
  /// - Parameter typeDecl: The declaration of the type that qualifies the component.
  public func resolve(qualifiedIn typeDecl: TypeDecl) -> TypeDecl? {
    let matches = typeDecl.lookup(qualified: name).types
    guard !matches.isEmpty else {
      return nil
    }

    // FIXME: Handle overloaded type decls.
    precondition(matches.count == 1, "overloaded type declarations are not supported yet")
    return matches[0]
  }

  /// Realizes the semantic type denoted by the component, assuming it is qualified by a type.
  ///
  /// The main purpose of this method is to realize the component as part of a compound identifier.
  /// Unlike `realize(unqualifiedFrom:)`, it looks for the referred type using a *qualified* name
  /// lookup from the given type declaration.
  ///
  /// - Parameters:
  ///   - typeDecl: The declaration of the type that qualifies the component.
  ///   - useSite: The declaration space in which the type representation resides.
  public func realize(qualifiedIn typeDecl: TypeDecl, from useSite: DeclSpace) -> ValType {
    let context = type.context

    guard let decl = resolve(qualifiedIn: typeDecl) else {
      context.report(.cannotFind(type: name, in: typeDecl.instanceType, range: range))
      type = context.errorType
      return type
    }

    if let specializedRepr = self as? SpecializedTypeRepr {
      let baseDecl = decl as! NominalTypeDecl
      AST.realize(typeRepr: specializedRepr, from: useSite, baseDecl: baseDecl)
    } else {
      type = decl.instanceType
    }

    type = decl.instanceType
    return type
  }

}

/// Realizes an unqualified identifier.
fileprivate func realize(_ typeRepr: ComponentTypeRepr, from useSite: DeclSpace) -> ValType {
  let context = typeRepr.type.context

  // Search for a type declaration.
  let matches = useSite.lookup(unqualified: typeRepr.name, in: context).types
  guard !matches.isEmpty else {
    context.report(.cannotFind(type: typeRepr.name, range: typeRepr.range))
    typeRepr.type = context.errorType
    return context.errorType
  }

  // FIXME: Handle overloaded type decls.
  precondition(matches.count == 1, "overloaded type declarations are not supported yet")

  if let specializedRepr = typeRepr as? SpecializedTypeRepr {
    let baseDecl = matches[0] as! NominalTypeDecl
    AST.realize(typeRepr: specializedRepr, from: useSite, baseDecl: baseDecl)
  } else {
    typeRepr.type = matches[0].instanceType
  }

  return typeRepr.type
}

fileprivate func realize(
  typeRepr: SpecializedTypeRepr, from space: DeclSpace, baseDecl: NominalTypeDecl
) {
  let context = baseDecl.type.context

  // Realize the generic arguments.
  var argTypes: [ValType] = []
  for arg in typeRepr.args {
    argTypes.append(arg.realize(unqualifiedFrom: space))
    guard arg.type !== context.errorType else {
      typeRepr.type = context.errorType
      return
    }
  }

  typeRepr.type = context.boundGenericType(decl: baseDecl, args: argTypes)
}

/// Realizes a compound identifier.
fileprivate func realize(_ typeRepr: CompoundTypeRepr, from useSite: DeclSpace) -> ValType {
  let context = typeRepr.type.context
  let components = typeRepr.components

  // Realize the base component, unqualified.
  let baseType = components[0].realize(unqualifiedFrom: useSite)
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
    let componentType = components[0].realize(qualifiedIn: baseDecl, from: useSite)
    guard componentType !== context.errorType else {
      components[i...].forEach({ $0.type = context.errorType })
      return context.errorType
    }

    // Components always have a nominal type.
    baseDecl = (componentType as! NominalType).decl
  }

  return typeRepr.lastComponent.type
}
