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
    case let this as TupleTypeRepr    : return AST.realize(this, from: useSite)
    case let this as FunTypeRepr      : return AST.realize(this, from: useSite)
    case let this as AsyncTypeRepr    : return AST.realize(this, from: useSite)
    case let this as InoutTypeRepr    : return AST.realize(this, from: useSite)
    case let this as ViewCompTypeRepr : return AST.realize(this, from: useSite)
    case let this as UnionTypeRepr    : return AST.realize(this, from: useSite)
    default: fatalError("unreachable")
    }
  }

}

extension ComponentTypeRepr {

  /// Resolves the type declaration to which this component refers.
  ///
  /// - Parameter typeDecl: The declaration of the type that qualifies the component.
  public func resolve(qualifiedIn typeDecl: TypeDecl) -> TypeDecl? {
    // FIXME: Handle other type declarations.
    guard let typeDecl = typeDecl as? NominalTypeDecl else {
      fatalError("not implemented")
    }

    let matches = typeDecl.lookup(qualified: name).types
    guard !matches.isEmpty else {
      return nil
    }

    assert(matches.count == 1)
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

  // Bypass name lookup if the repr is `Any` or `Nothing`.
  switch typeRepr.name {
  case "Any":
    typeRepr.type = context.anyType
    return context.anyType

  case "Nothing":
    typeRepr.type = context.nothingType
    return context.nothingType

  default:
    break
  }

  // Search for a type declaration.
  let matches = useSite.lookup(unqualified: typeRepr.name, in: context).types
  guard !matches.isEmpty else {
    context.report(.cannotFind(type: typeRepr.name, range: typeRepr.range))
    typeRepr.type = context.errorType
    return context.errorType
  }

  assert(matches.count == 1)

  // If the repr is specialized (e.g., `Foo<Bar>`), we must realize its arguments as well.
  if let specializedRepr = typeRepr as? SpecializedTypeRepr {
    let baseDecl = matches[0] as! GenericTypeDecl
    AST.realize(typeRepr: specializedRepr, from: useSite, baseDecl: baseDecl)
  } else {
    typeRepr.type = matches[0].instanceType
  }

  return typeRepr.type
}

/// Realizes a specialized identifier.
fileprivate func realize(
  typeRepr: SpecializedTypeRepr, from space: DeclSpace, baseDecl: GenericTypeDecl
) {
  let context = baseDecl.type.context

  // Make sure we didn't get too many arguments.
  guard let clause = baseDecl.genericClause else {
    context.report(
      .tooManyGenericArguments(
        type: baseDecl.instanceType, got: typeRepr.args.count, expected: 0,
        range: typeRepr.range))
    typeRepr.type = context.errorType
    return
  }

  guard clause.params.count >= typeRepr.args.count else {
    context.report(
      .tooManyGenericArguments(
        type: baseDecl.instanceType, got: typeRepr.args.count, expected: clause.params.count,
        range: typeRepr.range))
    typeRepr.type = context.errorType
    return
  }

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
    let componentType = components[i].realize(qualifiedIn: baseDecl, from: useSite)
    guard componentType !== context.errorType else {
      components[i...].forEach({ $0.type = context.errorType })
      return context.errorType
    }

    // Components always have a nominal type.
    baseDecl = (componentType as! NominalType).decl
  }

  return typeRepr.lastComponent.type
}

/// Realizes a tuple type signature.
fileprivate func realize(_ typeRepr: TupleTypeRepr, from useSite: DeclSpace) -> ValType {
  let elems = typeRepr.elems.map({ (repr: TupleTypeReprElem) -> TupleType.Elem in
    TupleType.Elem(
      label: repr.label,
      type: repr.sign.realize(unqualifiedFrom: useSite))
  })

  typeRepr.type = typeRepr.type.context.tupleType(elems)
  return typeRepr.type
}

/// Realizes a function type signature.
fileprivate func realize(_ typeRepr: FunTypeRepr, from useSite: DeclSpace) -> ValType {
  let paramType = typeRepr.paramSign.realize(unqualifiedFrom: useSite)
  let retType = typeRepr.retSign.realize(unqualifiedFrom: useSite)

  typeRepr.type = typeRepr.type.context.funType(paramType: paramType, retType: retType)
  return typeRepr.type
}

/// Realizes an asynchronous type signature.
fileprivate func realize(_ typeRepr: AsyncTypeRepr, from useSite: DeclSpace) -> ValType {
  let baseType = typeRepr.base.realize(unqualifiedFrom: useSite)
  if baseType is AsyncType {
    baseType.context.report(.superfluousTypeModifier(range: typeRepr.modifierRange))
  }

  typeRepr.type = baseType.context.asyncType(of: baseType)
  return typeRepr.type
}

/// Realizes an in-out type signature.
fileprivate func realize(_ typeRepr: InoutTypeRepr, from useSite: DeclSpace) -> ValType {
  let baseType = typeRepr.base.realize(unqualifiedFrom: useSite)

  typeRepr.type = baseType.context.inoutType(of: baseType)
  return typeRepr.type
}

/// Realizes a view composition signature.
fileprivate func realize(_ typeRepr: ViewCompTypeRepr, from useSite: DeclSpace) -> ValType {
  assert(!typeRepr.views.isEmpty, "ill-formed AST; composition is empty")
  let context = typeRepr.type.context

  var views: [ViewType] = []
  for repr in typeRepr.views {
    switch repr.realize(unqualifiedFrom: useSite) {
    case let view as ViewType:
      views.append(view)

    case is ErrorType:
      typeRepr.type = context.errorType
      return context.errorType

    case let type:
      context.report(.nonViewTypeInViewComposition(type: type, range: repr.range))
      typeRepr.type = context.errorType
      return context.errorType
    }
  }

  typeRepr.type = context.viewCompositionType(views)
  return typeRepr.type
}

/// Realizes a union type signature.
fileprivate func realize(_ typeRepr: UnionTypeRepr, from useSite: DeclSpace) -> ValType {
  assert(!typeRepr.elems.isEmpty, "ill-formed AST; type union is empty")
  let elems = typeRepr.elems.map({ repr in repr.realize(unqualifiedFrom: useSite) })

  typeRepr.type = typeRepr.type.context.unionType(elems)
  return typeRepr.type
}
