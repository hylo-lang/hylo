import AST

/// An AST visitor that realizes the type of a declaration.
struct DeclRealizer: DeclVisitor {

  typealias DeclResult = Bool

  func visit(_ node: Module) -> Bool {
    return true
  }

  func visit(_ node: PatternBindingDecl) -> Bool {
    return true
  }

  func visit(_ node: VarDecl) -> Bool {
    return true
  }

  func visit(_ node: AbstractFunDecl) -> Bool {
    node.retSign?.realize(within: node)
    for param in node.params {
      _ = visit(param)
    }
    node.recomputeAppliedType()
    return true
  }

  func visit(_ node: FunDecl) -> Bool {
    visit(node as AbstractFunDecl)
  }

  func visit(_ node: CtorDecl) -> Bool {
    visit(node as AbstractFunDecl)
  }

  func visit(_ node: FunParamDecl) -> Bool {
    if let sign = node.sign {
      if let type = sign.realize(within: node.parentDeclSpace!) {
        node.type = type
      } else {
        node.type = node.type.context.unresolvedType
      }
    } else {
      node.type = TypeVar(context: node.type.context, node: node)
    }
    return true
  }

  func visit(_ node: AbstractNominalTypeDecl) -> Bool {
    assert(node.type is KindType)
    return true
  }

  func visit(_ node: ProductTypeDecl) -> Bool {
    assert(node.type is KindType)
    return true
  }

  func visit(_ node: ViewTypeDecl) -> Bool {
    assert(node.type is KindType)
    return true
  }

  func visit(_ node: GenericParamDecl) -> Bool {
    assert(node.type is GenericParamType)
    return true
  }

  func visit(_ node: TypeExtDecl) -> Bool {
    guard let type = node.extendedIdent.realize(within: node.parentDeclSpace!) else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      node.state = .invalid
      return false
    }

    guard let decl = (type as? NominalType)?.decl else {
      type.context.report(.nonNominalExtension(type, range: node.extendedIdent.range))
      node.state = .invalid
      return false
    }

    node.state = .bound(decl)
    return true
  }

}
