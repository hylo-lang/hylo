import AST

/// A visitor that emits the VIL code of an l-value.
struct LValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The function emitter that owns this emitter.
  unowned let parent: FunctionEmitter

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: AssignExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    // The value being cast hast to be emittable as an l-value.
    switch node.value.accept(self) {
    case .success(let source):
      guard node.type != node.value.type else {
        parent.context.report(.unsafeCastToSameTimeHasNoEffect(type: node.type, range: node.range))
        return .success(source)
      }

      let cast = parent.builder.buildUnsafeCastAddr(
        source: source, type: VILType.lower(node.type).address)
      return .success(cast)

    case let failure:
      return failure
    }
  }

  func visit(_ node: TupleExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: CallExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: UnresolvedDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: UnresolvedMemberExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: UnresolvedQualDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: OverloadedDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: DeclRefExpr) -> ExprResult {
    // If the expression refers to a variable, we have to emit its access.
    if let decl = node.decl as? VarDecl {
      guard decl.hasStorage else { return .failure(.immutableLocation) }

      // If the variable is already in the local symbol table, just return its value.
      if let lv = parent.locals[ObjectIdentifier(node.decl)] {
        guard lv.type.isAddress else { return .failure(.immutableLocation) }
        return .success(lv)
      }

      // If the variable declaration resides in a nominal type declaration, then the expression
      // probably refers to a member declaration implicitly (i.e., without spelling `self`).
      if decl.parentDeclSpace is NominalTypeDecl {
        // Make sure that `self` is mutable.
        let selfDecl = parent.funDecl.selfDecl!
        guard selfDecl.type is InoutType else {
          return .failure(.immutableSelf)
        }

        let base = parent.locals[ObjectIdentifier(selfDecl)]!
        let memberAddr = parent.builder.buildRecordMemberAddr(
          record: base, memberDecl: decl, type: VILType.lower(node.type).address)
        return .success(memberAddr)
      }

      fatalError("unreachable")
    }

    // If the expression refers to a local function argument, just lookup the symbol table.
    if let decl = node.decl as? FunParamDecl {
      let lv = parent.locals[ObjectIdentifier(decl)]!
      guard lv.type.isAddress else { return .failure(.immutableLocation) }
      return .success(lv)
    }

    // FIXME: Handle global symbols.
    fatalError("not implemented")
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // The base has to be emittable as an l-value.
    switch node.base.accept(self) {
    case .success(let base):
      // Make sure the base has an address type.
      guard base.type.isAddress else { return .failure(.immutableLocation) }
      let baseType = base.type.valType

      // The expression must refer to a variable declaration; member functions are never l-values.
      guard let decl = node.decl as? VarDecl
      else { return .failure(.immutableLocation) }

      if decl.hasStorage {
        // The member has physical storage.
        switch baseType {
        case is ProductType:
          // The member refers to a stored property of a concrete product type.
          let memberAddr = parent.builder.buildRecordMemberAddr(
            record: base, memberDecl: decl, type: VILType.lower(node.type).address)
          return .success(memberAddr)

        case let bgType as BoundGenericType where bgType.decl.instanceType is ProductType:
          let memberAddr = parent.builder.buildRecordMemberAddr(
            record: base, memberDecl: decl, type: VILType.lower(node.type).address)
          return .success(memberAddr)

        default:
          // FIXME: Handle tuples.
          fatalError("not implemented")
        }
      } else {
        // FIXME: Handle property setters.
        fatalError("not implemented")
      }

    case let failure:
      return failure
    }
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: AsyncExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: AwaitExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: AddrOfExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

}
