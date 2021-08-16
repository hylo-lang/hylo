import AST

/// A VIL emitter for l-values.
struct LValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Value, EmitterError>

  /// The declaration of the function being emitted.
  let funDecl: BaseFunDecl

  /// A symbol table that maps locally visible declarations to their emitted value, populated by
  /// function parameters and local pattern binding declarations.
  var locals: SymbolTable = [:]

  /// The VIL builder used by the emitter.
  var builder: Builder

  /// The context in which the function declaration is defined.
  var context: AST.Context { funDecl.type.context }

  func visit(_ node: BoolLiteralExpr) -> Result<Value, EmitterError> {
    return .failure(.immutableExpr)
  }

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: FloatLiteralExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: StringLiteralExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: AssignExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: BaseCastExpr) -> Result<Value, EmitterError> {
    fatalError("unreachable")
  }

  func visit(_ node: DynCastExpr) -> Result<Value, EmitterError> {
    return .failure(.immutableExpr)
  }

  mutating func visit(_ node: UnsafeCastExpr) -> Result<Value, EmitterError> {
    // The value being cast has to be emittable as an l-value.
    switch node.value.accept(&self) {
    case .success(let source):
      guard node.type != node.value.type else {
        context.report(.unsafeCastToSameTypeHasNoEffect(type: node.type, range: node.range))
        return .success(source)
      }

      let cast = builder.buildUnsafeCastAddr(
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
    switch node.decl {
    case let decl as VarDecl:
      guard decl.isMutable else { return .failure(.immutableBinding(decl)) }

      // FIXME: Handle computed properties.
      assert(decl.hasStorage)

      // If the identifier denotes a local binding, lookup the local symbol table.
      if let loc = locals[ObjectIdentifier(node.decl)] {
        assert(loc.type.isAddress)
        return .success(loc)
      }

      // If the expression refers to a member declaration, we must emit a property access.
      if decl.isMember {
        // The expression should refer to a stored property declaration.
        assert(decl.parentDeclSpace is NominalTypeDecl)

        // Make sure that `self` is mutable.
        let selfDecl = funDecl.selfDecl!
        guard selfDecl.type is InoutType else {
          return .failure(.immutableSelf(property: decl))
        }

        let base = locals[ObjectIdentifier(selfDecl)]!
        let memberAddr = builder.buildRecordMemberAddr(
          record: base, memberDecl: decl, type: VILType.lower(node.type).address)
        return .success(memberAddr)
      }

      fatalError("unreachable")

    case let decl as FunParamDecl:
      guard decl.type is InoutType else { return .failure(.immutableBinding(decl)) }

      // If the expression refers to a mutating parameter, just look at the symbol table.
      let loc = locals[ObjectIdentifier(decl)]!
      assert(loc.type.isAddress)
      return .success(loc)

    default:
      // FIXME: Handle global symbols.
      fatalError("not implemented")
    }
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // The base has to be emittable as an l-value.
    switch node.base.accept(&self) {
    case .success(let base):
      // Make sure the base has an address type.
      guard base.type.isAddress else { return .failure(.immutableExpr) }
      let baseType = base.type.valType

      // The expression must refer to a variable declaration; member functions are never l-values.
      guard let decl = node.decl as? VarDecl
      else { return .failure(.immutableExpr) }

      if decl.hasStorage {
        // The member has physical storage.
        switch baseType {
        case is ProductType:
          // The member refers to a stored property of a concrete product type.
          let memberAddr = builder.buildRecordMemberAddr(
            record: base, memberDecl: decl, type: VILType.lower(node.type).address)
          return .success(memberAddr)

        case let bgType as BoundGenericType where bgType.decl.instanceType is ProductType:
          let memberAddr = builder.buildRecordMemberAddr(
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

  func visit(_ node: MatchExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

}
