import AST

/// A VIL emitter for l-values.
struct LValueEmitter: ExprVisitor {

  typealias ExprResult = Result<(loc: Value, pathID: PathIdentifier), EmitterError>

  /// The srate in which the r-value is emitted.
  let _state: UnsafeMutablePointer<Emitter.State>

  /// The VIL builder used by the emitter.
  let _builder: UnsafeMutablePointer<Builder>

  var context: Context { _state.pointee.funDecl.type.context }

  var funDecl: BaseFunDecl { _state.pointee.funDecl }

  var locals: SymbolTable {
    get { _state.pointee.locals }
    _modify { yield &_state.pointee.locals }
  }

  var loans: Set<PathIdentifier> {
    get { _state.pointee.loans }
    _modify { yield &_state.pointee.loans }
  }

  var builder: Builder {
    get { _builder.pointee }
    _modify { yield &_builder.pointee }
  }

  func visit(_ node: BoolLiteralExpr) -> ExprResult {
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

  func visit(_ node: BaseCastExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: DynCastExpr) -> ExprResult {
    return .failure(.immutableExpr)
  }

  mutating func visit(_ node: UnsafeCastExpr) -> ExprResult {
    // The value being cast has to be emittable as an l-value.
    switch node.value.accept(&self) {
    case .success(let result):
      guard node.type != node.value.type else {
        context.report(.unsafeCastToSameTypeHasNoEffect(type: node.type, range: node.range))
        return .success(result)
      }

      let cast = builder.buildUnsafeCastAddr(
        source: result.loc, type: VILType.lower(node.type).address)
      return .success((loc: cast, pathID: result.pathID))

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
    // The keys of a capture table are either original declarations, for implicit captures, or
    // explicit redeclarations in the capture list. Implicit captures are always immutable.
    guard funDecl.computeAllCaptures()[node.decl] == nil else {
      return .failure(.immutableCapture(node.decl))
    }

    switch node.decl {
    case let decl as VarDecl:
      // The node is either a reference to a local binding or to a member property. In either case,
      // we must check that the referred declaration is indeed mutable. In the latter case, we
      // must additionally check that `self` (referred implicitly) is mutable.
      guard decl.isMutable else { return .failure(.immutableBinding(decl)) }

      // FIXME: Handle computed properties.
      assert(decl.hasStorage)

      guard let loc = locals[ObjectIdentifier(node.decl)] else { fatalError("unreachable") }
      assert(loc.type.isAddress)
      return .success((loc: loc, pathID: .binding(decl: decl)))

    case let decl as CaptureDecl:
      // The node is a reference to an explicit capture.
      guard decl.semantics != .val else { return .failure(.immutableCapture(node.decl)) }
      let loc = locals[ObjectIdentifier(decl)]!
      assert(loc.type.isAddress)
      return .success((loc: loc, pathID: .binding(decl: decl)))

    case let decl as FunParamDecl:
      // The node is a reference to a mutable parameter.
      guard decl.policy == .inout || decl.policy == .consumingMutable else {
        return .failure(.immutableBinding(decl))
      }

      let loc = locals[ObjectIdentifier(decl)]!
      assert(loc.type.isAddress)
      return .success((loc: loc, pathID: .binding(decl: decl)))

    case is BaseFunDecl:
      return .failure(.immutableBinding(node.decl))

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
    case .success(let (loc, pathID)):
      // Make sure the base has an address type.
      guard loc.type.isAddress else { return .failure(.immutableExpr) }
      let baseType = loc.type.valType

      // The expression must refer to a variable declaration; member functions are never l-values.
      guard let decl = node.decl as? VarDecl
      else { return .failure(.immutableExpr) }

      if decl.hasStorage {
        // The member has physical storage.
        switch baseType {
        case is ProductType:
          // The member refers to a stored property of a concrete product type.
          let memberAddr = builder.buildRecordMemberAddr(
            record: loc, memberDecl: decl, type: VILType.lower(node.type).address)
          return .success((loc: memberAddr, .member(base: pathID, decl: node.decl)))

        case let bgType as BoundGenericType where bgType.decl.instanceType is ProductType:
          let memberAddr = builder.buildRecordMemberAddr(
            record: loc, memberDecl: decl, type: VILType.lower(node.type).address)
          return .success((loc: memberAddr, .member(base: pathID, decl: node.decl)))

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
    return .failure(.immutableExpr)
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
