import AST

/// A VIL emitter for l-values.
struct LValueEmitter: ExprVisitor {

  typealias ExprResult = Result<Operand, EmitterError>

  /// The srate in which the r-value is emitted.
  let _state: UnsafeMutablePointer<Emitter.State>

  /// The VIL module used by the emitter.
  let _module: UnsafeMutablePointer<Module>

  var funDecl: BaseFunDecl { _state.pointee.funDecl }

  var locals: SymbolTable {
    get { state.locals }
    _modify { yield &state.locals }
  }

  var state: Emitter.State {
    get { _state.pointee }
    _modify { yield &_state.pointee }
  }

  var module: Module {
    get { _module.pointee }
    _modify { yield &_module.pointee }
  }

  func visit(_ node: BoolLiteralExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: IntLiteralExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: FloatLiteralExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: StringLiteralExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: AssignExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: BaseCastExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: DynCastExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  mutating func visit(_ node: UnsafeCastExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: TupleExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: CallExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
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
    // explicit declarations in the capture list. Implicit captures are always immutable.
    guard funDecl.computeAllCaptures()[node.decl] == nil else {
      return .failure(.immutableCapture(node.decl))
    }

    switch node.decl {
    case let decl as VarDecl:
      // FIXME: Handle computed properties.
      assert(decl.hasStorage)

      let loc = locals[ObjectIdentifier(node.decl)]!
      assert(module.type(of: loc).isAddress)
      return .success(loc)

    case let decl as CaptureDecl:
      // The node is a reference to an explicit capture.
      let loc = locals[ObjectIdentifier(decl)]!
      assert(module.type(of: loc).isAddress)
      return .success(loc)

    case let decl as FunParamDecl:
      // The node is a reference to a parameter.
      let loc = locals[ObjectIdentifier(decl)]!
      assert(module.type(of: loc).isAddress)
      return .success(loc)

    case is BaseFunDecl:
      // Functions are not l-values.
      return .failure(.useOfRValueAsLValue(node))

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
    case .success(let loc):
      // Make sure the base has an address type.
      guard module.type(of: loc).isAddress else { return .failure(.useOfRValueAsLValue(node)) }
      let baseType = module.type(of: loc).valType

      // The expression must refer to a variable declaration; member functions are never l-values.
      guard let decl = node.decl as? VarDecl
      else { return .failure(.useOfRValueAsLValue(node)) }

      if decl.hasStorage {
        // The member has physical storage.
        switch baseType {
        case is ProductType:
          // The member refers to a stored property of a concrete product type.
          let memberAddr = module.insertRecordMemberAddr(
            record: loc, memberDecl: decl, type: VILType.lower(node.type).address, state.ip)
          return .success(Operand(memberAddr))

        case let bgType as BoundGenericType where bgType.decl.instanceType is ProductType:
          let memberAddr = module.insertRecordMemberAddr(
            record: loc, memberDecl: decl, type: VILType.lower(node.type).address, state.ip)
          return .success(Operand(memberAddr))

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
    return .failure(.useOfRValueAsLValue(node))
  }

  mutating func visit(_ node: AddrOfExpr) -> ExprResult {
    return node.value.accept(&self)
  }

  func visit(_ node: MatchExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: WildcardExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: ErrorExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

}
