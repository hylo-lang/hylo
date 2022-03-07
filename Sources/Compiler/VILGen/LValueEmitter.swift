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

  func visit(_ node: StaticCastExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: RuntimeCastExpr) -> ExprResult {
    // Cast expressions are l-values iff the expression being cast is an l-value as well.
    var converted: Operand
    switch node.value.accept(&self) {
    case .success(let address):
      converted = address
    case let failure:
      return failure
    }

    // Determine the kind of conversion we have to emit.
    let sourceType = node.value.type.canonical
    let targetType = node.type.canonical

    // FIXME: Handle structural casts.
    precondition(!(sourceType is TupleType) || !(targetType is TupleType))

    if sourceType == targetType {
      // Same type conversion always succeeds.
      DiagDispatcher.instance.report(
        .castAlwaysSucceeds(from: node.value.type, to: node.type, range: node.range))
    } else if targetType is FunType {
      // Runtime conversion of function types always fails.
      DiagDispatcher.instance.report(.runtimeFunctionTypeConversion(range: node.range))
      module.insertFail(range: node.range, at: state.ip)
      converted = Operand(PoisonValue(type: .lower(targetType).address))
    } else if sourceType.isExistential && !targetType.isExistential {
      // Existential to grounded conversion borrows from the existential.
      converted = Operand(module.insertOpenExistAddr(
        container: converted, range: node.range, at: state.ip))
      converted = Operand(module.insertCheckedCastAddr(
        source: converted, type: .lower(targetType).address, range: node.range, at: state.ip))
    } else {
      // Default to scalar conversion.
      converted = Operand(module.insertCheckedCastAddr(
        source: converted, type: .lower(targetType).address, range: node.range, at: state.ip))
    }

    return .success(converted)
  }

  mutating func visit(_ node: PointerCastExpr) -> ExprResult {
    let pointer = Emitter.emit(
      borrow: node.value, mutably: false, state: &_state.pointee, into: &_module.pointee)
    let address = Operand(module.insertPointerCast(
      pointer: pointer, type: .lower(node.type).address, range: node.range, at: state.ip))
    return .success(address)
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
    // Look for the referred declaration in the local symbol table first.
    if let loc = locals[ObjectIdentifier(node.decl)] {
      if module.type(of: loc).isAddress {
        return .success(loc)
      }
    }

    // If the expression refers to a function, that function must be thin or it would have been
    // found in the local symbol table above.
    if node.decl is BaseFunDecl {
      return .failure(.useOfRValueAsLValue(node))
    }

    // FIXME: Handle computed properties.
    fatalError("not implemented")
  }

  func visit(_ node: TypeDeclRefExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: KindRefExpr) -> ExprResult {
    fatalError("not implemented")
  }

  mutating func visit(_ node: MemberDeclRefExpr) -> ExprResult {
    // The base has to be emittable as an l-value.
    switch node.base.accept(&self) {
    case .success(let baseAddr):
      // Make sure the base has an address type.
      let baseType = module.type(of: baseAddr)
      guard baseType.isAddress else { return .failure(.useOfRValueAsLValue(node)) }

      switch node.decl {
      case let decl as VarDecl where decl.hasStorage:
        let lvalue = Emitter.emit(
          storedMemberAddr: node,
          baseAddr: baseAddr,
          state: &_state.pointee,
          into: &_module.pointee)
        return .success(lvalue)

      case is BaseFunDecl:
        // Member functions are not l-values.
        return .failure(.useOfRValueAsLValue(node))

      default:
        fatalError("not implemented")
      }

    case let failure:
      return failure
    }
  }

  func visit(_ node: TupleMemberExpr) -> ExprResult {
    fatalError("not implemented")
  }

  func visit(_ node: SpecializedDeclRefExpr) -> ExprResult {
    fatalError("unreachable")
  }

  func visit(_ node: LambdaExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
  }

  func visit(_ node: AsyncExpr) -> ExprResult {
    return .failure(.useOfRValueAsLValue(node))
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
