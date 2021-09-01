import AST
import Basic

/// A VIL emitter.
///
/// VIL generation consists of transforming the high-level representation of Val (i.e., AST nodes)
/// into a lower-level language, better suited for data-flow analysis and code optimizations.
public enum Emitter {

  /// A data structure representing the environment in which an AST is being lowered.
  public struct Environment {

    /// The declaration of the function being emitted.
    let funDecl: BaseFunDecl

    /// A symbol table that maps locally visible declarations to their emitted value, populated by
    /// function parameters and local pattern binding declarations.
    var locals: SymbolTable

    /// A set identifying the paths currently known to be borrowed.
    var loans: Set<PathIdentifier>

  }

  /// Lowers to VIL a module declaration into the specified builder.
  ///
  /// This method is the entry point to VIL code generation. It lowers a module declaration to its
  /// VIL representation, inserting VIL code into the module associated with the given builder.
  ///
  /// - Parameter decl: A module declaration. `decl` must be type checked: all symbols must be
  ///   resolved or substituted by error nodes.
  public static func emit(module decl: ModuleDecl) -> Module {
    // Create an emitter enviornment.
    var builder = Builder(module: Module(id: decl.name), context: decl.type.context)
    for decl in decl {
      emit(topLevel: decl, with: &builder)
    }
    return builder.module
  }

  /// Lowers the given "top-level" declaration.
  public static func emit(topLevel decl: Decl, with builder: inout Builder) {
    switch decl {
    case is ModuleDecl:
      fatalError("unreachable")
    case is ImportDecl:
      fatalError("not implemented")
    case is PatternBindingDecl:
      fatalError("not implemented")
    case let decl as BaseFunDecl:
      _ = emit(function: decl, with: &builder)
    case let decl as ProductTypeDecl:
      emit(productType: decl, with: &builder)
    case let decl as TypeExtnDecl:
      emit(typeExtn: decl, with: &builder)

    case is ViewTypeDecl, is AbstractTypeDecl:
      // Views and abstract types are not concrete; there's nothing to emit.
      break

    case is AliasTypeDecl:
      // FIXME: Emit the type's witness table.
      break

    default:
      fatalError("unreachable")
    }
  }

  public static func emit(member decl: Decl, with builder: inout Builder) {
    switch decl {
    case let decl as PatternBindingDecl:
      // Pattern binding declarations are member properties in the context of a type declaration
      emit(property: decl, with: &builder)

    default:
      emit(topLevel: decl, with: &builder)
    }
  }

  public static func emit(property decl: PatternBindingDecl, with builder: inout Builder) {
    assert(decl.isMember)

    // FIXME: Emit computed properties and handle initializes in synthesized constructors.
    assert(decl.varDecls.isEmpty || decl.varDecls[0].hasStorage)
    assert(decl.initializer == nil)
  }

  public static func emit(productType decl: ProductTypeDecl, with builder: inout Builder) {
    // Emit the the type's witness table(s).
    for conformance in decl.conformanceTable.values {
      var entries: [(decl: BaseFunDecl, impl: VILFun)] = []
      for (req, impl) in conformance.entries {
        if let reqFunDecl = req as? BaseFunDecl {
          let implFunDecl = impl as! BaseFunDecl
          let fun = emit(witness: implFunDecl, for: reqFunDecl, with: &builder)
          entries.append((reqFunDecl, fun))
        }
      }

      let table = ViewWitnessTable(
        type: decl.instanceType as! NominalType, view: conformance.viewType, entries: entries)
      builder.module.viewWitnessTables.append(table)
    }

    // Emit the direct members of the declaration.
    for member in decl.members {
      emit(member: member, with: &builder)
    }
  }

  public static func emit(typeExtn decl: TypeExtnDecl, with builder: inout Builder) {
    for member in decl.members {
      emit(member: member, with: &builder)
    }
  }

  /// Emits a function.
  public static func emit(function decl: BaseFunDecl, with builder: inout Builder) -> VILFun {
    // Create (i.e., declare) the function in the module.
    let fun = builder.getOrCreateFunction(from: decl)

    // We're done if the function doesn't have body.
    guard (decl.body != nil) || decl.isSynthesized else { return fun }

    let oldIP = builder.insertionPointer
    defer { builder.insertionPointer = oldIP }
    builder.insertionPointer = InsertionPointer(funName: fun.name)

    // Contextualize the function's arguments.
    let genericEnv = decl.genericEnv!
    var params = fun.type.paramTypes.map({ type -> ArgumentValue in
      return builder.buildArgument(type: type.contextualized(in: genericEnv, from: decl))
    })

    // Create the function's entry block.
    builder.insertionPointer!.blockID = builder.buildBasicBlock(params: params, isEntry: true)
    var locals: SymbolTable = [:]

    // Register the function's receiver in the local symbol table, if necessary.
    if let selfDecl = decl.selfDecl {
      var (selfType, _) = genericEnv.contextualize(selfDecl.type, from: decl)
      if decl.isMember {
        // Member functions accept their receiver as an implicit parameter.
        locals[ObjectIdentifier(selfDecl)] = params[0]
        params.removeFirst()
      } else {
        // Constructors should allocate `self`.
        assert(decl is CtorDecl)
        selfType = (selfType as! InoutType).base
        locals[ObjectIdentifier(selfDecl)] = builder.buildAllocStack(type: .lower(selfType))
      }
    }

    // Register the function's formal parameters in the local symbol table.
    let captureTable = decl.computeAllCaptures()
    for (capture, param) in zip(captureTable, params) {
      locals[ObjectIdentifier(capture.value.referredDecl)] = param
    }
    for (paramDecl, param) in zip(decl.params, params[captureTable.count...]) {
      locals[ObjectIdentifier(paramDecl)] = param
    }

    // Emit the function's body.
    var env = Environment(funDecl: decl, locals: locals, loans: [])
    guard let body = decl.body else {
      emit(synthesizedBodyOf: decl, locals: env.locals, with: &builder)
      return fun
    }

    emit(brace: body, in: &env, with: &builder)

    // If the function's a constructor, emit the implicit return statement.
    if decl is CtorDecl {
      let selfLoc = locals[ObjectIdentifier(decl.selfDecl!)]
      let selfVal = builder.buildLoad(location: selfLoc!)
      builder.buildRet(value: selfVal)
    }

    // Emit the function's epilogue.
    let funType = decl.type as! FunType
    let context = funType.context

    switch funType.retType {
    case context.nothingType:
      // If the function never returns, emit a halt statement.
      builder.buildHalt()

    case context.unitType:
      // The function returns unit.
      builder.buildRet(value: builder.buildUnit())

    default:
      // FIXME: Detect missing return value in non unit functions.
      break
    }

    return fun
  }

  /// Emits the synthesized body of a function.
  static func emit(
    synthesizedBodyOf decl: BaseFunDecl,
    locals: SymbolTable,
    with builder: inout Builder
  ) {
    assert(decl.isSynthesized)

    switch decl {
    case is CtorDecl:
      // Emit a synthesized constructor.
      let base = locals[ObjectIdentifier(decl.selfDecl!)]!
      let type = decl.parentDeclSpace as! NominalTypeDecl

      for (varDecl, paramDecl) in zip(type.storedVars, decl.params) {
        let memberAddr = builder.buildRecordMemberAddr(
          record: base, memberDecl: varDecl, type: VILType.lower(varDecl.type).address)
        let value = locals[ObjectIdentifier(paramDecl)]!
        builder.buildStore(target: memberAddr, value: value)
      }

      let selfVal = builder.buildLoad(location: base)
      builder.buildRet(value: selfVal)

    default:
      preconditionFailure("unexpected synthesized declaration '\(decl.name)'")
    }
  }

  /// Emits a function wrapping the implementation satisfying a conformance requirement.
  ///
  /// - Parameters:
  ///   - impl: The declaration of the function that implements the requirement.
  ///   - req: The declaration of the function requirement.
  ///   - builder: A VIL builder.
  static func emit(
    witness impl: BaseFunDecl,
    for req: BaseFunDecl,
    with builder: inout Builder
  ) -> VILFun {
    // Create the VIL function object.
    var mangler = Mangler()
    mangler.append(witnessImpl: impl, for: req)
    let name = VILName(mangler.finalize())
    let fun = builder.getOrCreateFunction(name: name, type: req.unappliedType as! FunType)

    // Create the function's entry point.
    builder.insertionPointer = InsertionPointer(funName: name)
    let params = fun.type.paramTypes.map({ type -> ArgumentValue in
      return builder.buildArgument(type: type)
    })
    builder.insertionPointer!.blockID = builder.buildBasicBlock(params: params, isEntry: true)

    // Emit the function's body.
    var args: [Value] = params
    if !(req is CtorDecl) {
      // Unless the function is a constructor, we have to open the self parameter.
      let openedSelfType = impl.selfDecl!.type
      if req.isMutating {
        args[0] = builder.buildOpenExistentialAddr(container: args[0], type: .lower(openedSelfType))

        if !impl.isMutating {
          args[0] = builder.buildLoad(location: args[0])
        }
      } else {
        assert(!impl.isMutating)
        args[0] = builder.buildOpenExistential(container: args[0], type: .lower(openedSelfType))
      }
    }

    let openedFun = builder.getOrCreateFunction(from: impl)
    let ret = builder.buildApply(callee: builder.buildFunRef(function: openedFun), args: args)
    builder.buildRet(value: ret)

    return fun
  }

  /// Emits a local pattern binding declaration.
  static func emit(
    localBinding decl: PatternBindingDecl,
    in env: inout Environment,
    with builder: inout Builder
  ) {
    // Create the variable locations for each name in the pattern.
    let lvalues = decl.pattern.namedPatterns.map({ pattern in
      emit(localVar: pattern.decl, into: &env.locals, with: &builder)
    })

    // Emit the initializer, if any.
    if let initializer = decl.initializer {
      // Emit a store right away if the pattern matches a single value.
      if let varDecl = decl.pattern.singleVarDecl {
        assert((lvalues.count == 1) && (lvalues[0] === env.locals[ObjectIdentifier(varDecl)]))
        emit(assign: initializer, to: lvalues[0], in: &env, with: &builder)
      } else {
        // FIXME: Handle destructuring,
        fatalError("not implemented")
      }
    }
  }

  /// Emits a local variable declaration.
  static func emit(
    localVar decl: VarDecl,
    into locals: inout SymbolTable,
    with builder: inout Builder
  ) -> Value {
    guard decl.state >= .typeChecked else {
      return builder.buildPoison()
    }
    precondition(decl.hasStorage, "computed properties are not supported yet")

    // Allocate storage on the stack for the variable.
    let location = builder.buildMarkUninitialized(
      location: builder.buildAllocStack(type: .lower(decl.type)))
//    let location = builder.buildAllocStack(type: .lower(decl.type))
    locals[ObjectIdentifier(decl)] = location
    return location
  }

  static func emit(
    brace: BraceStmt,
    in env: inout Environment,
    with builder: inout Builder
  ) {
    assert(builder.insertionPointer != nil, "insertion block not configured")

    for i in 0 ..< brace.stmts.count {
      switch brace.stmts[i] {
      case let decl as PatternBindingDecl:
        emit(localBinding: decl, in: &env, with: &builder)

      case let decl as FunDecl:
        _ = emit(function: decl, with: &builder)

        // Emit the value of each captured declaration. Capture with `val` or `var` semantics are
        // copied from the environment, and so we must emit a r-value either way.
        let captureTable = decl.computeAllCaptures()
        let partialArgs = captureTable.map({ (key, value) -> Value in
          // FIXME: Implement capture-by-reference (requires local bindings).
          assert(value.semantics != .mut, "not implemented")
          let expr = DeclRefExpr(decl: key.capturedDecl, type: value.type)
          return emit(rvalue: expr, in: &env, with: &builder)
        })

        // Local function with captures declarations require stack allocation.
        if !partialArgs.isEmpty {
          let fun = builder.buildFunRef(function: builder.getOrCreateFunction(from: decl))
          let loc = builder.buildAllocStack(type: .lower(decl.type))

          env.locals[ObjectIdentifier(decl)] = loc
          builder.buildStore(
            target: loc,
            value: builder.buildPartialApply(delegator: fun, partialArgs: partialArgs))
        }

      case let decl as NominalTypeDecl:
        emit(topLevel: decl, with: &builder)

      case let stmt as BraceStmt:
        emit(brace: stmt, in: &env, with: &builder)

      case let stmt as RetStmt:
        emit(stmt: stmt, in: &env, with: &builder)
        if i > brace.stmts.count - 1 {
          let context = env.funDecl.type.context
          context.report(.codeAfterReturnNeverExecuted(range: brace.stmts[i + 1].range))
          return
        }

      case let expr as Expr:
        // FIXME: Drop the result of the expression.
        _ = emit(rvalue: expr, in: &env, with: &builder)

      default:
        fatalError("unreachable")
      }
    }
  }

  static func emit(
    stmt: RetStmt,
    in env: inout Environment,
    with builder: inout Builder
  ) {
    let result: Value
    if let expr = stmt.value {
      result = emit(rvalue: expr, in: &env, with: &builder)
    } else {
      result = builder.buildUnit()
    }

    builder.buildRet(value: result)
  }

  /// Emits the assignment of the value represented by `expr` to `lvalue`.
  static func emit(
    assign expr: Expr,
    to target: Value,
    in env: inout Environment,
    with builder: inout Builder
  ) {
    let exprType = expr.type.dealiased.canonical
    let targetType = target.type.valType

    // If both operands have an existential layout and the r-value can be treated as a location,
    // then we may simply emit `copy_addr`.
    if targetType.isExistential && expr.type.isExistential {
      let result =
        withUnsafeMutablePointer(to: &env, { (_env) -> LValueEmitter.ExprResult in
        withUnsafeMutablePointer(to: &builder, { (_builder) -> LValueEmitter.ExprResult in
          var emitter = LValueEmitter(_env: _env, _builder: _builder)
          return expr.accept(&emitter)
        })
        })

      if case .success(let result) = result {
        // If the r-value has a different type than the l-value, it must be cast.
        var source = result.loc
        if targetType != exprType {
          source = builder.buildUnsafeCastAddr(source: source, type: target.type)
        }

        /// Copies the contents from the location on the right to the location on the left.
        builder.buildCopyAddr(target: target, source: source)
        return
      }
    }

    // Otherwise, emit the r-value and fall back to the regular path.
    let rvalue = emit(rvalue: expr, in: &env, with: &builder)
    emit(assign: rvalue, ofType: exprType, to: target, with: &builder)
  }

  /// Emits the assignment of `rvalue` to `lvalue`.
  static func emit(
    assign rvalue: Value,
    ofType rvalueType: ValType,
    to lvalue: Value,
    with builder: inout Builder
  ) {
    assert(rvalueType.isCanonical)

    // If the l-value has the same type as the r-value, we can emit a simple store.
    if lvalue.type.valType == rvalueType {
      builder.buildStore(target: lvalue, value: rvalue)
      return
    }

    // If both the l-value and the r-value have a union type, we can emit a simple store.
    if (rvalue.type.valType is UnionType) && (rvalueType is UnionType) {
      builder.buildStore(target: lvalue, value: rvalue)
      return
    }

    // If the l-value is an existential container, then there are two cases to consider.
    if lvalue.type.valType.isExistential {
      assert(lvalue.type.valType != rvalueType)

      if rvalueType.isExistential {
        // If the r-value is an existential container too (but with a different type), then we need
        // to cast its package.
        var tmp: Value = builder.buildAllocStack(type: rvalue.type)
        builder.buildStore(target: tmp, value: rvalue)
        tmp = builder.buildUnsafeCastAddr(source: tmp, type: lvalue.type)
        builder.buildCopyAddr(target: lvalue, source: tmp)
      } else {
        // If the r-value has a concrete type, then it must be packed into an existential container
        // before being stored
        let lvalue = builder.buildAllocExistential(container: lvalue, witness: rvalue.type)
        builder.buildStore(target: lvalue, value: rvalue)
      }

      return
    }

    builder.buildStore(target: lvalue, value: rvalue)
  }

  /// Emits a r-value.
  static func emit(
    rvalue expr: Expr,
    in env: inout Environment,
    with builder: inout Builder
  ) -> Value {
    // Emit an error value for any expression that has an error type.
    guard !expr.type.isError else { return builder.buildPoison() }

    let result =
      withUnsafeMutablePointer(to: &env, { (_env) -> RValueEmitter.ExprResult in
      withUnsafeMutablePointer(to: &builder, { (_builder) -> RValueEmitter.ExprResult in
        var emitter = RValueEmitter(_env: _env, _builder: _builder)
        return expr.accept(&emitter)
      })
      })

    switch result {
    case .success(let result):
      return result

    case .failure(let error):
      // FIXME: This should be reported.
      print(error)
      return builder.buildPoison()
    }
  }

  /// Emits a nil value.
  static func emitNil(context: Context, with builder: inout Builder) -> Value {
    let decl = context.getTypeDecl(for: .Nil) as! ProductTypeDecl
    return builder.buildRecord(typeDecl: decl, type: .lower(decl.instanceType))
  }

}
