import AST

/// The type checker for Val's declarations.
struct DeclChecker: DeclVisitor {

  typealias DeclResult = Void

  /// The top-level type checker.
  unowned let checker: TypeChecker

  func visit(_ node: Module) {
    for decl in node.decls {
      decl.accept(self)
    }
  }

  func visit(_ node: PatternBindingDecl) {
    node.setState(.typeCheckRequested)
    let useSite = node.parentDeclSpace!

    // Create a new constraint system to infer the pattern's type.
    var system = ConstraintSystem()

    // If there's a signature, use it as the authoritative type information. Otherwise, infer it
    // from the pattern initializer.
    var patternType: ValType
    if let sign = node.sign {
      // Realize the signature, generating diagnostics as necessary.
      var signType = sign.realize(unqualifiedFrom: useSite)
      assert(!signType.hasUnresolved)

      // Bail out if the signature is invalid.
      guard !signType.hasErrors else {
        setInvalid(pbd: node)
        return
      }

      if signType.hasTypeParams {
        // The signature is generic; we have to contextualize its generic parameters. We can assume
        // there's a declaration space from the use-sie, otherwise `realize()` would have failed to
        // resolve the type repr.
        guard let env = useSite.innermostGenericSpace!.prepareGenericEnv() else {
          setInvalid(pbd: node)
          return
        }

        // Contextualize the signature.
        signType = env.contextualize(
          signType, from: useSite,
          processingContraintsWith: { prototype in
            system.insert(RelationalConstraint(prototype: prototype, at: ConstraintLocator(sign)))
          })
      }

      // Check if we have to synthetize additional generic arguments, in case the the signature
      // refers to an "underspecialized" generic nominal type.
      guard let completeSignType = completeGenericArgs(
        type: signType, system: &system, locator: ConstraintLocator(sign))
      else {
        setInvalid(pbd: node)
        return
      }

      patternType = completeSignType

      // If the signature contains opened existentials, require an initializer to infer them.
      guard !patternType.hasVariables || node.initializer != nil else {
        checker.context.report(
          .referenceToGenericRequiresArguments(type: patternType, range: node.pattern.range))
        setInvalid(pbd: node)
        return
      }
    } else if node.initializer != nil {
      patternType = TypeVar(context: checker.context, node: node.pattern)
    } else {
      // Unannotated declarations require an initializer.
      checker.context.report(.missingPatternInitializer(range: node.pattern.range))
      setInvalid(pbd: node)
      return
    }

    // Type-check the initializer if there's one.
    if node.initializer != nil {
      checker.check(
        expr: &(node.initializer!), contextualType: patternType, useSite: useSite, system: &system)
      patternType = node.initializer!.type
      assert(!patternType.hasVariables)
    }

    // Apply the pattern's type.
    guard TypeChecker.assign(type: patternType, to: node.pattern) else {
      setInvalid(pbd: node)
      return
    }

    node.setState(.typeChecked)
    for decl in node.varDecls {
      decl.setState(.typeChecked)
    }
  }

  func visit(_ node: VarDecl) {
    // VarDecls are always visited through their pattern, never directly.
    fatalError("unreachable")
  }

  func visit(_ node: BaseFunDecl) {
    /// Realize the function's signature.
    _ = node.realize()
    node.setState(.typeCheckRequested)

    /// Initialize the function's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return
    }

    /// Type check the function's body, if any.
    if let body = node.body {
      checker.check(stmt: body, useSite: node)
    }

    node.setState(.typeChecked)
  }

  func visit(_ node: FunDecl) {
    visit(node as BaseFunDecl)
  }

  func visit(_ node: CtorDecl) {
    visit(node as BaseFunDecl)
  }

  func visit(_ node: FunParamDecl) {
  }

  func visit(_ node: NominalTypeDecl) {
  }

  func visit(_ node: ProductTypeDecl) {
    node.setState(.typeCheckRequested)

    /// Initialize the type's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return
    }

    // Type-check the type's members.
    for member in node.members {
      member.accept(self)
    }

    node.setState(.typeChecked)
  }

  func visit(_ node: ViewTypeDecl) {
  }

  func visit(_ node: GenericParamDecl) {
  }

  func visit(_ node: TypeExtDecl) {
    // Bind the extension to the type it extends.
    guard node.computeExtendedDecl() != nil else { return }

    // Type-check the extension's members.
    node.setState(.typeCheckRequested)
    for member in node.members {
      member.accept(self)
    }

    node.setState(.typeChecked)
  }

  // MARK: Helpers

  /// Marks a pattern binding declaration invalid, along with all its associated var decls.
  private func setInvalid(pbd node: PatternBindingDecl) {
    node.setState(.invalid)

    /// Mark all variable declaration for which we couldn't assign a concrete type as invalid,
    /// to prevent further use by the type checker.
    for decl in node.varDecls {
      if decl.type.hasUnresolved || decl.type.hasVariables {
        decl.type = checker.context.errorType
        decl.setState(.invalid)
      } else {
        decl.setState(.typeChecked)
      }
    }
  }

  /// Completes the argument list of an "underspecified" generic nominal type.
  ///
  /// If `type` is a generic nominal type with too few generic arguments, this method produces a
  /// bound generic type where all missing rgument is replaced by an opened existential. This
  /// only happens if `type` is a bare nominal type, or if it is a bound generic type with less
  /// arguments than the number of parameters of its declaration.
  private func completeGenericArgs(
    type: ValType, system: inout ConstraintSystem, locator: ConstraintLocator
  ) -> ValType? {
    guard let nominalType = type as? NominalType,
          let genericDecl = nominalType.decl as? GenericTypeDecl,
          let clause = genericDecl.genericClause
    else { return type }

    // Complete the argument list if necessary.
    var args = (nominalType as? BoundGenericType)?.args ?? []
    guard args.count < clause.params.count else { return type }

    args.append(contentsOf: clause.params.dropFirst(args.count).map({ $0.instanceType }))
    guard let env = genericDecl.prepareGenericEnv() else {
      return nil
    }

    let newType = checker.context.boundGenericType(decl: nominalType.decl, args: args)
    return env.contextualize(
      newType, from: nominalType.decl.rootDeclSpace,
      processingContraintsWith: { prototype in
        system.insert(RelationalConstraint(prototype: prototype, at: locator))
      })
  }

}
