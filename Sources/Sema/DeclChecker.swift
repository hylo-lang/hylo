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
    let useSite = node.parentDeclSpace!

    // Assign error types everywhere we couldn't assign a concrete type to prevent further use by
    // the type checker.
    func setInvalid() {
      for decl in node.varDecls where decl.type.hasUnresolved || decl.type.hasVariables {
        decl.type = checker.context.errorType
        decl.setInvalid()
      }
    }

    // If there's a signature, use it as the authoritative type information. Otherwise, infer it
    // from the pattern initializer.
    var patternType: ValType
    if let sign = node.sign {
      // Realize the signature, generating diagnostics as necessary.
      let signType = sign.realize(unqualifiedFrom: useSite)
      assert(!signType.hasUnresolved)

      // Bail out if the signature is invalid.
      guard !signType.hasErrors else {
        setInvalid()
        return
      }

      // Instantiate the signature's type if it contains references to generic type parameters.
      if signType.hasTypeParams {
        let gds = useSite.innermostGenericSpace!
        gds.prepareGenericEnv()
        patternType = gds.genericEnv.instantiate(signType, from: useSite)
      } else {
        patternType = signType
      }
      assert(patternType.isWellFormed)
    } else if node.initializer != nil {
      patternType = TypeVar(context: checker.context, node: node.pattern)
    } else {
      // Unannotated declarations require an initializer.
      checker.context.report(.missingPatternInitializer(range: node.pattern.range))
      setInvalid()
      return
    }

    // Type-check the initializer if there's one.
    if node.initializer != nil {
      checker.check(expr: &(node.initializer!), contextualType: patternType, useSite: useSite)
      patternType = node.initializer!.type
      assert(!patternType.hasVariables)
    }

    // Apply the pattern's type.
    guard TypeChecker.assign(type: patternType, to: node.pattern) else {
      setInvalid()
      return
    }
  }

  func visit(_ node: VarDecl) {
    // VarDecls are always visited through their pattern, never directly.
    fatalError("unreachable")
  }

  func visit(_ node: BaseFunDecl) {
    /// Realize the function's signature.
    _ = node.realize()

    /// Initialize the function's generic environment.
    node.prepareGenericEnv()

    /// Type check the function's body, if any.
    if let body = node.body {
      checker.check(stmt: body, useSite: node)
    }
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
    // Type-check the type's members.
    for member in node.members {
      member.accept(self)
    }
  }

  func visit(_ node: ViewTypeDecl) {
  }

  func visit(_ node: GenericParamDecl) {
  }

  func visit(_ node: TypeExtDecl) {
    // Bind the extension to the type it extends.
    guard node.computeExtendedDecl() != nil else { return }

    // Type-check the extension's members.
    for member in node.members {
      member.accept(self)
    }
  }

}
