import AST
import Basic

/// The type checker for Val's declarations.
struct DeclChecker: DeclVisitor {

  typealias DeclResult = Bool

  /// The top-level type checker.
  unowned let checker: TypeChecker

  func visit(_ node: ModuleDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    var isWellFormed = true
    for decl in node {
      isWellFormed = isWellFormed && decl.accept(self)
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  func visit(_ node: ImportDecl) -> Bool {
    // Check that the imported module belongs to the current module's dependencies.
    let module = node.parentDeclSpace!.rootDeclSpace
    if !module.dependencies.contains(where: { $0.name == node.name }) {
      module.type.context.report(.cannotFind(module: node.name, range: node.range))
      return false
    }

    return true
  }

  func visit(_ node: PatternBindingDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    let useSite = node.parentDeclSpace!

    // Create a new constraint system to infer the pattern's type.
    var system = ConstraintSystem()

    // If there's a signature, use it as the authoritative type information. Otherwise, infer it
    // from the pattern initializer.
    let patternType: ValType
    if let sign = node.sign {
      // Contextualize the type signature.
      guard let signType = TypeChecker.contextualize(
              repr: sign, from: useSite, system: &system)
      else {
        setInvalid(pbd: node)
        return false
      }

      // If the signature contains opened generic parameter, require an initializer to infer them.
      guard !signType.hasVariables || node.initializer != nil else {
        checker.context.report(
          .referenceToGenericRequiresArguments(type: signType, range: node.pattern.range))
        setInvalid(pbd: node)
        return false
      }

      // Type-check the initializer if there's one.
      if node.initializer != nil {
        let solution = checker.check(
          expr: &(node.initializer!), expectedType: signType,
          useSite: useSite, system: &system)
        patternType = solution.reify(signType, freeVariablePolicy: .bindToErrorType)
      } else {
        patternType = signType
      }
    } else if node.initializer != nil {
      // Infer everything from the initializer alone.
      checker.check(expr: &(node.initializer!), useSite: useSite, system: &system)
      patternType = node.initializer!.type
    } else {
      // Unannotated declarations require an initializer.
      checker.context.report(.missingPatternInitializer(range: node.pattern.range))
      setInvalid(pbd: node)
      return false
    }

    assert(!patternType.hasVariables)

    // Apply the pattern's type.
    guard TypeChecker.assign(type: patternType, to: node.pattern) else {
      setInvalid(pbd: node)
      return false
    }

    node.setState(.typeChecked)
    for decl in node.varDecls {
      decl.setState(.typeChecked)
    }
    return true
  }

  func visit(_ node: VarDecl) -> Bool {
    // If the variable is introduced by a pattern binding declaration, type check it.
    if let pbd = node.patternBindingDecl {
      return visit(pbd)
    }

    return true
  }

  func visit(_ node: BaseFunDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }

    // Realize the function's signature. Note that we use the checker's 'contextualize' method, to
    // handle synthesized declarations that require type checking (e.g., constructors).
    _ = checker.contextualize(decl: node, from: node.rootDeclSpace)
    node.setState(.typeCheckRequested)

    /// Initialize the function's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    /// Type check the function's body, if any.
    if let body = node.body {
      checker.check(stmt: body, useSite: node)
    }

    node.setState(.typeChecked)
    return true
  }

  func visit(_ node: FunDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  func visit(_ node: CtorDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  func visit(_ node: FunParamDecl) -> Bool {
    fatalError("unreachable")
  }

  func visit(_ node: GenericTypeDecl) -> Bool {
    fatalError("unreachable")
  }

  func visit(_ node: NominalTypeDecl) -> Bool {
    fatalError("unreachable")
  }

  func visit(_ node: ProductTypeDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)
    var isWellFormed = true

    // Initialize the type's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    // Type check the type's direct members.
    node.updateMemberTables()
    for member in node.members {
      isWellFormed = isWellFormed && member.accept(self)
    }

    // Type check the type's conformances.
    let receiverType = node.receiverType
    for var conformance in node.conformanceTable.values {
      // Retrieve the view's `Self` generic parameter.
      let viewReceiverType = conformance.viewDecl.receiverType as! GenericParamType

      // Identify each requirement's implementation.
      var satisfied = true
      for (name, reqs) in conformance.viewDecl.valueMemberTable {
        for req in reqs {
          let reqParams: [GenericParamType]
          if let env = (req as? GenericDeclSpace)?.genericEnv {
            reqParams = env.params
          } else {
            reqParams = []
          }

          let candidates = node.lookup(qualified: name).values.filter({ (decl) -> Bool in
            // Skip the requirement itself.
            guard req !== decl else { return false }

            // Discard the candidate if it's not the same kind of construct. This prevents a method
            // requirement to be fulfilled with a property, and vice versa.
            guard (req is BaseFunDecl) && (decl is BaseFunDecl) ||
                  (req is VarDecl) && (decl is VarDecl)
            else { return false }

            // Discard the candidate if it doesn't have the same number of generic arguments.
            let declParams: [GenericParamType]
            if let env = (decl as? GenericDeclSpace)?.genericEnv {
              declParams = env.params
            } else {
              declParams = []
            }
            guard reqParams.count == declParams.count else { return false }

            // Check if the type of the requirement matches that of the candidate, modulo a
            // substition of its generic type parameters.
            var subst = [viewReceiverType: receiverType]
            subst.merge(zip(reqParams, declParams), uniquingKeysWith: { lhs, _ in lhs })

            return req.type.specialized(with: subst) == decl.type
          })

          if candidates.count == 1 {
            conformance.entries.append((req, candidates[0]))
          } else {
            node.type.context.report(
              .conformanceRequiresMissingImplementation(
                view: conformance.viewDecl.name,
                requirement: req.name,
                range: conformance.range ?? (node.range.lowerBound ..< node.range.lowerBound)))
            satisfied = false
          }
        }
      }

      // Update the conformance relation in the lookup table.
      conformance.state = satisfied
        ? .checked
        : .invalid
      node.conformanceTable[conformance.viewType] = conformance
      isWellFormed = isWellFormed && satisfied
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  func visit(_ node: ViewTypeDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)
    var isWellFormed = true

    // Initialize the type's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    // Type-check the type's members.
    node.updateMemberTables()
    for member in node.members {
      isWellFormed = isWellFormed && member.accept(self)
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  func visit(_ node: AliasTypeDecl) -> Bool {
    // Realize the aliased type.
    _ = node.realize()

    // Initiate type checking.
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    // Initialize the type's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    // FIXME: Type check the type's conformances.

    node.setState(.typeChecked)
    return true
  }

  func visit(_ node: GenericParamDecl) -> Bool {
    // Generic parameter are visited through their associated type declaration.
    fatalError("unreachable")
  }

  func visit(_ node: TypeExtDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)
    var isWellFormed = true

    // Bind the extension to the type it extends.
    guard node.computeExtendedDecl() != nil else { return false }

    // Type-check the extension's members.
    node.setState(.typeCheckRequested)
    for member in node.members {
      isWellFormed = isWellFormed && member.accept(self)
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  // MARK: Helpers

  /// Returns whether or not type checking succeeded, depending on the current state of the
  /// declaration.
  private func handleCheckState(_ node: Decl) -> Bool {
    switch node.state {
    case .invalid:
      // Type checking has already failed.
      return false

    case .typeChecked:
      // Type checking has already succeeded.
      return true

    case .typeCheckRequested:
      // Type checking was requested on the same path.
      checker.context.report(Diagnostic("circular dependency detected", anchor: node.range))
      return false

    default:
      fatalError("unreachable")
    }
  }

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

}
