import AST
import Basic

/// The type checker for Val's declarations.
struct DeclChecker: DeclVisitor {

  typealias DeclResult = Bool

  func visit(_ node: ModuleDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    var isWellFormed = true
    for decl in node {
      isWellFormed = decl.accept(self) && isWellFormed
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
    let context = useSite.rootDeclSpace.type.context

    // Create a new constraint system to infer the pattern's type.
    var system = ConstraintSystem()

    // If there's a signature, use it as the authoritative type information. Otherwise, infer it
    // from the pattern initializer.
    let patternType: ValType
    if let sign = node.sign {
      // Contextualize the type signature.
      guard let signType = TypeChecker.contextualize(sign: sign, from: useSite, system: &system)
      else {
        setInvalid(pbd: node)
        return false
      }

      // If the signature contains opened generic parameter, require an initializer to infer them.
      guard !signType.hasVariables || node.initializer != nil else {
        context.report(
          .referenceToGenericRequiresArguments(type: signType, range: node.pattern.range))
        setInvalid(pbd: node)
        return false
      }

      // Type-check the initializer if there's one.
      if node.initializer != nil {
        let solution = TypeChecker.check(
          expr        : &(node.initializer!),
          expectedType: signType,
          useSite     : useSite,
          system      : &system)
        patternType = solution.reify(signType, freeVariablePolicy: .bindToErrorType)
      } else {
        patternType = signType
      }
    } else if node.initializer != nil {
      // Infer everything from the initializer alone.
      TypeChecker.check(expr: &(node.initializer!), useSite: useSite, system: &system)
      patternType = node.initializer!.type
    } else {
      // Unannotated declarations require an initializer.
      context.report(.missingPatternInitializer(range: node.pattern.range))
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
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    // If the variable is introduced by a pattern binding declaration, type check it.
    if let pbd = node.patternBindingDecl {
      return visit(pbd)
    }

    // Otherwise, the declaration belongs to a binding pattern in a case statement and its type
    // should have been inferred before it is looked up.
    preconditionFailure("cannot type check variable declaration in a binding pattern")
  }

  func visit(_ node: BaseFunDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }

    // Realize the function's signature. The call to `TypeChecker.contextualize` only serves to
    // handle synthesized declarations that require type checking (e.g., constructors).
    _ = TypeChecker.contextualize(decl: node, from: node.rootDeclSpace)
    node.setState(.typeCheckRequested)

    /// Initialize the function's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    /// Type check the function's body, if any.
    if let body = node.body {
      TypeChecker.check(stmt: body, useSite: node)
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
      isWellFormed = member.accept(self) && isWellFormed
    }

    let receiverType = node.receiverType
    let context = node.type.context

    // Type check the type's conformances.
    for var conformance in node.conformanceTable.values {
      var satisfied = true

      // Build a substitution table mapping the view's generic type parameters to their
      // specialization in the conforming type.
      let viewReceiverType = conformance.viewDecl.receiverType as! GenericParamType
      var substitutions = [viewReceiverType: receiverType]

      // Verify that abstract type requirements are satisfied.
      for case let req as AbstractTypeDecl in conformance.viewDecl.typeMemberTable.values {
        // Look for a declaration in the type member table that shadows the abstract requirement.
        guard let member = node.typeMemberTable[req.name],
              member.state != .invalid,
              !(member is AbstractTypeDecl)
        else {
          context.report(
            .conformanceRequiresMatchingImplementation(
              view: conformance.viewDecl.name,
              requirement: req.name,
              range: conformance.range ?? (node.range.lowerBound ..< node.range.lowerBound)))
          satisfied = false
          continue
        }

        // Register the type member as a substitution for the abstract requirement.
        substitutions[req.instanceType as! GenericParamType] = member.instanceType
      }

      // Verify that value member requirements are satisfied.
      for (name, reqs) in conformance.viewDecl.valueMemberTable {
        for req in reqs {
          // FIXME: Skip requirements that have a default implementation.

          // Gather the generic parameters of the requirement.
          let reqParams = (req as? GenericDeclSpace)?.genericEnv?.params ?? []

          // Search for a valid candidate.
          let candidates = node.lookup(qualified: name).values.filter({ (decl) -> Bool in
            // Skip the requirement itself.
            guard req !== decl else { return false }

            // Discard the candidate if it's not the same kind of construct, preventing a method
            // requirement from being fulfilled by a property, or vice versa.
            guard (req is BaseFunDecl) && (decl is BaseFunDecl) ||
                  (req is VarDecl)     && (decl is VarDecl)
            else { return false }

            // Discard the candidate if it doesn't have the same number of generic arguments.
            let declParams = (decl as? GenericDeclSpace)?.genericEnv?.params ?? []
            guard reqParams.count == declParams.count else { return false }

            // Substitute the generic parameters of the requirement with that of the candidate.
            let subst = substitutions.merging(disjointKeysWithValues: zip(reqParams, declParams))
            let reqType = req.type.specialized(with: subst)

            // Select the candidate if its type matches that of the requirement.
            return reqType.dealiased == decl.type.dealiased
          })

          if candidates.count == 1 {
            conformance.entries.append((req, candidates[0]))
          } else {
            context.report(
              .conformanceRequiresMatchingImplementation(
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
      isWellFormed = satisfied && isWellFormed
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
      isWellFormed = member.accept(self) && isWellFormed
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  func visit(_ node: AliasTypeDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }

    // Realize the aliased type.
    if node.state < .realizationRequested {
      node.setState(.realizationRequested)
      _ = node.realizeAliasedType()
    }

    // Initiate type checking.
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

  func visit(_ node: AbstractTypeDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    // FIXME: Handle type requirements.
    // FIXME: Warn or complain about conformances added through type requirements rather than in
    // the inheritance clause (e.g., type E where E: V)

    node.setState(.typeChecked)
    return true
  }

  func visit(_ node: GenericParamDecl) -> Bool {
    // Generic parameter are visited through their associated type declaration.
    fatalError("unreachable")
  }

  func visit(_ node: TypeExtDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }

    // Bind the extension to the type it extends.
    guard node.computeExtendedDecl() != nil else { return false }

    // Type-check the extension's members.
    node.setState(.typeCheckRequested)
    var isWellFormed = true
    for member in node.members {
      isWellFormed = member.accept(self) && isWellFormed
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
      let context = node.parentDeclSpace!.rootDeclSpace.type.context
      context.report(Diagnostic("circular dependency detected", anchor: node.range))
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
        decl.type = decl.type.context.errorType
        decl.setState(.invalid)
      } else {
        decl.setState(.typeChecked)
      }
    }
  }

}
