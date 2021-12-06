import AST
import Basic

/// The type checker for Val's declarations.
struct DeclChecker: DeclVisitor {

  typealias DeclResult = Bool

  mutating func visit(_ node: ModuleDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    var isWellFormed = true
    for decl in node {
      isWellFormed = decl.accept(&self) && isWellFormed
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
    var success = true

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
          .referenceToGenericTypeRequiresArguments(type: signType, range: node.pattern.range))
        setInvalid(pbd: node)
        return false
      }

      // Type-check the initializer if there's one.
      if node.initializer != nil {
        let (succeeded, solution) = TypeChecker.check(
          expr: &(node.initializer!),
          fixedType: signType,
          useSite: useSite,
          system: &system)
        patternType = solution.reify(signType, substPolicy: .bindToErrorType)
        success = succeeded
      } else {
        patternType = signType
      }
    } else if node.initializer != nil {
      // Infer everything from the initializer alone.
      (success, _) = TypeChecker.check(
        expr: &(node.initializer!), fixedType: nil, useSite: useSite, system: &system)
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
    return success
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
    guard node.state < .invalid else { return false }
    node.setState(.typeCheckRequested)

    // Initialize the function's generic environment.
    guard node.prepareGenericEnv() != nil else {
      node.setState(.invalid)
      return false
    }

    var success = true

    // Resolve the function's explicit captures.
    for capture in node.explicitCaptures {
      success = visit(capture) && success
    }

    // Type check the function's body, if any.
    if let body = node.body {
      success = TypeChecker.check(stmt: body, useSite: node) && success
    }

    node.setState(.typeChecked)
    return success
  }

  func visit(_ node: FunDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  func visit(_ node: CtorDecl) -> Bool {
    return visit(node as BaseFunDecl)
  }

  func visit(_ node: CaptureDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)

    let context = node.type.context
    let funDecl = node.parentDeclSpace!

    let matches = funDecl.parentDeclSpace!.lookup(unqualified: node.name, in: context)
    switch matches.values.count {
    case 1:
      node.capturedDecl = matches.values[0]
      node.type = TypeChecker.contextualize(decl: matches.values[0], from: funDecl)
      node.setState(.typeChecked)
      return true

    case 0:
      context.report(.cannotFind(symbol: node.name, range: node.range))
      node.setState(.invalid)
      return false

    default:
      // FIXME: We forbid explicit capture declarations on overloaded symbols, because we have
      // currently no way to disambiguate them. We could support type signatures on capture
      // declarations to solve this issue.
      context.report(.ambiguousReference(to: node.name, range: node.range))
      node.setState(.invalid)
      return false
    }
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

  mutating func visit(_ node: ProductTypeDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }
    node.setState(.typeCheckRequested)
    var isWellFormed = true

    // Initialize the type's generic environment.
    guard let declEnv = node.prepareGenericEnv() else {
      node.setState(.invalid)
      return false
    }

    // Type check the type's direct members.
    node.updateMemberTables()
    for member in node.members {
      isWellFormed = member.accept(&self) && isWellFormed
    }

    // Type check known extensions.
    let context = node.type.context
    for module in context.modules.values {
      for extDecl in module.extensions(of: node) {
        isWellFormed = extDecl.accept(&self) && isWellFormed
      }
    }

    // Type check the type's conformances.
    let conformingType = node.receiverType
    for (view, conformance) in node.conformanceTable {
      // Typecheck the view declaration if necessary.
      guard view.decl.accept(&self) else {
        node.conformanceTable[view]!.state = .invalid
        continue
      }

      // Assume the conformance holds.
      node.conformanceTable[view]!.state = .checked

      // Build a substitution table mapping the view's generic type parameters to their
      // specialization in the conforming type.
      let viewReceiverType = view.decl.receiverType as! GenericParamType
      var substitutions: [ReferenceBox<ValType>: ValType] = [:]
      substitutions[ReferenceBox(viewReceiverType)] = conformingType

      // Verify that abstract type requirements are satisfied.
      for case let req as AbstractTypeDecl in view.decl.typeMemberTable.values {
        // Look for a declaration in the type member table that shadows the abstract requirement.
        guard let member = node.typeMemberTable[req.name] else {
          context.report(
            .conformanceRequiresMatchingImplementation(
              view: view.decl.name,
              requirement: req.name,
              range: conformance.range ?? node.introRange))

          // Mark the conformance as invalid.
          node.conformanceTable[view]!.state = .invalid
          isWellFormed = false
          continue
        }

        // Bail out if the member is invalid.
        guard member.state != .invalid else {
          node.conformanceTable[view]!.state = .invalid
          isWellFormed = false
          continue
        }

        // Register the member as a witness.
        node.conformanceTable[view]!.entries.append((req, member))
        let key = context.assocType(
          interface: req.instanceType as! GenericParamType,
          base: viewReceiverType)
        substitutions[ReferenceBox(key)] = member.instanceType.dealiased

        if member.state == .invalid {
          node.conformanceTable[view]!.state = .invalid
          isWellFormed = false
        }
      }

      // Bail out if the abstract type requirements were not satisfied.
      guard node.conformanceTable[view]!.state != .invalid else { continue }

      // Verify that value member requirements are satisfied.
      for (name, reqs) in view.decl.valueMemberTable {
        for req in reqs {
          // FIXME: Skip requirements that have a default implementation.

          // Gather the generic parameters of the requirement.
          let reqParams = (req as? GenericDeclSpace)?.genericEnv?.params ?? []

          // Search for a valid candidate.
          let candidates = node.lookup(qualified: name).values.filter({ (decl) -> Bool in
            assert(req !== decl)

            // Discard the candidate if it's not the same kind of construct, preventing a method
            // requirement from being fulfilled by a property, or vice versa.
            guard (req is BaseFunDecl) && (decl is BaseFunDecl) ||
                  (req is VarDecl)     && (decl is VarDecl)
            else { return false }

            // If the requirement has generic parameters, we need to match them with those of the
            // candidate by completing the substitution table.
            var subst = substitutions
            let declParams = (decl as? GenericDeclSpace)?.genericEnv?.params ?? []
            guard reqParams.count == declParams.count else { return false }
            for i in 0 ..< reqParams.count {
              subst[ReferenceBox(reqParams[i])] = declParams[i]
            }

            // Check if the candidate satisfies the requirement.
            let a = req.type.dealiased
            let b = decl.type.dealiased
            return a.matches(with: b, reconcilingWith: { (lhs, rhs) -> Bool in
              subst[ReferenceBox(lhs)] === rhs
            })
          })

          if candidates.count == 1 {
            node.conformanceTable[view]!.entries.append((req, candidates[0]))
          } else {
            context.report(
              .conformanceRequiresMatchingImplementation(
                view: view.decl.name,
                requirement: req.name,
                range: conformance.range ?? node.introRange))

            // Mark the conformance as invalid.
            node.conformanceTable[view]!.state = .invalid
            isWellFormed = false
          }
        }
      }

      // Bail out if the conformance is invalid.
      guard node.conformanceTable[view]!.state != .invalid else { continue }

      // Finally, we must type check the abstract requirements.
      let locator = ConstraintLocator(node)

      // Contextualize the view's `Self` to translate the view's abstract requirements in the
      // context of the conforming type.
      var system = ConstraintSystem()
      let (_, openedParams) = view.decl.genericEnv!.contextualize(
        viewReceiverType,
        from: node,
        processingContraintsWith: { system.insert(prototype: $0, at: locator) })

      // Map opened parameters to their contextal type.
      for (key, member) in substitutions {
        guard let param = key.value as? GenericParamType,
              let opened = openedParams[param]
        else { continue }
        let (tau, _) = declEnv.contextualize(member, from: node)
        system.insert(RelationalConstraint(kind: .equality, lhs: opened, rhs: tau, at: locator))
      }

      // Type check the constraint system.
      var solver = CSSolver(system: system, context: context)
      let result = solver.solve()

      if result.succeeded {
        node.conformanceTable[view]!.state = .checked
      } else {
        result.solution.reportAllErrors(in: context)
        node.conformanceTable[view]!.state = .invalid
        isWellFormed = false
      }
    }

    node.setState(isWellFormed ? .typeChecked : .invalid)
    return isWellFormed
  }

  mutating func visit(_ node: ViewTypeDecl) -> Bool {
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
      isWellFormed = member.accept(&self) && isWellFormed
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
      guard node.state != .invalid else { return false }
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

  mutating func visit(_ node: TypeExtnDecl) -> Bool {
    guard node.state < .typeChecked else { return handleCheckState(node) }

    // Bind the extension to the type it extends.
    guard node.computeExtendedDecl() != nil else { return false }

    // Type-check the extension's members.
    node.setState(.typeCheckRequested)
    var isWellFormed = true
    for member in node.members {
      isWellFormed = member.accept(&self) && isWellFormed
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
      context.report(Diag("circular dependency detected", anchor: node.range))
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
