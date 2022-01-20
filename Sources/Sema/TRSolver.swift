import AST
import Basic

/// A type requirement solver.
///
/// This solver is responsible for validating type requirements in generic signatures. Unlike the
/// constraint solver, it operates on generic type parameters rather than type variables, and does
/// not try to infer concrete types when the constraint system is underspecified.
struct TRSolver {

  public init() {}

  private var bindings: [BindingKey: BindingValue] = [:]

  /// Compute the equivalence classes inferred after solving a set of type requirements.
  ///
  /// This method should be called only after `solve(typeReqs:from:)` returned successfully.
  func computeEquivalenceClasses(env: GenericEnv) -> EquivalenceClassSet {
    var classes: [[ValType]] = []
    var count = 0

    for case .parameter(let entryA) in bindings.keys {
      guard case .parameter(let entryB) = representative(of: .parameter(entryA)) else {
        fatalError("not implemented yet")
      }

      if let i = classes.firstIndex(where: { class_ in class_.contains(entryB) }) {
        if entryA != entryB {
          classes[i].append(entryA)
          count += 1
        }
      } else {
        if entryA != entryB {
          classes.append([entryA, entryB])
          count += 2
        } else {
          classes.append([entryB])
          count += 1
        }

        if case .type(let type) = bindings[.parameter(entryB)] {
          classes[classes.count - 1].append(type)
          count += 1
        }
      }
    }

    return EquivalenceClassSet(classes: classes, numberOfEntries: count)
  }

  /// Solves a collection of type requirements.
  mutating func solve(typeReqs: [TypeReq], from useSite: DeclSpace) -> Bool {
    var success = true

    for req in typeReqs where req.kind == .equality {
      // Realize each operand's signature.
      let lhs = req.lhs.realize(unqualifiedFrom: useSite)
      let rhs = req.rhs.realize(unqualifiedFrom: useSite)

      // Skip the requirement if either of the types has an error; otherwise, unify them.
      guard !lhs[.hasErrors] && !rhs[.hasErrors] else { continue }

      let result = unify(lhs, rhs)
      switch result {
      case .success:
        continue

      case .inequal:
        lhs.context.report(.conflictingEqualityRequirement(range: req.range))
        success = false

      case .recursive:
        lhs.context.report(.recursiveEqualityRequirement(range: req.range))
        success = false
      }
    }

    return success
  }

  private mutating func unify(_ lhs: ValType, _ rhs: ValType) -> UnificationResult {
    // The the types are obviously equivalent, we're done.
    if lhs == rhs { return .success }

    switch (lhs, rhs) {
    case (let lhs as GenericParamType, let rhs as GenericParamType):
      return unify(.parameter(lhs), .parameter(rhs))

    case (let lhs as GenericParamType, _):
      // The right operand is bound to a non-generic type.
      let a = representative(of: .parameter(lhs))
      switch bindings[a] {
      case nil:
        // Just bind the left operand.
        // FIXME: Run the occurs check.
        bindings[a] = .type(rhs)
        return .success

      case .type(let type):
        // The left operand is already bound; the bound with the right operand.
        return unify(type, rhs)

      case .variable, .shape:
        fatalError("not implemented yet")
      }

    case (_, let rhs as GenericParamType):
      return unify(rhs, lhs)

    case (let lhs as TupleType, let rhs as TupleType):
      guard lhs[.hasTypeParams] || rhs[.hasTypeParams] else {
        return lhs == rhs
          ? .success
          : .inequal
      }

      guard lhs.elems.count == rhs.elems.count else { return .inequal }
      for (a, b) in zip(lhs.elems, rhs.elems) {
        guard a.label == b.label else { return .inequal }
        let result = unify(a.type, b.type)
        guard result == .success else { return result }
      }
      return .success

    case (let lhs as FunType, let rhs as FunType):
      guard lhs[.hasTypeParams] || rhs[.hasTypeParams] else {
        return lhs == rhs
          ? .success
          : .inequal
      }

      guard lhs.params.count == rhs.params.count else { return .inequal }
      for (a, b) in zip(lhs.params, rhs.params) {
        guard a.label == b.label else { return .inequal }
        let result = unify(a.type, b.type)
        guard result == .success else { return result }
      }

      return unify(lhs.retType, rhs.retType)

    case (let lhs as FunParamType, let rhs as FunParamType):
      guard lhs.policy == rhs.policy else { return .inequal }
      return unify(lhs.rawType, rhs.rawType)

    default:
      // Unification failed.
      return .inequal
    }
  }

  private mutating func unify(_ lhs: BindingKey, _ rhs: BindingKey) -> UnificationResult {
    // Find the representative of each equivalent class.
    let a = representative(of: lhs)
    let b = representative(of: rhs)

    // Don't do anything if the representative of each class are stricly identical.
    if a == b { return .success }

    // Unify the equialence classes.
    switch (bindings[a], bindings[b]) {
    case (nil, _):
      // If the left side is not constrained to any shape, simply choose the right side as the
      // new representatative.
      bindings[a] = BindingValue(b)
      return .success

    case (.shape, nil):
      // The left side is already constrained to a shape, but right side is not.
      bindings[b] = BindingValue(a)
      return .success

    case (.shape(let lshape), .shape(let rshape)):
      // Both sides are constrained to a shape. We must unify them.
      for (key, lval) in lshape {
        guard let rval = rshape[key] else { continue }
        let result = unify(lval, rval)
        guard result == .success else { return result }
      }

      // Choose the right side as the representative of the equivalence class.
      bindings[a] = BindingValue(b)
      bindings[b] = .shape(lshape.merging(rshape, uniquingKeysWith: { _, rhs in rhs }))
      return .success

    case (.type(let ltype), .type(let rtype)):
      // Both sides re bound types. We must unify them.
      return unify(ltype, rtype)

    default:
      // Unification failed.
      return .inequal
    }
  }

  private func representative(of key: BindingKey) -> BindingKey {
    var walked = key
    while true {
      switch bindings[walked] {
      case .type(let type as GenericParamType):
        walked = .parameter(type)
      case .variable(let i):
        walked = .variable(i)
      default:
        return walked
      }
    }
  }

}

fileprivate enum UnificationResult {

  case success

  case inequal

  case recursive

}

fileprivate enum BindingKey: Hashable {

  case parameter(GenericParamType)

  case variable(Int)

}

fileprivate enum BindingValue {

  case type(ValType)

  case variable(Int)

  case shape([GenericParamType: BindingKey])

  init(_ key: BindingKey) {
    switch key {
    case .parameter(let param): self = .type(param)
    case .variable(let i)     : self = .variable(i)
    }
  }

}
