import AST
import Basic

/// A binding policy for reifying type expression containing free variables.
enum FreeTypeVarSubstPolicy {

  /// Bind the free variables to the error type.
  case bindToError

  /// Keep the free variables.
  case keep

}

/// The solution returned by a constraint solver.
///
/// This is essentially a table that assigns type variables to concrete types, together with a
/// mapping that keep tracks of selected overloads.
struct Solution {

  /// The score of a solution.
  struct Score: RawRepresentable, Comparable {

    let rawValue: UInt64

    init(rawValue: UInt64) {
      self.rawValue = rawValue
    }

    init(penalities: Int, errorCount: Int) {
      rawValue =
        (UInt64(UInt32(truncatingIfNeeded: penalities))) |
        (UInt64(UInt32(truncatingIfNeeded: errorCount)) << 32)
    }

    static func < (lhs: Score, rhs: Score) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }

    static let worst = Score(rawValue: UInt64.max)

  }

  /// The type bindings that were made to solve the constraint system.
  var bindings: [TypeVar: ValType]

  /// The choice(s) that have been selected for each overload binding constraint solved.
  var overloadChoices: [ConstraintLocator: [ValueDecl]] = [:]

  /// The penalities of the solution.
  var penalities: Int

  /// The errors associated with the solution.
  var errors: [TypeError]

  /// The score of the solution.
  var score: Score {
    return Score(penalities: penalities, errorCount: errors.count)
  }

  /// Reifies the given type, substituting each free variable by its corresponding binding.
  ///
  /// - Parameters:
  ///   - type: The type to reify.
  ///   - substPolicy: The substitution policy to adopt for free type variables.
  func reify(
    _ type: ValType,
    substPolicy: FreeTypeVarSubstPolicy
  ) -> ValType {
    guard type[.hasVariables] else { return type }

    switch type {
    case let type as KindType:
      return reify(type.type, substPolicy: substPolicy).kind

    case let type as BoundGenericType:
      return type.context
        .boundGenericType(
          decl: type.decl,
          args: type.args.map({ reify($0, substPolicy: substPolicy) }))

    case let type as AssocType:
      return type.context
        .assocType(
          interface: type.interface,
          base: reify(type.base, substPolicy: substPolicy))
        .canonical

    case let type as TupleType:
      return type.context
        .tupleType(type.elems.map({ elem in
          TupleType.Elem(
            label: elem.label,
            type: reify(elem.type, substPolicy: substPolicy))
        }))
        .canonical

    case let type as FunType:
      return type.context
        .funType(
          params: type.params.map({ param in
            param.map({ reify($0, substPolicy: substPolicy) })
          }),
          retType: reify(type.retType, substPolicy: substPolicy))
        .canonical

    case let type as FunParamType:
      return type.context
        .funParamType(
          policy: type.policy,
          rawType: reify(type.rawType, substPolicy: substPolicy))

    case let type as AsyncType:
      return type.context
        .asyncType(of: reify(type.base, substPolicy: substPolicy))

    case let type as TypeVar:
      if let binding = bindings[type] {
        return reify(binding, substPolicy: substPolicy)
      }

      // The variable is free in this solution. Apply the specified policy.
      switch substPolicy {
      case .bindToError: return type.context.errorType
      case .keep: return type
      }

    default:
      fatalError("unreachable")
    }
  }

  /// Reports a type error.
  ///
  /// - Parameters:
  ///   - error: A type error.
  ///   - context: The AST conext in which type checking occured.
  func report(_ error: TypeError, in context: Context) {
    switch error {
    case .conflictingTypes(let constraint):
      let lhs = describe(reify(constraint.lhs, substPolicy: .keep))
      let rhs = describe(reify(constraint.rhs, substPolicy: .keep))

      // Compute the diagnostic's message.
      let message: String
      switch constraint.kind {
      case .equality, .oneWayEquality:
        message = "type \(lhs) is not equal to type \(rhs)"
      case .conformance:
        message = "type \(lhs) does not conform to the view \(rhs)"
      case .subtyping, .paramSubtyping:
        message = "type \(lhs) is not a subtype of type \(rhs)"
      case .conversion:
        message = "type \(lhs) is not expressible by type \(rhs) in conversion"
      }

      // Report the diagnostic.
      let anchor = constraint.locator.resolve()
      context.report(Diag(message, anchor: anchor.range))

    case .nonConformingType(let constraint):
      let lhs = describe(reify(constraint.lhs, substPolicy: .keep))
      let rhs = describe(reify(constraint.rhs, substPolicy: .keep))

      let anchor = constraint.locator.resolve()
      context.report(
        Diag("type \(lhs) does not conform to view \(rhs)", anchor: anchor.range))

    case .noViableOverload(let constraint):
      let message = "no viable overload to resolve '\(constraint.declSet[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diag(message, anchor: anchor.range))

    case .multipleOverloads(let constraint, let decls):
      let message = "ambiguous use of '\(decls[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diag(message, anchor: anchor.range))

    default:
      let anchor = error.constraint.locator.resolve()
      context.report(Diag(String(describing: error), anchor: anchor.range))
    }
  }

  /// Reports all the type errors of this solution.
  ///
  /// - Parameter context: The AST conext in which type checking occured.
  func reportAllErrors(in context: Context) {
    // Sort the errors by source location.
    for error in errors.sorted(by: <) {
      report(error, in: context)
    }
  }

  private func describe(_ type: ValType) -> String {
    if type[.isCanonical] {
      return "'\(type)'"
    } else if let type = type as? UnionType {
      let elems = type.elems.map({ String(describing: $0.canonical) }).joined(separator: " | ")
      return "'\(elems)'"
    } else {
      return "'\(type)' (i.e., '\(type.canonical)')"
    }
  }

}

extension Solution {

  static func ~= (lhs: Solution, rhs: Solution) -> Bool {
    guard lhs.bindings.count == rhs.bindings.count else { return false }
    for (key, a) in lhs.bindings {
      guard
        let b = rhs.bindings[key],
        lhs.reify(a, substPolicy: .bindToError) == rhs.reify(b, substPolicy: .bindToError)
      else { return false }
    }
    return true
  }

}

extension Solution.Score: CustomReflectable {

  var customMirror: Mirror {
    return Mirror(
      self,
      children: [
        "penalities": UInt32(truncatingIfNeeded: rawValue),
        "errorCount": UInt32(truncatingIfNeeded: rawValue >> 32),
      ],
      displayStyle: .struct)
  }

}
