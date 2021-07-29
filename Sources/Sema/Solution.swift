import AST
import Basic

/// A binding policy for reifying type expression containing free variables.
enum FreeTypeVarBindingPolicy {

  /// Bind the free variables to the error type.
  case bindToErrorType

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

    init(rawValue: UInt64) {
      self.rawValue = rawValue
    }

    init(penalities: Int, errorCount: Int) {
      rawValue =
        (UInt64(UInt32(truncatingIfNeeded: penalities))) |
        (UInt64(UInt32(truncatingIfNeeded: errorCount)) << 32)
    }

    let rawValue: UInt64

    static func < (lhs: Score, rhs: Score) -> Bool {
      return lhs.rawValue < rhs.rawValue
    }

    static var worst = Score(rawValue: UInt64.max)

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
  ///   - freeVariablePolicy: The binding policy to adopt for free type variables.
  func reify(
    _ type: ValType,
    freeVariablePolicy: FreeTypeVarBindingPolicy
  ) -> ValType {
    guard type.hasVariables else { return type }

    switch type {
    case let type as KindType:
      return reify(type.type, freeVariablePolicy: freeVariablePolicy).kind

    case let type as BoundGenericType:
      return type.context
        .boundGenericType(
          decl: type.decl,
          args: type.args.map({ reify($0, freeVariablePolicy: freeVariablePolicy) }))

    case let type as TupleType:
      return type.context
        .tupleType(type.elems.map({ elem in
          TupleType.Elem(
            label: elem.label,
            type: reify(elem.type, freeVariablePolicy: freeVariablePolicy))
        }))
        .canonical

    case let type as FunType:
      return type.context
        .funType(
          paramType: reify(type.paramType, freeVariablePolicy: freeVariablePolicy),
          retType: reify(type.retType, freeVariablePolicy: freeVariablePolicy))
        .canonical

    case let type as AsyncType:
      return type.context
        .asyncType(of: reify(type.base, freeVariablePolicy: freeVariablePolicy))

    case let type as InoutType:
      return type.context
        .inoutType(of: reify(type.base, freeVariablePolicy: freeVariablePolicy))

    case let type as TypeVar:
      if let binding = bindings[type] {
        return reify(binding, freeVariablePolicy: freeVariablePolicy)
      }

      // The variable is free in this solution. Apply the specified policy.
      switch freeVariablePolicy {
      case .bindToErrorType : return type.context.errorType
      case .keep            : return type
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
      let lhs = reify(constraint.lhs, freeVariablePolicy: .keep)
      let rhs = reify(constraint.rhs, freeVariablePolicy: .keep)

      // Compute the diagnostic's message.
      let message: String
      switch constraint.kind {
      case .equality, .oneWayEquality:
        message = "type '\(lhs)' is not equal to type '\(rhs)'"
      case .conformance:
        message = "type '\(lhs)' does not conform to the view '\(rhs)'"
      case .subtyping:
        message = "type '\(lhs)' is not a subtype of type '\(rhs)'"
      case .conversion:
        message = "type '\(lhs)' is not expressible by type '\(rhs)' in conversion"
      }

      // Report the diagnostic.
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    case .nonConformingType(let constraint):
      let lhs = reify(constraint.lhs, freeVariablePolicy: .keep)
      let rhs = reify(constraint.rhs, freeVariablePolicy: .keep)
      assert(rhs is ViewType)

      let anchor = constraint.locator.resolve()
      context.report(
        Diagnostic("type '\(lhs)' does not conform to view '\(rhs)'", anchor: anchor.range))

    case .noViableOverload(let constraint):
      let message = "no viable overload to resolve '\(constraint.declSet[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    case .multipleOverloads(let constraint, let decls):
      let message = "ambiguous use of '\(decls[0].name)'"
      let anchor = constraint.locator.resolve()
      context.report(Diagnostic(message, anchor: anchor.range))

    default:
      let anchor = error.constraint.locator.resolve()
      context.report(Diagnostic(String(describing: error), anchor: anchor.range))
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

}
