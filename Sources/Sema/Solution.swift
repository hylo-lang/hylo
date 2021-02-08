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
    guard type.props.contains(.hasVariables) else { return type }

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

}
