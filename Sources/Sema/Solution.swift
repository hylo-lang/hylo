import AST
import Basic

/// The solution returned by a constraint solver.
struct Solution {

  /// The type bindings that were made to solve the constraint system.
  let bindings: [TypeVar: ValType]

  /// The penalities of the solution.
  let penalities: Int

  /// The errors associated with the solution.
  let errors: [TypeError]

  /// The score of the solution.
  var score: Score {
    return Score(penalities: penalities, errorCount: errors.count)
  }

  /// Reifies the given type, substituting each free variable by its corresponding concrete type
  /// in the type assumptions.
  func reify(_ type: ValType) -> ValType {
    guard type.props.contains(.hasVariables) else { return type }

    switch type {
    case let type as KindType:
      return reify(type.type).kind

    case let type as TupleType:
      return type.context
        .tupleType(type.elems.map({ elem in
          TupleType.Elem(label: elem.label, type: reify(elem.type))
        }))
        .canonical

    case let type as FunType:
      return type.context
        .funType(paramType: reify(type.paramType), retType: reify(type.retType))
        .canonical

    case let type as TypeVar:
      return bindings[type].map(reify(_:)) ?? type

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
