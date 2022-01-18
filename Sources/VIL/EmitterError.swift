import AST
import Basic

/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  /// A stored property is being moved out of a non-tuple container.
  case moveOfStoredProperty(VarDecl)

  /// Runtime conversion of function types.
  case runtimeFunctionTypeConversion(Expr)

  /// An upcast is being used as an l-value.
  case useOfUpcastAsLValue(Expr)

  /// A r-value is being used as an l-value.
  case useOfRValueAsLValue(Expr)

  func diag() -> Diag {
    switch self {
    case .useOfRValueAsLValue(let expr):
      return .useOfRValueAsLValue(expr: expr)

    default:
      fatalError()
    }
  }

}
