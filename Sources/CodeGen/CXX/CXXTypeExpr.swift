import FrontEnd

/// A type expression in C++.
public struct CXXTypeExpr: CustomStringConvertible {

  /// The textual represention of the type expression.
  public let description: String

  /// Creates the C++ expression corresponding to `type`, reading relevant information from `ast`,
  /// or returns `nil` if `type` cannot be expressed in C++.
  ///
  /// - Parameters:
  ///   - isReturnType: If `true`, creates a type expression suitable to appear as the return type
  ///     of a function declaration. Otherwise, creates an expression suitable to appear as a local
  ///     variable type annotation.
  public init?(_ type: AnyType, ast: AST, asReturnType isReturnType: Bool = false) {
    switch type.base {
    case AnyType.void:
      description = isReturnType ? "void" : "std::monostate"

    case let type as ProductType:
      // TODO: we should translate this to an "int" struct
      if type == ast.coreType(named: "Int") {
        description = "int"
      } else {
        description = type.name.value
      }

    case let type as ParameterType:
      // TODO: convention
      let bareDescription = CXXTypeExpr(type.bareType, ast: ast)!.description
      description = bareDescription

    default:
      return nil
    }
  }

  /// Creates a C++ type expression from its textual representation.
  public init(_ description: String) {
    self.description = description
  }

}
