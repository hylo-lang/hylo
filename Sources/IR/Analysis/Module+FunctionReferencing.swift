import Core

extension Module {

  /// Creates a reference to `f`, without specialization.
  public func reference<T>(to f: T) -> FunctionReference {
    if let f = f as? Function.ID {
      return FunctionReference(to: f, in: self)
    } else if let f = f as? AnyDeclID {
      var copy = self
      return FunctionReference(to: f, in: &copy)
    } else {
      preconditionFailure("unsupported function reference")
    }
  }

  /// Creates a reference to `f`, specialized by `specialization` in `scopeOfUse`.
  public func reference<T>(
    to f: T, specializedBy specialization: GenericArguments, in scopeOfUse: AnyScopeID
  ) -> FunctionReference {
    if let f = f as? Function.ID {
      return FunctionReference(to: f, in: self, specializedBy: specialization, in: scopeOfUse)
    } else if let f = f as? AnyDeclID {
      var copy = self
      return FunctionReference(to: f, in: &copy, specializedBy: specialization, in: scopeOfUse)
    } else {
      preconditionFailure("unsupported function reference")
    }
  }

}
