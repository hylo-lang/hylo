extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled<T: DeclID>(_ d: T) -> String {
    var output = ""
    var m = Mangler(self)
    m.mangle(d, to: &output)
    return output.assemblySanitized
  }

  /// Returns the mangled representation of `t`.
  public func mangled<T: TypeProtocol>(_ t: T) -> String {
    var output = ""
    var m = Mangler(self)
    m.mangle(AnyType(t), to: &output)
    return output.assemblySanitized
  }

}
