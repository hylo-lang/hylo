extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled(_ d: AnyDeclID) -> String {
    var output = ""
    var m = Mangler(self)
    m.mangle(d, to: &output)
    return output.assemblySanitized
  }

}
