extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled(_ d: AnyDeclID) -> String {
    var output = ""
    var m = Mangler<String>()
    m.mangle(d, of: self, to: &output)
    return output.assemblySanitized
  }

}
