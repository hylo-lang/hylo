extension TypedProgram {

  /// Returns the mangled representation of `d`.
  public func mangled(_ d: AnyDeclID) -> String {
    var output = ""
    var m = Mangler<String>()
    m.mangle(d, of: self, to: &output)
    return output
  }

  /// Decodes and returns the symbol represented by the mangled string `s`, returning `nil` if
  /// decoding failed.
  public func demangle(_ s: String) -> Symbol? {
    var m = Demangler(program: self)
    var i = s[...]
    return m.demangle(from: &i)
  }

}
