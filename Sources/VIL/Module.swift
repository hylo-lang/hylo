import Basic

/// A VIL module.
///
/// A VIL module is essentially a collection of VIL functions that have been lowered from a module
/// declaration.
public final class Module {

  /// The module's identifier.
  public let id: String

  /// The functions in the module.
  public var functions: [String: Function] = [:]

  public init(id: String) {
    self.id = id
  }

  /// Dumps a textual representation of the module.
  public func dump() {
    var stream = StandardOutput()
    dump(to: &stream)
  }

  /// Dumps a textual representation of the module to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("// module \(id)\n")

    // Dump the functions in the module.
    for function in functions.values {
      function.dump(to: &stream)
    }
  }

}
