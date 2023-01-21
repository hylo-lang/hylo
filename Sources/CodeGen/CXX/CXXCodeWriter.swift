/// Class used to write the output CXX code from the given CXX AST.
public struct CXXCodeWriter {

  /// Initializes the current object.
  public init() {}

  /// Write the CXX header content for the given module to the given text stream.
  public func writeHeaderCode<Target: TextOutputStream>(
    _ module: CXXModule, into target: inout Target
  ) {
    let code = module.emitHeader()
    target.write(code)
  }

  /// Write the CXX source content for the given module to the given text stream.
  public func writeSourceCode<Target: TextOutputStream>(
    _ module: CXXModule, into target: inout Target
  ) {
    let code = module.emitSource()
    target.write(code)
  }

}
