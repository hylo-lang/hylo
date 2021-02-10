import AST

/// A VIL emitter.
///
/// This class is the entry point to VIL code generation phase, which lowers a type checked module
/// declaration to a VIL module.
public struct Emitter {

  /// The VIL builder used by the emitter.
  public let builder: Builder

  public init(builder: Builder) {
    self.builder = builder
  }

  public func emit(decl: Decl) {
  }

}
