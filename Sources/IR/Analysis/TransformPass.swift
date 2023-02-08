import Core

/// An IR transformation pass.
public protocol TransformPass {

  /// Runs the IR pass represented by this instance on `function` in `module`, reporting warnings
  /// and errors into `diagnostics`.
  mutating func run(function: Function.ID, module: inout Module, diagnostics: inout Diagnostics)

}
