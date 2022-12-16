import Core

/// An IR transformation pass.
public protocol TransformPass {

  /// The name of the pass.
  static var name: String { get }

  /// The diagnostics collected during the last run of the pass.
  var diagnostics: [Diagnostic] { get }

  /// Runs the pass on the specified function in `module` and returns whether it succeeded.
  mutating func run(function functionID: Function.ID, module: inout Module) -> Bool

}
