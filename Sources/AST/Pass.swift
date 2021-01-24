/// An AST pass that performs some sort of analysis.
public protocol Pass {

  /// The name of the pass.
  static var name: String { get }

  /// Executes the pass on the given module in the specified AST context.
  func run(on module: Module) throws

}
