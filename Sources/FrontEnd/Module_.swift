/// A module within a program representation P.
public struct Module_<P> { // Underscore to avoid confusion with IR.Module

  /// The whole program within which the module resides
  public let program: P

  /// The identity of the module within `program`.
  public let module: ModuleDecl.ID

  /// A memberwise-initialized instance.
  public init(program: P, module: ModuleDecl.ID) {
    self.program = program
    self.module = module
  }

  /// Returns `(program, module)`.
  public consuming func components() -> (program: P, module: ModuleDecl.ID) {
    return (program: program, module: module)
  }

}
