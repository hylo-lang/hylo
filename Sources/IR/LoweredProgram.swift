import Core

/// A program lowered to Val IR.
public struct LoweredProgram {

  /// The high-level form of the program.
  public let syntax: TypedProgram

  /// A map from module ID to its lowered form.
  public let modules: [ModuleDecl.ID: IR.Module]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [ModuleDecl.ID : IR.Module]) {
    precondition(modules.values.elementCount(where: { $0.entryFunctionID != nil }) <= 1)
    self.syntax = syntax
    self.modules = modules
  }

  /// The identity of the entry module.
  public var entry: ModuleDecl.ID? {
    modules.first(where: { $0.value.entryFunctionID != nil })?.key
  }

}
