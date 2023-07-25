import Core

/// A program lowered to Val IR.
public struct Program {

  /// The high-level form of the program.
  public let syntax: TypedProgram

  /// A map from module ID to its lowered form.
  public private(set) var modules: [ModuleDecl.ID: IR.Module]

  /// Creates an instance with the given properties.
  public init(syntax: TypedProgram, modules: [ModuleDecl.ID: IR.Module]) {
    precondition(modules.values.elementCount(where: { $0.entryFunction != nil }) <= 1)
    self.syntax = syntax
    self.modules = modules
  }

  /// The identity of the entry module.
  public var entry: ModuleDecl.ID? {
    modules.first(where: { $0.value.entryFunction != nil })?.key
  }

  /// Applies `p` to the modules in `self`.
  public mutating func applyPass(_ p: ModulePass) {
    for k in syntax.ast.modules {
      modules[k]!.applyPass(p, in: self)
    }
  }

  /// Returns the module containing the unique definition of `f`.
  public func module(defining f: Function.ID) -> ModuleDecl.ID {
    switch f.value {
    case .lowered(let d):
      return syntax.module(containing: syntax[d].scope)
    case .loweredSubscript(let d):
      return syntax.module(containing: syntax[d].scope)
    case .monomorphized:
      fatalError("not implemented")
    case .existentialized(let i):
      return module(defining: i)
    case .synthesized:
      fatalError("not implemented")
    }
  }

  // MARK: Mangling

  /// Returns the mangled name of `t`.
  public func mangle(_ t: WitnessTable) -> String {
    var result = "__VWT" + syntax.mangled(t.witness)
    for c in t.conformances {
      result.append(syntax.mangled(c.concept))
    }
    return result
  }

  /// Returns the mangled name of `f`.
  public func mangle(_ f: Function.ID) -> String {
    switch f.value {
    case .lowered(let d):
      return syntax.mangled(d)
    case .loweredSubscript(let d):
      return syntax.mangled(d)
    case .monomorphized(let f, let a):
      return mangle(f) + "<\(list: a.values)>"
    case .synthesized(let s):
      return "\(s.kind)\(s.type)"
    case .existentialized:
      fatalError("not implemented")
    }
  }

}
