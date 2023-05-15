import Core

/// A program lowered to Val IR.
public struct LoweredProgram {

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
    for k in modules.keys {
      modules[k]!.applyPass(p)
    }
  }

  // MARK: Mangling

  /// Returns the mangled name of `t`.
  public func mangle(_ t: AnyType) -> String {
    switch t.base {
    case let u as ProductType:
      return mangle(u)
    case let u as TraitType:
      return mangle(u)
    default:
      fatalError()
    }
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: ProductType) -> String {
    syntax.debugName(decl: t.decl)
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: TraitType) -> String {
    syntax.debugName(decl: t.decl)
  }

  /// Returns the mangled name of `t`.
  public func mangle(_ t: WitnessTable) -> String {
    var result = "__VWT" + mangle(t.witness)
    for c in t.conformances {
      result.append(mangle(^c.concept))
      result.append(String(describing: c.source.rawValue))
    }
    return result
  }

  /// Returns the name of `f`.
  public func mangle(_ f: Function.ID) -> String {
    switch f.value {
    case .lowered(let d):
      return syntax.debugName(decl: d)
    case .loweredSubscript(let d):
      return syntax.debugName(decl: d)
    case .monomorphized(let f, let a):
      return mangle(f) + "<\(list: a.values)>"
    case .synthesized(let d, let t):
      return "\(d)\(t)"
    case .existentialized:
      fatalError("not implemented")
    }
  }

}
