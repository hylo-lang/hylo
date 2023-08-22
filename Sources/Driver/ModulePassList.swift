import ArgumentParser
import IR

/// A list of IR module passes.
struct ModulePassList: ExpressibleByArgument {

  /// The elements in the list.
  let elements: [IR.ModulePass]

  public init?(argument: String) {
    var elements: [IR.ModulePass] = []
    for p in argument.split(separator: " ") {
      guard let q = ModulePass.init(rawValue: String(p)) else { return nil }
      elements.append(q)
    }

    if elements.isEmpty {
      return nil
    } else {
      self.elements = elements
    }
  }

}
