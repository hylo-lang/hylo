/// A directed graph, represented thread dependencies.
struct ThreadDependencyGraph {

  /// The dependencies of each thread.
  private(set) var dependencies: [VirtualThread.ID: [VirtualThread.ID]] = [:]

  /// The reverse dependencies of each thread.
  private(set) var reverseDependencies: [VirtualThread.ID: [VirtualThread.ID]] = [:]

  init() {}

  /// Makes `dependent` a dependent of `supplier`.
  ///
  /// - Parameters:
  ///   - dependent: A thread identifier.
  ///   - supplier: Another thread identifier.
  mutating func insertDependency(dependent: VirtualThread.ID, supplier: VirtualThread.ID) {
    guard dependencies[dependent]?.firstIndex(of: supplier) == nil else { return }
    dependencies[dependent, default: []].append(supplier)
    reverseDependencies[supplier, default: []].append(dependent)
  }

  /// Removes the dependency between `dependent` and `supplied`.
  ///
  /// - Parameters:
  ///   - dependent: A thread identifier.
  ///   - supplier: Another thread identifier.
  mutating func removeDependency(dependent: VirtualThread.ID, supplier: VirtualThread.ID) {
    guard let i = dependencies[dependent]?.firstIndex(of: supplier) else { return }

    dependencies[dependent]!.remove(at: i)
    if dependencies[dependent]!.isEmpty { dependencies[dependent] = nil }

    let j = reverseDependencies[supplier]!.firstIndex(of: dependent)!
    reverseDependencies[supplier]!.remove(at: j)
    if reverseDependencies[supplier]!.isEmpty { reverseDependencies[supplier] = nil }
  }

}
