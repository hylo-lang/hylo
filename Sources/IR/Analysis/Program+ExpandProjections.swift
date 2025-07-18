import Foundation
import FrontEnd
import Utils

extension IR.Program {

  /// Expands projections into regular function calls for the modules in `self`.
  public mutating func expandProjections() {
    for m in modules.values {
      lowerProjections(in: m)
    }
    for m in modules.values {
      lowerProjectCalls(in: m)
    }
  }

  /// Lowers the projections in `m` into a pair of ramp+slide functions.
  private mutating func lowerProjections(in m: Module) {
    // TODO
  }

  /// Lowers the `Project` instructions in `m` into regular function calls to projection's ramp & slide functions.
  private mutating func lowerProjectCalls(in m: Module) {
    // TODO
  }

}
