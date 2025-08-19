import Foundation
import FrontEnd
import Utils

extension Module {

  /// Analyzes projection in `f`, populating `projectionSkeletons` with the relevant information for expanding projections.
  mutating func analyzeProjection(
    _ f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    if !self[f].isSubscript {
      return
    }

    if let p = try? ProjectionSkeleton(f, in: self, reportingDiagnosticsTo: &log) {
      projectionSkeletons[f] = p
    }
  }

}
