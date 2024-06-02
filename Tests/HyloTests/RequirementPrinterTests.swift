import Utils
import XCTest

@testable import FrontEnd

final class RequirementPrinterTests: XCTestCase {

  func testShowRequirementSystem() throws {
    // This test is merely exercising the code for printing requirement systems, which is meant for
    // debugging the type checker.
    var d = DiagnosticSet()
    let a = try Utils.Host.freestandingLibraryAST.get()
    let p = try TypedProgram(annotating: ScopedProgram(a), reportingDiagnosticsTo: &d)

    let m = a.core.movable.decl
    let n = a.core.copyable.decl
    var s = RequirementSystem()
    s.insert(
      RequirementRule([.parameterType(a[m].receiver), .trait(m)], [.parameterType(a[m].receiver)]),
      orderingTermsWith: { (_, _) in .descending })
    s.insert(
      RequirementRule([.parameterType(a[m].receiver)], [.parameterType(a[n].receiver)]),
      orderingTermsWith: { (_, _) in .descending })

    _ = p.describe(s)
    _ = p.describe(s.rules.first!)
    _ = p.describe(s.rules.first!.lhs)
  }

}
