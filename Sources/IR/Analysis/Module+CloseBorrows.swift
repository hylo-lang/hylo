import Core
import Utils

extension Module {

  /// Inserts `end_borrow` instructions after the last use of each `access` instruction in `f`,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `self` is has gone through access reification. `f` is in `self`.
  public mutating func closeBorrows(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for blockToProcess in blocks(in: f) {
      for i in instructions(in: blockToProcess) {
        close(i, in: f, reportingDiagnosticsTo: &diagnostics)
      }
    }
  }

  /// If `i` is `access` or `project`, make sure it is post-dominated by respectively `end_borrow`
  /// or `end_project`, inserting new instructions as necessary. Otherwise, does nothing.
  private mutating func close(
    _ i: InstructionID, in f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    switch self[i] {
    case let s as Access:
      let region = lifetime(of: .register(i))

      // Delete the borrow if it's never used.
      if region.isEmpty {
        if let decl = s.binding {
          log.insert(.unusedBinding(name: program.ast[decl].baseName, at: s.site))
        }
        removeInstruction(i)
        return
      }

      insertClose(i, atBoundariesOf: region) { (this, site) in
        this.makeEndAccess(.register(i), at: site)
      }

    case is Project:
      let region = lifetime(of: .register(i))
      insertClose(i, atBoundariesOf: region) { (this, site) in
        this.makeEndProject(.register(i), at: site)
      }

    default:
      break
    }
  }

  /// Closes the access formed by `i` at the boundaries of `region`, calling `make` to create the
  /// appropriate lifetiem closer instruction.
  ///
  /// No instruction is inserted at after already existing lifetime closers for `i`.
  private mutating func insertClose<T: LifetimeCloser>(
    _ i: InstructionID, atBoundariesOf region: Lifetime,
    makingInstructionWith make: (inout Self, SourceRange) -> T
  ) {
    for boundary in region.upperBoundaries {
      switch boundary {
      case .after(let u):
        // Skip the insertion if the last user already closes the borrow.
        if let e = self[u] as? T, e.start.instruction == i {
          continue
        }
        let s = make(&self, self[u].site)
        insert(s, after: u)

      case .start(let b):
        let site = instructions(in: b).first.map(default: self[i].site) {
          SourceRange.empty(at: self[$0].site.first())
        }
        let s = make(&self, site)
        insert(s, at: boundary)

      default:
        unreachable()
      }
    }
  }

  /// Returns the lifetime of `operand`.
  private func lifetime(of operand: Operand) -> Lifetime {
    // Nothing to do if the operand has no use.
    guard let uses = uses[operand] else { return Lifetime(operand: operand) }

    // Compute the live-range of the operand.
    var r = liveRange(of: operand, definedIn: operand.block!)

    // Extend the lifetime with that of its borrows.
    for use in uses {
      switch self[use.user] {
      case is LifetimeExtender:
        if let o = result(of: use.user) {
          r = extend(lifetime: r, toCover: lifetime(of: o))
        }

      default:
        continue
      }
    }

    return r
  }

}

extension Diagnostic {

  fileprivate static func unusedBinding(name: Identifier, at site: SourceRange) -> Diagnostic {
    .warning("binding '\(name)' was never used", at: site)
  }

}

/// An instruction that extends the lifetime of all its uses.
private protocol LifetimeExtender {}

extension Access: LifetimeExtender {}

extension OpenUnion: LifetimeExtender {}

extension Project: LifetimeExtender {}

extension ProjectBundle: LifetimeExtender {}

extension SubfieldView: LifetimeExtender {}

extension WrapExistentialAddr: LifetimeExtender {}

/// An instruction that ends the lifetime of a borrow.
private protocol LifetimeCloser: Instruction {

  /// The lifetime being closed.
  var start: Operand { get }

}

extension EndAccess: LifetimeCloser {}

extension EndProject: LifetimeCloser {}
