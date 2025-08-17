import FrontEnd
import Utils

extension Module {

  /// Inserts `end_borrow` instructions after the last use of each `access` instruction in `f`,
  /// reporting errors and warnings to `diagnostics`.
  ///
  /// - Requires: `self` is has gone through access reification. `f` is in `self`.
  public mutating func closeBorrows(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    for i in self[f].instructions {
      close(i, in: f, reportingDiagnosticsTo: &diagnostics)
    }
  }

  /// If `i` is `access` or `project`, make sure it is post-dominated by respectively `end_borrow`
  /// or `end_project`, inserting new instructions as necessary; does nothing otherwise.
  private mutating func close(
    _ i: InstructionID, in f: Function.ID, reportingDiagnosticsTo log: inout DiagnosticSet
  ) {
    switch self[i, in: f] {
    case let s as Access:
      let region = extendedLiveRange(of: .register(i), in: f)

      // Delete the borrow if it's never used.
      if region.isEmpty {
        if let decl = s.binding {
          log.insert(.warning(unusedBinding: program.ast[decl].baseName, at: s.site))
        }
        self[f].removeInstruction(i)
        return
      }

      insertClose(i, in: f, atBoundariesOf: region) { (this, site) in
        this[f].makeEndAccess(.register(i), at: site)
      }

    case is OpenCapture:
      let region = extendedLiveRange(of: .register(i), in: f)
      insertClose(i, in: f, atBoundariesOf: region) { (this, site) in
        this[f].makeCloseCapture(.register(i), at: site)
      }

    case is OpenUnion:
      let region = extendedLiveRange(of: .register(i), in: f)
      insertClose(i, in: f, atBoundariesOf: region) { (this, site) in
        this[f].makeCloseUnion(.register(i), at: site)
      }

    case is Project:
      let region = extendedLiveRange(of: .register(i), in: f)
      insertClose(i, in: f, atBoundariesOf: region) { (this, site) in
        this[f].makeEndProject(.register(i), at: site)
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
    _ i: InstructionID, in f: Function.ID, atBoundariesOf region: Lifetime,
    makingInstructionWith make: (inout Self, SourceRange) -> T
  ) {
    for boundary in region.upperBoundaries {
      switch boundary {
      case .after(let u):
        // Skip the insertion if the last user already closes the borrow.
        if let e = self[u, in: f] as? T, e.start.instruction == i {
          continue
        }
        let s = make(&self, self[u, in: f].site)
        self[f].insert(s, at: boundary)

      case .start(let b):
        let site = self[f].firstInstruction(in: b).map(default: self[i, in: f].site) {
          SourceRange.empty(at: self[$0, in: f].site.start)
        }
        let s = make(&self, site)
        self[f].insert(s, at: boundary)

      default:
        unreachable()
      }
    }
  }

  /// Returns the extended live-range of `definition`.
  ///
  /// A definition is *live* over an instruction if it is is used by that instruction or may be
  /// used by another instruction in the future. The live-range of a definition `d` is the region
  /// of a function containing all instructions over which `d` is live. The extended live-range of
  /// `d` is its live-range merged with the extended live-ranges of the definitions extending `d`.
  ///
  /// - Note: The definition of an operand `o` isn't part of `o`'s lifetime.
  private func extendedLiveRange(of definition: Operand, in f: Function.ID) -> Lifetime {
    // Nothing to do if the operand has no use.
    guard let uses = self[f].uses[definition] else { return Lifetime(operand: definition) }

    // Compute the live-range of the definition.
    var r = liveRange(of: definition, definedIn: definition.block!, from: f)

    // Extend the lifetime with that of its borrows.
    for use in uses {
      switch self[use.user, in: f] {
      case is LifetimeExtender:
        if let o = self[f].result(of: use.user) {
          r = extend(lifetime: r, toCover: extendedLiveRange(of: o, in: f), in: f)
        }

      case let s as CaptureIn where use.index == 0:
        let p = self[f].provenances(s.target).uniqueElement!
        guard self[p, in: f] is AllocStack else { UNIMPLEMENTED() }
        let u = self[f].uses[p, default: []].first(where: { self[$0.user, in: f] is ReleaseCaptures })!
        return extend(lifetime: r, toInclude: u, in: f)

      default:
        continue
      }
    }

    return r
  }

}

extension Diagnostic {

  fileprivate static func warning(
    unusedBinding n: Identifier, at site: SourceRange
  ) -> Diagnostic {
    .warning("binding '\(n)' was never used", at: site)
  }

}

/// An instruction that extends the lifetime of all its uses.
private protocol LifetimeExtender {}

extension Access: LifetimeExtender {}

extension AdvancedByBytes: LifetimeExtender {}

extension AdvancedByStrides: LifetimeExtender {}

extension OpenCapture: LifetimeExtender {}

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

extension RegionExit: LifetimeCloser {}
