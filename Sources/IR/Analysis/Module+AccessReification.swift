import Core
import DequeModule
import Utils

/// Implementation notes
/// ====================
///
/// This transformation pass implements the following algorithm:
///
/// 1. Gather all reifiable access instructions in the function in a set `W`.
/// 2. While an instruction `i` can be popped from `W`:
///   a. Initialize a set `R` with the weakest capability offered by `i`.
///   b. Gather the requested capabilities on `i` from all its uses into `R`.
///   c. If `R` is the singleton `{k}`, reify `i` as an access providing `k`. Otherwise, insert `i`
///      back into `W`.
///   d. Go back to step 2.
///
/// The algorithm converges because a reifiable `i` is put back into `W` if and only if all its
/// uses are other reifiable accesses. Since a definition cannot use itself, dependencies between
/// reifiable accesses form a forest whose leaves do not have reifiable user. Iterating over the
/// elements in `W` eliminates those leaves, forming a smaller forest until `W` is empty.
///
/// Step 2b is implemented by `Module.forEachClient(of:)` and `Module.requests(_:)`. The former
/// iterates over all the uses of an instruction, traversing instructions that compute derived
/// addresses (.e.g., `subfield_view`). The latter returns the set of requested capabilities for
/// each user. As well-formed accesses to memory must be done via `borrow` or `load`, there's a
/// fairly small number of instructions to consider.
///
/// Note that `move` are assumed to request `inout` only as assignment should be preferred over
/// initialization. This assumption is sound because if the reifiable access only provides `set`,
/// then `move` will be rewritten as initialization during the DI pass.

extension Module {

  /// Replace uses of `access` instructions by either `borrow` or `load` depending on the weakest
  /// capability required on the access, reporting errors and warnings to `diagnostics`.
  ///
  /// This pass uses def-use chains to identify the weakest capability required on an `access`. If
  /// it is `sink`, the instruction is removed and its uses are replaced be uses of its source.
  /// Otherwise, a `borrow` instruction is substitutde for the access.
  ///
  /// - Requires: `f` is in `self`.
  public mutating func reifyAccesses(in f: Function.ID, diagnostics: inout DiagnosticSet) {
    var work: Deque<InstructionID> = []
    for i in blocks(in: f).map(instructions(in:)).joined() {
      guard let s = self[i] as? ReifiableAccess else { continue }

      // Algorithm assumes no access or projection can offer `set` and `let` without also offering
      // `inout` so that `move` instruction can be treated as though they only requested `inout`.
      let k = s.capabilities
      precondition(!k.isSuperset(of: [.set, .let]) || k.contains(.inout))
      work.append(i)
    }

    while let i = work.popFirst() {
      let s = self[i] as! ReifiableAccess
      let available = s.capabilities
      var requested: AccessEffectSet = [available.weakest!]

      forEachClient(of: i) { (u) in
        let r = requests(u).intersection(available)
        if let k = r.uniqueElement {
          requested = [requested.strongest(including: k)]
        } else {
          requested.formUnion(r)
        }
      }

      if let k = requested.uniqueElement {
        reify(i, as: k)
      } else {
        work.append(i)
      }
    }
  }

  private func requests(_ u: Use) -> AccessEffectSet {
    switch self[u.user] {
    case let t as AccessInstruction:
      return t.capabilities
    case let t as BorrowInstruction:
      return [t.capability]
    case is LoadInstruction:
      return [.sink]
    case is MoveInstruction:
      return [.inout]
    case is ProjectBundleInstruction:
      return requests(projectBundle: u)
    default:
      unreachable()
    }
  }

  private func requests(projectBundle u: Use) -> AccessEffectSet {
    let s = self[u.user] as! ProjectBundleInstruction
    let t = s.parameters[u.index]
    return (t.access == .yielded) ? s.capabilities : [t.access]
  }

  /// Calls `action` on the uses of a capability on the access at the origin of `i`.
  private func forEachClient(of i: InstructionID, _ action: (Use) -> Void) {
    guard let uses = self.uses[.register(i, 0)] else { return }
    for u in uses {
      switch self[u.user] {
      case is OpenSumInstruction, is SubfieldViewInstruction, is AdvancedByBytesInstruction:
        forEachClient(of: u.user, action)
      default:
        action(u)
      }
    }
  }

  /// Replaces the uses of `i` with uses of an instruction providing the same access but only
  /// with capability `k`.
  private mutating func reify(_ i: InstructionID, as k: AccessEffect) {
    switch self[i] {
    case is AccessInstruction:
      reify(access: i, as: k)
    case is ProjectBundleInstruction:
      reify(projectBundle: i, as: k)
    default:
      unreachable()
    }
  }

  /// Replaces the uses of `i`, which is an `access` instruction, with uses of a borrow or load
  /// instruction for capability `k`.
  private mutating func reify(access i: InstructionID, as k: AccessEffect) {
    let s = self[i] as! AccessInstruction
    if k == .sink {
      replaceUses(of: .register(i, 0), with: s.source, in: i.function)
      removeInstruction(i)
    } else {
      let reified = makeBorrow(k, from: s.source, correspondingTo: s.binding, at: s.site)
      replace(i, with: reified)
    }
  }

  private mutating func reify(projectBundle i: InstructionID, as k: AccessEffect) {
    let s = self[i] as! ProjectBundleInstruction

    // Generate the proper instructions to prepare the projection's arguments.
    // Note: the relative order between `access` reified as `load` is not preserved.
    var arguments = s.operands
    for a in arguments.indices where s.parameters[a].access == .yielded {
      let b: Instruction =
        (k == .sink)
        ? makeLoad(arguments[a], at: s.site)
        : makeBorrow(k, from: arguments[a], at: s.site)
      arguments[a] = insert(b, before: i)[0]
    }

    let o = RemoteType(k, s.projection.bareType)
    let reified = makeProject(
      o, applying: s.variants[k]!, parameterizedBy: s.bundle.arguments, to: arguments, at: s.site)
    replace(i, with: reified)
  }

}

/// An instruction denoting an access to reify.
private protocol ReifiableAccess {

  /// The capabilities available on the access.
  var capabilities: AccessEffectSet { get }

}

extension AccessInstruction: ReifiableAccess {}

extension ProjectBundleInstruction: ReifiableAccess {}
