import Core
import FrontEnd
import Utils

extension Module {

  /// Ensures that addressors from `self` are compiled as regular functions, reading definitions from `ir`.
  public mutating func simplifyAddressors(in ir: IR.Program) {
    let addressors = collectAddressors(from: ir)
    let _ = addressors

    // Transform addressors into regular functions.
    for f in addressors {
      simplifyAddressor(f, in: ir)
    }

    // Transform calls to addressors.
    for f in functions.keys {
      let fd = functions[f]!

      // Ignore declarations without definition.
      if fd.entry == nil { continue }

      simplifyAddressorCalls(from: f, in: ir)
    }
  }

  /// Returns the set of addressors from `ir`.
  private func collectAddressors(from ir: IR.Program) -> Set<Function.ID> {
    var addressors: Set<Function.ID> = []
    for k in functions.keys {
      let f = functions[k]!

      // Ignore declarations without definition.
      if f.entry == nil { continue }

      if isAddressor(k, in: ir) {
        addressors.insert(k)
      }
    }
    return addressors
  }

  /// `true` iff `f` is an addressor.
  private func isAddressor(_ f: Function.ID, in ir: IR.Program) -> Bool {
    let fd = functions[f]!
    if !fd.isSubscript {
      return false
    }
    // TODO: multi-branch case.
    var yieldSeen = false
    var isGoodAccessor = true
    for i in blocks(in: f).map(instructions(in:)).joined() {
      let instruction = self[i]
      if instruction is Yield {
        yieldSeen = true
        continue
      } else if !yieldSeen {
        continue
      }

      switch instruction {
      case is EndAccess, is EndProject, is DeallocStack, is MarkState, is Return:
        continue
      default:
        isGoodAccessor = false
        break
      }
    }
    return isGoodAccessor
  }

  /// Transforms the projection into a regular function call.
  private mutating func simplifyAddressor(_ f: Function.ID, in ir: IR.Program) {
    let fd = functions[f]!
    let _ = fd
  }

  /// Transforms the calls to addressors into regular function calls.
  private mutating func simplifyAddressorCalls(from f: Function.ID, in ir: IR.Program) {
    // TODO
  }
}
