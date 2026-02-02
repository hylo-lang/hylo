import FrontEnd

/// An InstructionTransformer that uses dictionaries to map instructions.
///
/// Does not change types.
struct DictionaryInstructionTransformer: InstructionTransformer {

  /// A map from block in `source` to its corresponding block in `result`.
  var rewrittenBlocks: [Block.ID: Block.ID] = [:]

  /// A map from instruction in `source` to its corresponding instruction in `result`.
  var rewrittenInstructions: [InstructionID: InstructionID] = [:]

  /// A map from block parameter in `source` to its corresponding block parameter in `result`.
  var rewrittenParameters: [Operand: Operand] = [:]

  /// A map from registers in `source` to its corresponding register `result`.
  var rewrittenRegisters: [Operand: Operand] = [:]

  /// Returns `t`, as we are not changing types.
  func transform(_ t: AnyType, in ir: inout IR.Program) -> AnyType {
    return t
  }

  /// Returns a transformed copy of `o`.
  func transform(_ o: Operand) -> Operand {
    switch o {
    case .constant(let c):
      return .constant(c)
    case .parameter(let b, let i):
      return rewrittenParameters[o] ?? .parameter(rewrittenBlocks[b]!, i)
    case .register(let s):
      if let rewritten = rewrittenRegisters[o] {
        return rewritten
      }
      return .register(rewrittenInstructions[s]!)
    }
  }
  /// Returns a transformed copy of `o`.
  func transform(_ o: Operand, in ir: inout IR.Program) -> Operand {
    self.transform(o)
  }

  /// Returns a transformed copy of `b`.
  func transform(_ b: Block.ID, in ir: inout IR.Program) -> Block.ID {
    return rewrittenBlocks[b]!
  }

}

extension IR.Program {

  /// Transforms with `t` the instructions `instructions` from function `f` in module `m`,
  /// inserting the transformed instructions in the corresponding blocks in function `g`.
  ///
  /// This uses `t.rewrittenBlocks` to find the corresponding blocks in `g`.
  mutating func rewrite<C: Sequence>(
    _ instructions: C, in f: Function.ID, from m: Module.ID,
    transformedBy t: inout DictionaryInstructionTransformer, to g: Function.ID
  ) where C.Element == InstructionID {
    let source = modules[m]![f]
    for i in instructions {
      let b = rewrittenBlocks(source.block(of: i), in: f, from: m, transformedBy: &t, to: g)
      let j = rewrite(
        i, in: f, from: m, transformedBy: &t, at: .end(of: b), targeting: g, in: m)
      t.rewrittenInstructions[i] = j
    }
  }

  /// Returns the rewritten block corresponding to `b` from function `f` in module `m`,
  /// creating it if needed in function `g`.
  private mutating func rewrittenBlocks(
    _ b: Block.ID, in f: Function.ID, from m: Module.ID,
    transformedBy t: inout DictionaryInstructionTransformer, to g: Function.ID
  ) -> Block.ID {
    let r = t.rewrittenBlocks[b]
    if r == nil {
      let s = modules[m]![f][b]
      let new = modules[m]![g].appendBlock(in: s.scope)
      t.rewrittenBlocks[b] = new
      return new
    } else {
      return r!
    }
  }

}
