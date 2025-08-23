import FrontEnd
import Utils

/// A collection of basic blocks representing a lowered function.
public struct Function {

  /// A collection of blocks with stable identities.
  public typealias Blocks = DoublyLinkedList<Block>

  /// A collection of instructions with stable identities.
  public typealias Instructions = DoublyLinkedList<Instruction>

  /// `true` iff the function implements a subscript.
  public let isSubscript: Bool

  /// The site in the source code to which the function corresponds..
  public let site: SourceRange

  /// The linkage of the function.
  public let linkage: Linkage

  /// The generic (a.k.a., compile-time) parameters of the function.
  public let genericParameters: [GenericParameterDecl.ID]

  /// The run-time parameters of the function.
  public let inputs: [Parameter]

  /// The type of the function's output.
  public let output: AnyType

  /// The blocks in the function.
  public var blocks: Blocks

  /// The instructions in the function.
  public var instructions: Instructions = []

  /// The block associated with each instruction.
  internal var blockForInstruction: [InstructionID: Block.ID] = [:]

  /// The def-use chains of the values in this module.
  public var uses: [Operand: [Use]] = [:]

  /// Accesses the basic block at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: Blocks.Address) -> Block {
    get { blocks[address] }
    _modify { yield &blocks[address] }
  }

  /// Accesses the block identified by `b`.
  public subscript(b: Block.ID) -> Block {
    _read { yield blocks[b.address] }
    _modify { yield &blocks[b.address] }
  }

  /// Accesses the instruction identified by `i`.
  public subscript(i: InstructionID) -> Instruction {
    _read { yield instructions[i.address] }
    _modify { yield &instructions[i.address] }
  }

  /// Returns the control flow graph of `self`.
  func cfg() -> ControlFlowGraph {
    var result = ControlFlowGraph()
    for source in blocks.indices {
      guard let s = self[blocks[source].last!] as? Terminator else { continue }
      for target in s.successors {
        result.define(source.address, predecessorOf: target.address)
      }
    }

    return result
  }

}
