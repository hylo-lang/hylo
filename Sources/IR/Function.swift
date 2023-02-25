import Core
import Utils
import Foundation

/// A collection of basic blocks representing a lowered function.
public struct Function {

  /// A collection of blocks with stable identities.
  public typealias Blocks = DoublyLinkedList<Block>

  /// The profile of a IR function input.
  public typealias Input = (convention: AccessEffect, type: LoweredType)

  /// The mangled name of the function.
  public let name: String

  /// The debug name of the function, if any.
  public let debugName: String?

  /// The position in source code at which the function is anchored.
  public let anchor: SourcePosition

  /// The linkage of the function.
  public let linkage: Linkage

  /// The types of the function's parameters.
  public let inputs: [Input]

  /// The type of the function's output.
  public let output: LoweredType

  /// The blocks in the function.
  ///
  /// The first block of the array is the function's entry.
  public internal(set) var blocks: Blocks

  /// The entry of the function.
  public var entry: Block? { blocks.first }

  /// Accesses the basic block at `address`.
  ///
  /// - Requires: `address` must be a valid address in `self`.
  public subscript(_ address: Blocks.Address) -> Block {
    get { blocks[address] }
    _modify { yield &blocks[address] }
  }

  /// The control flow graph of `self`.
  var cfg: ControlFlowGraph {
    var result = ControlFlowGraph()

    for source in blocks.indices {
      switch blocks[source.address].instructions.last {
      case let instruction as BranchInstruction:
        result.define(source.address, predecessorOf: instruction.target.address)
      case let instruction as CondBranchInstruction:
        result.define(source.address, predecessorOf: instruction.targetIfTrue.address)
        result.define(source.address, predecessorOf: instruction.targetIfFalse.address)
      default:
        break
      }
    }

    return result
  }

}

extension Function: CustomStringConvertible {

  public var description: String { "@\(name)" }

}


extension Function {

  /// The global identity of an IR function.
  public struct ID: Hashable {

    /// The value of a function IR identity.
    private enum Value: Hashable {

      /// The identity of a lowered Val function or method variant.
      case val(AnyNodeID)

      /// The identity of a synthesized IR function.
      case synthetic(UUID)

    }

    /// The value of this identity.
    private let value: Value

    /// Creates the identity of the lowered form of `f`.
    public init(_ f: FunctionDecl.ID) {
      self.value = .val(AnyNodeID(f))
    }

  }

}

extension Function.ID: CustomStringConvertible {

  public var description: String {
    switch value {
    case .val(let id):
      return "\(id).lowered"
    case .synthetic(let uuid):
      return uuid.description
    }
  }

}
