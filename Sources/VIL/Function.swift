import AST
import Basic

/// A VIL function.
public class Function {

  /// The mangled name of the function.
  public let name: String

  /// The VIL type of the function.
  public let type: VILFunType

  /// The basic blocks of the function.
  public var blocks: [BasicBlock] = []

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  ///   - type: The unapplied type of the function.
  ///   - paramConv: The passing convention of the function's parameters.
  ///   - retConv: The passing convention of the funtion's return value.
  init(name: String, type: VILFunType) {
    self.name = name
    self.type = type
  }

  /// Creates a new base block at the end of the function.
  ///
  /// - Parameters:
  ///   - arguments: The arguments of the basic block.
  ///   - successor: The basic block before which the new block should be inserted.
  public func createBasicBlock(
    arguments: [Value] = [],
    before successor: BasicBlock? = nil
  ) -> BasicBlock {
    let block = BasicBlock(function: self, arguments: arguments)

    if let s = successor,
       let i = blocks.firstIndex(where: {$0 === s })
    {
      blocks.insert(block, at: i)
    } else {
      blocks.append(block)
    }

    return block
  }

}
