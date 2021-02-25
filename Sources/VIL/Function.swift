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
  public func createBasicBlock(arguments: [Value] = []) -> BasicBlock {
    let block = BasicBlock(function: self, arguments: arguments)
    blocks.append(block)
    return block
  }

}
