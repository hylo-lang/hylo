import AST
import Basic

/// A VIL function.
public class Function {

  /// The name of the function.
  public let name: String

  /// The type of the function.
  public let type: FunType

  /// The arguments of the function.
  public private(set) var arguments: [ArgumentValue]

  /// The basic blocks of the function.
  public var blocks: [BasicBlock] = []

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  init(name: String, type: FunType) {
    self.name = name
    self.type = type

    // Create the function's arguments.
    self.arguments = []
    if let tupleType = type.paramType as? TupleType {
      arguments = tupleType.elems.map({ (elem) -> ArgumentValue in
        ArgumentValue(type: elem.type, function: self)
      })
    } else {
      arguments = [ArgumentValue(type: type.paramType, function: self)]
    }
  }

  /// Creates a new base block at the end of the function.
  public func createBasicBlock(arguments: [Value] = []) -> BasicBlock {
    let block = BasicBlock(function: self, arguments: arguments)
    blocks.append(block)
    return block
  }

  /// Dumps a textual representation of the function to the given output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    stream.write("vilfun \(name) : \(type) {\n")
    stream.write("}\n")
  }

}
