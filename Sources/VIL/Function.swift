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
        // Inout parameters get an address type.
        if let inoutType = elem.type as? InoutType {
          return ArgumentValue(type: .address(inoutType.base), function: self)
        } else {
          return ArgumentValue(type: .object(elem.type), function: self)
        }
      })
    } else {
      arguments = [ArgumentValue(type: .object(type.paramType), function: self)]
    }
  }

  /// Creates a new base block at the end of the function.
  public func createBasicBlock(arguments: [Value] = []) -> BasicBlock {
    let block = BasicBlock(function: self, arguments: arguments)
    blocks.append(block)
    return block
  }

}
