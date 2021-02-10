import AST
import Basic

/// A VIL function.
public class Function {

  /// The name of the function.
  public var name: String

  /// The type of the function.
  public var type: FunType

  /// The baisc blocks of the function.
  public var blocks: OrderedDictionary<String, BasicBlock> = [:]

  /// Creates a new VIL function.
  ///
  /// - Parameters:
  ///   - name: The name of the function.
  init(name: String, type: FunType) {
    self.name = name
    self.type = type
  }

}
