/// A constant Val IR value.
public enum Constant: Hashable {

  case builtin(BuiltinFunctionRef)

  case integer(IntegerConstant)

  case function(FunctionRef)

  case poison(PoisonConstant)

  /// The associated value of this constant.
  public var base: ConstantProtocol {
    switch self {
    case let .builtin(c):   return c
    case let .integer(c):   return c
    case let .function(c):  return c
    case let .poison(c):    return c
    }
  }

  /// The type of the constant.
  public var type: IRType { base.type }

}

extension Constant: CustomStringConvertible {

  public var description: String { String(describing: base) }

}
