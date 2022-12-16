/// A constant Val IR value.
public enum Constant: Hashable {

  case builtin(BuiltinFunctionRef)

  case integer(IntegerConstant)

  case function(FunctionRef)

  case poison(PoisonConstant)

  case void

  /// The associated value of this constant.
  public var base: ConstantProtocol {
    switch self {
    case .builtin(let c): return c
    case .integer(let c): return c
    case .function(let c): return c
    case .poison(let c): return c
    case .void: return VoidConstant()
    }
  }

  /// The type of the constant.
  public var type: LoweredType { base.type }

}

extension Constant: CustomStringConvertible {

  public var description: String { String(describing: base) }

}
