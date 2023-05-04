/// An instruction operand.
public enum Operand {

  /// The `index`-th result of `instruction`.
  case register(InstructionID, Int)

  /// The `index`-th parameter of `block`.
  case parameter(Block.ID, Int)

  /// A constant value.
  case constant(any Constant)

  /// The void constant.
  public static let void: Operand = .constant(VoidConstant())

  /// Returns a built-in Boolean constant.
  public static func i1(_ v: Bool) -> Operand {
    .constant(IntegerConstant(v ? 1 : 0, bitWidth: 1))
  }

  /// The ID of the function in which the operand is defined, if any.
  public var function: Function.ID? {
    block?.function
  }

  /// The ID of the block in which the operand is defined, if any.
  public var block: Block.ID? {
    switch self {
    case .register(let instruction, _):
      return Block.ID(instruction.function, instruction.block)
    case .parameter(let block, _):
      return block
    case .constant(_):
      return nil
    }
  }

  /// The ID of the instruction that produces this operand, if any.
  public var instruction: InstructionID? {
    switch self {
    case .register(let instruction, _):
      return instruction
    default:
      return nil
    }
  }

}

extension Operand: Equatable {

  public static func == (l: Self, r: Self) -> Bool {
    switch (l, r) {
    case (.register(let l0, let l1), .register(let r0, let r1)):
      return (l0 == r0) && (l1 == r1)
    case (.parameter(let l0, let l1), .parameter(let r0, let r1)):
      return (l0 == r0) && (l1 == r1)
    case (.constant(let l0), .constant(let r0)):
      return l0.equals(r0)
    default:
      return false
    }
  }

}

extension Operand: Hashable {

  public func hash(into hasher: inout Hasher) {
    switch self {
    case .register(let a, let b):
      a.hash(into: &hasher)
      b.hash(into: &hasher)
    case .parameter(let a, let b):
      a.hash(into: &hasher)
      b.hash(into: &hasher)
    case .constant(let a):
      a.hash(into: &hasher)
    }
  }

}

extension Operand: CustomStringConvertible {

  public var description: String {
    switch self {
    case .register(let i, let k):
      return "%\(i)#\(k)"
    case .parameter(let b, let k):
      return "%\(b)#\(k)"
    case .constant(let c):
      return "\(c)"
    }
  }

}
