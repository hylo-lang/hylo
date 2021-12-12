/// An instruction operand.
public struct Operand: Equatable, Hashable {

  /// A wrapper around a `Constant` existential.
  @usableFromInline internal class _ConstantBox: Equatable, Hashable {

    var value: Constant

    init(_ value: Constant) {
      self.value = value
    }

    @usableFromInline
    func hash(into hasher: inout Hasher) {
      value.hash(into: &hasher)
    }

    @usableFromInline
    static func == (lhs: Operand._ConstantBox, rhs: Operand._ConstantBox) -> Bool {
      return lhs.value.isEqual(to: rhs.value)
    }

  }

  /// The internal representation of an operand.
  @usableFromInline internal enum _Repr: Equatable, Hashable {

    // 10 bytes
    case inst(InstIndex)

    // 8 bytes
    case argument(ArgValue)

    // 8 bytes
    case constant(_ConstantBox)

  }

  @usableFromInline internal var _repr: _Repr

  /// Creates an instruction operand.
  public init(_ inst: InstIndex) {
    _repr = .inst(inst)
  }

  /// Creates an argument operand.
  public init(_ argument: ArgValue) {
    _repr = .argument(argument)
  }

  /// Creates a constant operand.
  public init(_ constant: Constant) {
    _repr = .constant(_ConstantBox(constant))
  }

  /// Attempts to unwraps the operand as an instruction index.
  public var inst: InstIndex? {
    if case .inst(let index) = _repr {
      return index
    } else {
      return nil
    }
  }

  /// Attempts to unwraps the operand as an argument identifier.
  public var argument: ArgValue? {
    if case .argument(let value) = _repr {
      return value
    } else {
      return nil
    }
  }

  /// Attempts to unwraps the operand as an argument.
  public var constant: Constant? {
    if case .constant(let box) = _repr {
      return box.value
    } else {
      return nil
    }
  }

}
