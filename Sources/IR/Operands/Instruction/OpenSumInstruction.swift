import Core

/// Projects the address of a sum payload, viewed as an instance of a given type.
public struct OpenSumInstruction: Instruction {

  /// The sum whose payload should be projected.
  ///
  /// `container` must denote the address of a sum instance and there shouldn't be used by any
  /// other live `open_sum` instruction.
  public private(set) var container: Operand

  /// The type of the projected payload.
  ///
  /// `payloadType` must be element of `container`'s storage type. If `payloadType.access`
  public let payloadType: AnyType

  /// `true` iff the projected address is used to initializer `container`'s storage.
  ///
  /// If this property's `true`, `container` must refer to uninitialized storage. Otherwise, it
  /// must refer to initialized storage.
  public let isUsedForInitialization: Bool

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    container: Operand, payloadType: AnyType, isUsedForInitialization: Bool, site: SourceRange
  ) {
    self.container = container
    self.payloadType = payloadType
    self.isUsedForInitialization = isUsedForInitialization
    self.site = site
  }

  public var types: [LoweredType] { [.address(payloadType)] }

  public var operands: [Operand] { [container] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    container = new
  }

}

extension OpenSumInstruction: CustomStringConvertible {

  public var description: String {
    if isUsedForInitialization {
      return "open_sum [set] \(container) as \(payloadType)"
    } else {
      return "open_sum \(container) as \(payloadType)"
    }
  }

}

extension Module {

  /// Creates an `open_sum` anchored at `site` that projects the address of `container`'s payload
  /// viewed as an instance of `payloadType`. The projected address is suitable for initialization
  /// iff `isUsedForInitialization` is `true`.
  func makeOpenSum(
    _ container: Operand, as payloadType: AnyType, forInitialization isUsedForInitialization: Bool,
    at site: SourceRange
  ) -> OpenSumInstruction {
    precondition(type(of: container).isAddress)
    precondition(payloadType[.isCanonical])
    return .init(
      container: container,
      payloadType: payloadType,
      isUsedForInitialization: isUsedForInitialization,
      site: site)
  }

}
