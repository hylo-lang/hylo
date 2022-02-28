/// A Val type that has been lowered to its VIL representation.
///
/// The type of an expression in Val purposely hide a number of implementation details that are
/// only relevant in the context of Val's low-level operational semantics. The most important
/// abstraction relates to the distinction between addressable and non-addressable types.
///
/// Addressable types typically represent l-values. Such values are stored in memory and require
/// explicit memory operations to convert them to actual values. In contrast, non-adressable types
/// typically represent notional r-values (e.g., the type of the value resulting from evaluating
/// an expression).
///
/// Just like Val, VIL does not have first-class references. It follows that address of an address
/// value cannot be typed in VIL.
public struct VILType {

  /// The high-level Val type from which this type is lowered.
  public let valType: ValType

  /// A Boolean value that indicates whether this type is addressable.
  public let isAddress: Bool

  /// A Boolean value that indicates whether this type is non-addressable.
  public var isObject: Bool { !isAddress }

  fileprivate init(valType: ValType, isAddress: Bool) {
    self.valType = valType
    self.isAddress = isAddress
  }

  /// The addressable variant of this type.
  public var address: VILType {
    return VILType(valType: valType, isAddress: true)
  }

  /// The non-addressable variant of this type.
  public var object: VILType {
    return VILType(valType: valType, isAddress: false)
  }

  /// Given that this type is lowered from a function type, returns the function's parameter list.
  public var params: [FunType.Param]? {
    return (valType as? FunType)?.params
  }

  /// Given that this type is lowered from a function type, returns the lowered type of the
  /// parameter at the specified index.
  public func paramType(at index: Int) -> VILType {
    let param = params![index]
    return .lower(param.type)
  }

  /// Given that this type is lowered from a function type, returns the lowered type of the
  /// function's return value.
  public var retType: VILType? {
    return ((valType as? FunType)?.retType).map(VILType.lower)
  }

  /// A flag that indicates whether the type is existential.
  public var isExistential: Bool { valType.isExistential }

  static func lower(_ type: ValType) -> VILType {
    switch type.canonical {
    case let type as FunParamType:
      switch type.policy {
      case .local, .inout:
        return VILType(valType: type.rawType, isAddress: true)
      case .consuming:
        return VILType(valType: type.rawType, isAddress: false)
      }

    default:
      return VILType(valType: type.canonical, isAddress: false)
    }
  }

}

extension VILType: Hashable {

  public func hash(into hasher: inout Hasher) {
    valType.hash(into: &hasher)
    hasher.combine(isAddress)
  }

}

extension VILType: CustomStringConvertible {

  public var description: String {
    switch valType {
    case is FunType:
      let params = self.params!.enumerated()
        .map({ (i, p) -> String in
          let policy = p.policy.map(String.init(describing:)) ?? "_"
          return "\(policy) \(paramType(at: i))"
        })
        .joined(separator: ", ")
      let desc = "(\(params)) -> \(retType!)"
      return isAddress ? "*(\(desc))" : desc

    default:
      let desc = String(describing: valType)
      if isAddress {
        return desc.contains(" ")
          ? "*(\(desc))"
          : "*\(desc)"
      } else {
        return desc
      }
    }
  }

}
