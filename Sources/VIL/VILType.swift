import AST

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
  /// function's return value.
  public var retType: VILType? {
    return ((valType as? FunType)?.retType).map(VILType.lower)
  }

  /// Returns this vil type contextualized in the given environment.
  ///
  /// - Parameters:
  ///   - env: A generic environment.
  ///   - useSite: The declaration space in which the type is being used.
  public func contextualized(in env: GenericEnv, from useSite: DeclSpace) -> VILType {
    let (contextualType, _) = env.contextualize(valType, from: useSite)
    let vilType = VILType.lower(contextualType)
    return isAddress
      ? vilType.address
      : vilType.object
  }

  /// A flag that indicates whether the type is existential.
  public var isExistential: Bool { valType.isExistential }

  static func lower(_ type: ValType) -> VILType {
    return VILType(valType: type.dealiased, isAddress: false)
  }

}

extension VILType: CustomStringConvertible {

  public var description: String {
    var desc = String(describing: valType)
    if desc.contains(" ") {
      desc = "(\(desc))"
    }
    return isAddress ? "*\(desc)" : desc
  }

}
