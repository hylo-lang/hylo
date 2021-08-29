import AST

/// A Val type that has been lowered to its VIL representation.
///
/// The type of an expression in Val purposely hide a number of implementation details that are
/// only relevant in the context of Val's low-level operational semantics. The most important
/// abstraction relates to the distinction between addressable and non-addressable values. The
/// former are stored in memory and can therefore be referenced by their address, whereas the
/// latter represent notional or temporary values.
///
/// Stored variables, properties and parameter references are represented by pointers to a phyiscal
/// storage internally. Hence, when lowered to VIL, these expressions must be associated with an
/// address type and must be loaded explicitly before being used as an actual value.
///
/// As Val does not have a first-class references, addressable values are not first-class neither:
/// the address of an address is not a legal value in VIL.
public class VILType: CustomStringConvertible {

  /// The high level Val from which this type is lowered.
  public final let valType: ValType

  /// A flag that indicates whether the type is an address type.
  public final let isAddress: Bool

  /// A flag that indicates whether the type is an object type.
  public final var isObject: Bool { !isAddress }

  fileprivate init(valType: ValType, isAddress: Bool) {
    self.valType = valType
    self.isAddress = isAddress
  }

  /// The address variant of this type.
  public var address: VILType {
    return VILType(valType: valType, isAddress: true)
  }

  /// The object variant of this type.
  public var object: VILType {
    return VILType(valType: valType, isAddress: false)
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

  public var description: String {
    var desc = String(describing: valType)
    if desc.contains(" ") {
      desc = "(\(desc))"
    }
    return isAddress ? "*\(desc)" : desc
  }

  static func lower(_ type: ValType) -> VILType {
    switch type.dealiased {
    case let valType as FunType:
      // Lower each parameter and determine its passing convention.
      var paramTypes: [VILType] = []
      var paramConvs: [VILParamConv] = []
      for valType in valType.paramTypeList {
        paramTypes.append(lower(valType))
        paramConvs.append(VILParamConv(for: valType))
      }

      // Lower the return type and determine its passing convention.
      let retType = lower(valType.retType)
      let retConv = VILParamConv(for: valType.retType)

      // Create a VIL function type.
      return VILFunType(
        valType: valType,
        paramTypes: paramTypes,
        paramConvs: paramConvs,
        retType: retType,
        retConv: retConv)

    case let valType as InoutType:
      // Mutable parameters get an address type.
      return VILType(valType: valType.base, isAddress: true)

    case let valType:
      return VILType(valType: valType, isAddress: false)
    }
  }

}

/// The type of a VIL function.
public final class VILFunType: VILType {

  // The VIL type of each parameter.
  public let paramTypes: [VILType]

  /// The passing convention of the function's parameters.
  public let paramConvs: [VILParamConv]

  /// The VIL type of the function's return value.
  public let retType: VILType

  /// The passing convention of the function's return value.
  public let retConv: VILParamConv

  init(
    valType: FunType,
    paramTypes: [VILType],
    paramConvs: [VILParamConv],
    retType: VILType,
    retConv: VILParamConv,
    isAddress: Bool = false
  ) {
    assert(paramTypes.count == paramConvs.count)
    self.paramTypes = paramTypes
    self.paramConvs = paramConvs
    self.retType = retType
    self.retConv = retConv
    super.init(valType: valType, isAddress: isAddress)
  }

  /// The address variant of this type.
  public override var address: VILType {
    return VILFunType(
      valType: valType as! FunType,
      paramTypes: paramTypes,
      paramConvs: paramConvs,
      retType: retType,
      retConv: retConv,
      isAddress: true)
  }

  /// The object variant of this type.
  public override var object: VILType {
    return VILFunType(
      valType: valType as! FunType,
      paramTypes: paramTypes,
      paramConvs: paramConvs,
      retType: retType,
      retConv: retConv,
      isAddress: false)
  }

  public override var description: String {
    var domain = "("
    for i in 0 ..< paramTypes.count {
      if i > 0 { domain.append(", ") }
      domain += "\(paramConvs[i]) \(paramTypes[i])"
    }
    domain += ")"
    let codomain = "\(retConv) \(retType)"

    if isAddress {
      return "*(\(domain) -> \(codomain))"
    } else {
      return "\(domain) -> \(codomain)"
    }
  }

}

/// The convention of a VIL parameter.
public enum VILParamConv: String, CustomStringConvertible {

  /// The parameter is passed directly, by value.
  case val

  /// The parameter is passed indirectly, by reference, and considered immutable.
  ///
  /// The pointee is initialized and may be aliased. Both the caller and the callee agree not to
  /// mutate it for the duration of the call.
  case brw

  /// The parameter is passed indirectly, by reference.
  ///
  /// The pointee is initialized and unaliased. The caller promise not to mutate it for the
  /// duration of the call; the callee promise not to deinitialize it.
  case mut

  /// The parameter is wrapped into an existential container, which is passed directly.
  ///
  /// This requires that the type of the parameter have an existential layout: it should be either
  /// a generic type parameter, a skolem, a view or a view composition.
  case exist

  public init(for type: ValType) {
    switch type {
    case is InoutType: self = .mut
    case is GenericParamType: self = .exist
    default: self = .val
    }
  }

  public var description: String { "@" + rawValue }

}
