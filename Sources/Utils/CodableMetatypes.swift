import Foundation

/// A marker by which we can identify instances of `CodableMetatypeWrapper<T>`.
private protocol CodableMetatypeWrapperProtocol: AnyObject {
  static var wrappedType: MetatypeCodable.Type { get }
}

/// A class type whose name gets serialized as a representative of `T.self`.
private class CodableMetatypeWrapper<T: MetatypeCodable>: CodableMetatypeWrapperProtocol {
  /// The metatype represented by `Self`
  static var wrappedType: MetatypeCodable.Type { T.self }
}

/// A category of types whose metatype values can be (de-)serialized.
public protocol MetatypeCodable {}

extension MetatypeCodable {
  /// The reconstitutable name of CodableMetatypeWrapper<Self>.
  fileprivate static var wrapperName: String {
    NSStringFromClass(CodableMetatypeWrapper<Self>.self)
  }
}

/// Serializes `t` into `deistnation`.
public func encode(_ t: MetatypeCodable.Type, to destination: Encoder) throws {
  var c = destination.singleValueContainer()
  try c.encode(t.wrapperName)
}

/// Deserializes the metatype of any MetatypeCodable type from `source`.
public func decodeMetatype(from source: Decoder) throws -> MetatypeCodable.Type {
  let metatypeWrapperName = try source.singleValueContainer().decode(String.self)
  let metatypeWrapper: Optional = NSClassFromString(metatypeWrapperName)

  guard let wrapperClass = metatypeWrapper as? CodableMetatypeWrapperProtocol.Type else {
    throw DecodingError.typeMismatch(
      MetatypeCodable.self,
      .init(
        codingPath: source.codingPath,
        debugDescription:
          "\(metatypeWrapper.map(String.init(describing:)) ?? "nil") is not a CodableMetatypeWrapper.",
        underlyingError: nil))
  }

  return wrapperClass.wrappedType
}


#if !os(iOS) && !os(OSX) && !os(watchOS) && !os(tvOS)
private func NSStringFromClass(_ aClass: AnyClass) -> String {
  let classNameString = String(reflecting: aClass)
  /*
  if let renamed = mapFromSwiftClassNameToObjCName[classNameString] {
    return renamed
  }
  */
  let aClassName = classNameString._bridgeToObjectiveC()

  return String(describing: aClassName)
}

private func NSClassFromString(_ aClassName: String) -> AnyClass? {
  let aClassNameWithPrefix = aClassName
  return _typeByName(aClassNameWithPrefix) as? AnyClass
}
#endif
