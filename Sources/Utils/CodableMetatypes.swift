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

#if os(iOS) || os(OSX) || os(watchOS) || os(tvOS)
extension MetatypeCodable {
  /// The name by which CodableMetatypeWrapper<Self>.self can be reconstituted.
  fileprivate static var wrapperName: String {
    NSStringFromClass(CodableMetatypeWrapper<Self>.self)
  }
}

/// Returns the CodableMetatypeWrapper<T> with the given name.
private func metatypeWrapperClass(named name: String) -> CodableMetatypeWrapperProtocol.Type? {
  guard let c = NSClassFromString(name) else { return nil }
  return c as? CodableMetatypeWrapperProtocol.Type
}
#else
extension MetatypeCodable {
  /// The name by which CodableMetatypeWrapper<Self>.self can be reconstituted.
  fileprivate static var wrapperName: String {
    "Utils.CodableMetatypeWrapper<\(String(reflecting: Self.self))>" //_typeName(CodableMetatypeWrapper<Self>.self)
  }
}

/// Returns the CodableMetatypeWrapper<T> with the given name.
private func metatypeWrapperClass(named name: String) -> CodableMetatypeWrapperProtocol.Type? {
  _typeByName(name) as? CodableMetatypeWrapperProtocol.Type
}
#endif

/// Serializes `t` into `destination`.
public func encode(_ t: MetatypeCodable.Type, to destination: Encoder) throws {
  var c = destination.singleValueContainer()
  try c.encode(t.wrapperName)
  print("******* encoding name:", t.wrapperName)
}

/// Deserializes the metatype of any MetatypeCodable type from `source`.
public func decodeMetatype(from source: Decoder) throws -> MetatypeCodable.Type {
  let metatypeWrapperName = try source.singleValueContainer().decode(String.self)

  print("******* reconstituting", _typeByName(metatypeWrapperName) ?? Never.self)
  guard let wrapperClass = metatypeWrapperClass(named: metatypeWrapperName) else {
    throw DecodingError.typeMismatch(
      MetatypeCodable.self,
      .init(
        codingPath: source.codingPath,
        debugDescription: "\(metatypeWrapperName) is not a CodableMetatypeWrapper.",
        underlyingError: nil))
  }

  return wrapperClass.wrappedType
}
