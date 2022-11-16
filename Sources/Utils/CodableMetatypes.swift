import Foundation

private protocol CodableMetatypeWrapperProtocol: AnyObject {
  static var wrappedType: Any.Type { get }
}

private class CodableMetatypeWrapper<T>: CodableMetatypeWrapperProtocol {
  static var wrappedType: Any.Type { T.self }
}

public protocol MetatypeCodable {

}

extension MetatypeCodable {
  static var wrapperName: String { NSStringFromClass(CodableMetatypeWrapper<Self>.self) }
}

public func encode(_ t: MetatypeCodable.Type, to encoder: Encoder) throws {
  var c = encoder.singleValueContainer()
  try c.encode(t.wrapperName)
}

public func decodeMetatype(from decoder: Decoder) throws -> Any.Type {
  let metatypeWrapperName = try decoder.singleValueContainer().decode(String.self)
  let metatypeWrapper: Optional = NSClassFromString(metatypeWrapperName)

  guard let wrapperClass = metatypeWrapper as? CodableMetatypeWrapperProtocol.Type else {
    throw DecodingError.typeMismatch(
      Any.Type.self,
      .init(
        codingPath: decoder.codingPath,
        debugDescription:
          "\(metatypeWrapper.map(String.init(describing:)) ?? "nil") is not a CodableMetatypeWrapper.",
        underlyingError: nil))
  }

  return wrapperClass.wrappedType
}
