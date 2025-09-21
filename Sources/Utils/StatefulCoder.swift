import Foundation

// Workaround for compiler bug in Swift 6.1 https://github.com/swiftlang/swift-foundation/issues/1515#issuecomment-3320126660
// Resolved in Swift 6.2.
#if compiler(>=6.1)
typealias UserInfoValue = any Sendable
#else
typealias UserInfoValue = Any
#endif

/// A key used to access the coding state of encoders/decoders.
private let stateKey = CodingUserInfoKey(rawValue: UUID().uuidString)!

/// An object that flattens values into—or reconstitutes values from—a serialized `Encoding`,
/// maintaining state across the (de)serialization of individual parts.
///
/// - Note: this protocol matches Foundation's `XXXEncoder`/`XXXDecoder` types,
///   which—confusingly—do not themselves conform to the `Encoder`/`Decoder` protocols.
public protocol StatefulCoder {

  /// The complete result of serializing anything with `self`.
  associatedtype Encoding = Data

  /// The storage vehicle for state.
  var userInfo: [CodingUserInfoKey: UserInfoValue] { get set }

}

extension StatefulCoder {

  /// Injects initialState into `self` for use as mutable storage during encoding/decoding.
  public mutating func setState<T: Sendable>(_ initialState: T) {
    userInfo[stateKey] = SharedMutable(initialState)
  }

}

/// An object that flattens values into a serialized `Encoding`, maintaining state across the
/// serialization of individual parts.
///
/// - Note: this protocol matches Foundation's `XXXEncoder` types, which—confusingly—do not
///   themselves conform to the `Encoder` protocol.
public protocol StatefulEncoder: StatefulCoder {

  /// Returns a serialized representation of `subject`.
  func encode<T>(_ subject: T) throws -> Encoding where T: Encodable

}

extension StatefulEncoder {

  /// Returns a serialized representation of `subject`, using `startState` as the initial encoding
  /// state.
  public mutating func encode<T, State: Sendable>(_ value: T, startState: State) throws -> Encoding
  where T: Encodable {
    setState(startState)
    defer { setState(0) }  // Discard state, just in case `self` persists.
    return try encode(value)
  }

}

/// An object that reconstitutes values from a serialized `Encoding`, maintaining state across the
/// deserialization of individual parts.
///
/// - Note: this protocol matches Foundation's `XXXEncoder` types, which—confusingly—do not
///   themselves conform to the `Encoder` protocol.
public protocol StatefulDecoder: StatefulCoder {

  /// Returns the `T` value that was serialized into `archive`.
  func decode<T>(_ type: T.Type, from archive: Encoding) throws -> T where T: Decodable

}

extension StatefulDecoder {

  /// Returns the `T` value that was serialized into `archive`, using `startState` as the initial
  /// decoding state.
  public mutating func decode<T, State: Sendable>(_ type: T.Type, from encodedT: Encoding, startState: State)
    throws -> T where T: Decodable
  {
    setState(startState)
    defer { setState(0) }  // Discard state, just in case `self` persists.
    return try decode(type, from: encodedT)
  }

}

extension Encoder {

  /// Accesses the previously-injected encoding state of type `T`.
  ///
  /// - Precondition: `self` is the product of some `StatefulEncoder` `e` on which `e.setState(s)`
  ///   has been called, where `s` has type `T`.
  public subscript<T: Sendable>(state state: T.Type) -> T {
    get { (userInfo[stateKey]! as! SharedMutable<T>).read{$0} }
    nonmutating set { (userInfo[stateKey]! as! SharedMutable<T>).modify{ $0 = newValue } }
    nonmutating _modify { 
      var stored = (userInfo[stateKey]! as! SharedMutable<T>).read{ $0 }
      yield &stored
      (userInfo[stateKey]! as! SharedMutable<T>).modify{ $0 = stored }
    }
  }

}

extension Decoder {

  /// Accesses the previously-injected decoding state of type `T`.
  ///
  /// - Precondition: `self` is the product of some `StatefulDecoder` `e` on which `e.setState(s)`
  ///   has been called, where `s` has type `T`.
  public subscript<T: Sendable>(state state: T.Type) -> T {
    get { (userInfo[stateKey]! as! SharedMutable<T>).read{ $0 } }
    nonmutating set { (userInfo[stateKey]! as! SharedMutable<T>).modify { $0 = newValue } }
    nonmutating _modify { 
      var stored = (userInfo[stateKey]! as! SharedMutable<T>).read(applying: { $0 })
      yield &stored
      (userInfo[stateKey]! as! SharedMutable<T>).modify{ $0 = stored }
    }
  }

}

// Conformances for known types.

extension JSONEncoder: StatefulEncoder {}
extension JSONDecoder: StatefulDecoder {}
extension PropertyListEncoder: StatefulEncoder {}
extension PropertyListDecoder: StatefulDecoder {}
