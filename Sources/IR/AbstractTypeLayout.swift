import Core
import FrontEnd

/// The abstract layout of a type, describing the relative offsets of its stored properties.
public struct AbstractTypeLayout {

  /// The name and type of a stored property.
  public typealias StoredProperty = (name: String?, type: AnyType)

  /// The program in which `type` is defined.
  private let program: TypedProgram

  /// The type of which this instance is the abstract layout.
  public let type: AnyType

  /// The stored properties in `type`, in the order in which they are laid out.
  public let properties: [TupleType.Element]

  /// Creates the abstract layout of `t` defined in `p`.
  public init<T: TypeProtocol>(of t: T, definedIn p: TypedProgram) {
    self.program = p
    self.type = ^t
    self.properties = program.storage(of: self.type)
  }

  /// Accesses the layout of the stored property at given `offset`.
  ///
  /// - Requires: `offset` is a valid offset in this instance.
  public subscript(offset: Int) -> AbstractTypeLayout {
    AbstractTypeLayout(of: properties[offset].type, definedIn: program)
  }

  /// Accesses the layout of the stored property at given `offsets`.
  public subscript<S: Sequence>(offsets: S) -> AbstractTypeLayout where S.Element == Int {
    offsets.reduce(self, { (l, offset) in l[offset] })
  }

  /// Accesses the layout of the stored property named `n` or `nil` if no such property exists.
  public subscript(n: String) -> AbstractTypeLayout? {
    offset(of: n).map({ self[$0] })
  }

  /// Returns the offset of the stored property named `n` or `nil` if no such property exists.
  public func offset(of n: String) -> Int? {
    properties.firstIndex(where: { $0.label == n })
  }

}

extension AbstractTypeLayout: Hashable {

  public func hash(into hasher: inout Hasher) {
    type.hash(into: &hasher)
  }

  public static func == (l: Self, r: Self) -> Bool {
    l.type == r.type
  }

}
