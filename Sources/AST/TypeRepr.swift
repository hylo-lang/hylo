import Basic

/// The syntactic representation of a type in source code.
public protocol TypeRepr: Node {

  /// The semantic type realized from this representation.
  var type: ValType { get }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A type representation visitor.
  func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor

}

/// A type identifier composed of one or several components.
///
/// This protocol abstracts over unqualified and compound type identifiers.
public protocol IdentTypeRepr: TypeRepr {

  /// The components of the identifier.
  var components: [UnqualTypeRepr] { get }

  /// The last component of the identifier.
  var lastComponent: UnqualTypeRepr { get }

}

extension IdentTypeRepr {

  /// Creates either a `CompoundTypeRepr` with the given array, or returns its last element if it
  /// contains just one entry.
  ///
  /// - Parameter components: The components of the identifier.
  public static func create(_ components: [UnqualTypeRepr]) -> IdentTypeRepr {
    precondition(!components.isEmpty)
    if components.count == 1 {
      return components.first!
    } else {
      return CompoundTypeRepr(
        components: components,
        range: components.first!.range.lowerBound ..< components.last!.range.upperBound)
    }
  }

}

/// An simple, unqualified type identifier (e.g., `Int64`).
public final class UnqualTypeRepr: IdentTypeRepr {

  public init(name: String, type: ValType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  /// The name of the type.
  public var name: String

  /// The type referred by the identifier.
  public var type: ValType

  public var components: [UnqualTypeRepr] { [self] }

  public var lastComponent: UnqualTypeRepr { self }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// A type compond identifier, composed of multiple components (e.g., `Builtin::Int64`).
///
/// This always refers to the type of the last component. The others serves as explicit qualifiers.
public final class CompoundTypeRepr: IdentTypeRepr {

  public init(components: [UnqualTypeRepr], range: SourceRange) {
    precondition(components.count > 1)
    self.components = components
    self.range = range
  }

  /// The components of the compound.
  public var components: [UnqualTypeRepr]

  public var lastComponent: UnqualTypeRepr {
    return components[components.count - 1]
  }

  public var type: ValType {
    return lastComponent.type
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}
