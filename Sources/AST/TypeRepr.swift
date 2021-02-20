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

/// The signature of a tuple type (e.g., `(foo: A, bar: B)`).
public final class TupleTypeRepr: TypeRepr {

  public init(elems: [TupleTypeElem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  /// The elements of the tuple.
  public var elems: [TupleTypeElem]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// The element of a tuple type signature.
public struct TupleTypeElem {

  public init(label: String? = nil, sign: TypeRepr, range: SourceRange) {
    self.label = label
    self.sign = sign
    self.range = range
  }

  /// The label of the element.
  public var label: String?

  /// The signature of the element.
  public var sign: TypeRepr

  /// The source range of this element’s textual representation.
  public var range: SourceRange

}

/// The signature of a function type (e.g., `A -> B`).
public final class FunTypeRepr: TypeRepr {

  public init(paramSign: TypeRepr, retSign: TypeRepr, type: ValType, range: SourceRange) {
    self.paramSign = paramSign
    self.retSign = retSign
    self.type = type
    self.range = range
  }

  /// The signature of the function's domain.
  public var paramSign: TypeRepr

  /// The signature of the function's codomain.
  public var retSign: TypeRepr

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a union type (e.g., `A | B`).
public final class UnionTypeRepr: TypeRepr {

  public init(elems: [TypeRepr], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  /// The elements of the union.
  public var elems: [TypeRepr]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a view composition (e.g., `V & U`).
public final class ViewCompTypeRepr: TypeRepr {

  public init(views: [TypeRepr], type: ValType, range: SourceRange) {
    self.views = views
    self.type = type
    self.range = range
  }

  /// The views of the composition.
  public var views: [TypeRepr]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// A type identifier composed of one or several components.
///
/// This protocol abstracts over unqualified and compound type identifiers.
public protocol IdentTypeRepr: TypeRepr {

  /// The components of the identifier.
  var components: [ComponentTypeRepr] { get }

  /// The last component of the identifier.
  var lastComponent: ComponentTypeRepr { get }

}

extension IdentTypeRepr {

  /// Creates either a `CompoundTypeRepr` with the given array, or returns its last element if it
  /// contains just one entry.
  ///
  /// - Parameter components: The components of the identifier.
  public static func create(_ components: [ComponentTypeRepr]) -> IdentTypeRepr {
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

/// A single component in a compund type identifier.
public protocol ComponentTypeRepr: IdentTypeRepr {

  /// The name of the type.
  var name: String { get }

  /// The semantic type realized from this representation.
  var type: ValType { get set }

}

extension ComponentTypeRepr {

  public var components: [ComponentTypeRepr] { [self] }

  public var lastComponent: ComponentTypeRepr { self }

}

/// An simple, unqualified type identifier (e.g., `Int64`).
public final class UnqualTypeRepr: ComponentTypeRepr {

  public init(name: String, type: ValType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  public var name: String

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// An unqualified type identifier with generic arguments (e.g., `Array<Int64>`).
public final class SpecializedTypeRepr: ComponentTypeRepr {

  public init(name: String, args: [TypeRepr], type: ValType, range: SourceRange) {
    self.name = name
    self.args = args
    self.type = type
    self.range = range
  }

  public var name: String

  /// The generic arguments of the type.
  public var args: [TypeRepr]

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.TypeReprResult where V: TypeReprVisitor {
    return visitor.visit(self)
  }

}

/// A type compond identifier, composed of multiple components (e.g., `Builtin::i64`).
///
/// This always refers to the type of the last component. The others serves as explicit qualifiers.
public final class CompoundTypeRepr: IdentTypeRepr {

  public init(components: [ComponentTypeRepr], range: SourceRange) {
    precondition(components.count > 1)
    self.components = components
    self.range = range
  }

  /// The components of the compound.
  public var components: [ComponentTypeRepr]

  public var lastComponent: ComponentTypeRepr {
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
