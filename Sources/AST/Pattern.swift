import Basic

/// A pattern.
public protocol Pattern: Node {

  /// The type of the pattern.
  var type: ValType { get set }

  /// Returns the named patterns contained within this pattern.
  var namedPatterns: [NamedPattern] { get }

  /// If the pattern binds a single variable without destructuring, returns its declaration.
  var singleVarDecl: VarDecl? { get }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A pattern visitor.
  func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor

}

/// A pattern which binds an identifier.
public final class NamedPattern: Pattern {

  public init(decl: VarDecl, type: ValType, range: SourceRange) {
    self.decl = decl
    self.type = type
    self.range = range
  }

  /// The variable declarations to which the name refers.
  public var decl: VarDecl

  public var type: ValType

  public var namedPatterns: [NamedPattern] { [self] }

  public var singleVarDecl: VarDecl? { decl }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}

/// A comma-separated list of zero or more patterns, enclosed in parentheses.
public final class TuplePattern: Pattern {

  public init(elems: [Elem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  /// The elements of the tuple.
  public var elems: [Elem]

  public var type: ValType

  public var namedPatterns: [NamedPattern] {
    return Array(elems.map({ el in el.pattern.namedPatterns }).joined())
  }

  public var singleVarDecl: VarDecl? {
    guard elems.count == 1 else { return nil }
    return elems[0].pattern.singleVarDecl
  }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

  /// An element in a tuple pattern.
  public struct Elem {

    public init(label: String?, pattern: Pattern, range: SourceRange) {
      self.label = label
      self.pattern = pattern
      self.range = range
    }

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: Pattern

    /// The source range of this element's textual representation.
    public var range: SourceRange

  }

}

/// A pattern that matches an arbitrary value, but does not bind it to a name.
public final class WildcardPattern: Pattern {

  public init(type: ValType, range: SourceRange) {
    self.type = type
    self.range = range
  }

  public var type: ValType

  public var namedPatterns: [NamedPattern] { [] }

  public var singleVarDecl: VarDecl? { nil }

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}
