import Basic

/// A pattern.
public protocol Pattern: Node {

  /// The type of the pattern.
  var type: ValType { get set }

  /// Returns the named patterns contained within this pattern.
  var namedPatterns: [NamedPattern] { get }

  /// If the pattern binds a single variable without destructuring, returns its declaration.
  var singleVarDecl: VarDecl? { get }

  /// Indicates whether the pattern is refutable.
  var isRefutable: Bool { get }

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A pattern visitor.
  func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor

}

/// A pattern which binds an identifier.
public final class NamedPattern: Pattern {

  public var range: SourceRange

  public var type: ValType

  /// The variable declaration to which the name refers.
  public var decl: VarDecl

  public init(decl: VarDecl, type: ValType, range: SourceRange) {
    self.decl = decl
    self.type = type
    self.range = range
  }

  public var namedPatterns: [NamedPattern] { [self] }

  public var singleVarDecl: VarDecl? { decl }

  public var isRefutable: Bool { false }

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}

/// A comma-separated list of zero or more patterns, enclosed in parentheses.
public final class TuplePattern: Pattern {

  /// An element in a tuple pattern.
  public struct Elem {

    /// The label of the element.
    public var label: String?

    /// The pattern of the element.
    public var pattern: Pattern

    /// The source range of this element's textual representation.
    public var range: SourceRange

    public init(label: String?, pattern: Pattern, range: SourceRange) {
      self.label = label
      self.pattern = pattern
      self.range = range
    }

  }

  public var range: SourceRange

  public var type: ValType

  /// The elements of the tuple.
  public var elems: [Elem]

  public init(elems: [Elem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  public var namedPatterns: [NamedPattern] {
    return Array(elems.map({ el in el.pattern.namedPatterns }).joined())
  }

  public var singleVarDecl: VarDecl? {
    guard elems.count == 1 else { return nil }
    return elems[0].pattern.singleVarDecl
  }

  public var isRefutable: Bool {
    return (elems.count == 1) && elems[0].pattern.isRefutable
  }

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}

/// A pattern that introduces new variables.
///
/// This pattern alters the semantics of its sub-pattern. Nested name patterns create new variable
/// bindings, instead or referring to existing declarations.
public final class BindingPattern: Pattern {

  public var range: SourceRange

  public var type: ValType

  /// A flag indicating whether the declared variables are mutable.
  public var isMutable: Bool

  /// The sub-pattern.
  public var subpattern: Pattern

  /// The signature of the pattern.
  public var sign: Sign?

  /// The source range of the `val` or `var` keyword at the start of the pattern.
  public var keywordRange: SourceRange

  public init(
    isMutable: Bool,
    subpattern: Pattern,
    sign: Sign?,
    type: ValType,
    keywordRange: SourceRange,
    range: SourceRange
  ) {
    self.isMutable = isMutable
    self.subpattern = subpattern
    self.sign = sign
    self.type = type
    self.keywordRange = keywordRange
    self.range = range
  }

  public var namedPatterns: [NamedPattern] { subpattern.namedPatterns }

  public var singleVarDecl: VarDecl? { subpattern.singleVarDecl }

  public var isRefutable: Bool {
    return (sign != nil) || subpattern.isRefutable
  }

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}

/// A pattern that matches an arbitrary value, but does not bind it to a name.
public final class WildcardPattern: Pattern {

  public var range: SourceRange

  public var type: ValType

  public init(type: ValType, range: SourceRange) {
    self.type = type
    self.range = range
  }

  public var namedPatterns: [NamedPattern] { [] }

  public var singleVarDecl: VarDecl? { nil }

  public var isRefutable: Bool { false }

  public func accept<V>(_ visitor: V) -> V.PatternResult where V: PatternVisitor {
    return visitor.visit(self)
  }

}
