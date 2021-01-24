import Basic

/// A node representing the declaration of one or more entities.
public protocol Decl: Node {
}

/// A type or a value declaration.
public protocol TypeOrValueDecl: Decl {

  /// The name of the declaration.
  var name: String { get }

  /// The type of the declaration.
  var type: ValType { get }

  /// A flag indicating whether the declaration is overloadable.
  var isOverloadable: Bool { get }

}

/// A type declaration.
public protocol TypeDecl: TypeOrValueDecl, DeclScope {

  /// The type of instances of the declared type.
  var instanceType: ValType { get }

}

extension TypeDecl {

  public var instanceType: ValType {
    return (type as! KindType).type
  }

  public var isOverloadable: Bool { true }

}

/// A named declaration.
public protocol ValueDecl: TypeOrValueDecl {

  /// The type of the declaration.
  var type: ValType { get set }

}

/// A declaration that consists of a pattern and an optional initializer for the variables declared
/// in this pattern.
public final class PatternBindingDecl: Decl {

  public init(
    isMutable: Bool,
    pattern: Pattern,
    typeSign: TypeRepr?,
    initializer: Expr?,
    declKeywordRange: SourceRange,
    range: SourceRange
  ) {
    self.isMutable = isMutable
    self.pattern = pattern
    self.typeSign = typeSign
    self.initializer = initializer
    self.declKeywordRange = declKeywordRange
    self.range = range
  }

  /// A flag indicating whether the declared variables are mutable.
  public var isMutable: Bool

  /// The pattern being bound.
  public var pattern: Pattern

  /// The signature of the pattern.
  public var typeSign: TypeRepr?

  /// The initializer for the variables declared by the pattern.
  public var initializer: Expr?

  /// The source range of this declaration `val` or `var` keyword.
  public var declKeywordRange: SourceRange

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A variable declaration.
public final class VarDecl: ValueDecl {

  public init(
    name: String,
    type: ValType,
    range: SourceRange
  ) {
    self.name = name
    self.type = type
    self.range = range
  }

  /// The name of the variable.
  public var name: String

  /// The pattern binding declaration that introduces this variable declaration.
  public weak var patternBindingDecl: PatternBindingDecl?

  /// A flag indicating whether the variable is mutable.
  public var isMutable: Bool {
    return patternBindingDecl?.isMutable ?? false
  }

  public var isOverloadable: Bool { false }

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// The base class for function declarations.
public class AbstractFunDecl: ValueDecl, DeclScope {

  public init(
    name: String,
    declModifiers: [DeclModifier] = [],
    params: [FunParamDecl] = [],
    retTypeSign: TypeRepr? = nil,
    body: BraceStmt? = nil,
    type: ValType,
    range: SourceRange
  ) {
    self.name = name
    self.declModifiers = declModifiers
    self.params = params
    self.retTypeSign = retTypeSign
    self.body = body
    self.type = type
    self.range = range

    self.props = FunDeclProps()
    for modifier in declModifiers {
      switch modifier.kind {
      case .mut   : props.formUnion(.isMutating)
      case .static: props.formUnion(.isStatic)
      default     : continue
      }
    }
  }

  /// The name of the function (empty for anonymous functions).
  public var name: String

  /// The declaration modifiers of the function.
  ///
  /// - Note: Setting this property after initialization does not automatically updates `props`.
  public var declModifiers: [DeclModifier] = []

  /// The parameters of the function.
  public var params: [FunParamDecl]

  /// The signature of the function's return type.
  public var retTypeSign: TypeRepr?

  /// The implicit declaration of the `self` parameter for member functions.
  public var selfDecl: FunParamDecl?

  /// The body of the function.
  public var body: BraceStmt?

  /// The type and value declarations directly enclosed within the function scope.
  ///
  /// This property lists the function explicit and implicit parameters. It does **not** contain
  /// the declarations scoped within the function's body.
  public var localTypeAndValueDecls: [TypeOrValueDecl] {
    if let selfDecl = self.selfDecl {
      return params + [selfDecl]
    } else {
      return params
    }
  }

  /// The semantic properties of the declaration.
  public var props: FunDeclProps

  /// Returns whether this declaration is a member function.
  public var isMemberFun: Bool {
    return selfDecl != nil
  }

  public var isOverloadable: Bool { true }

  /// The type of the function.
  ///
  /// - Note: All member functions (except constructors) take their receiver (i.e., `self`) as am
  ///   implicit parameter. Hence, they have a type `Self, ParamTy -> RetTy`, where `Self` is the
  ///   receiver's type. You can use `appliedType` to get the method's type without the implict
  ///   receiver parameter.
  public var type: ValType

  /// The "applied" type of the function.
  ///
  /// For member functions (except constructors), this is the type of the function without the
  /// implicit receiver parameter. Otherwise, this is equal to `type`.
  public var appliedType: ValType {
    guard isMemberFun && !(self is CtorDecl) else { return type }

    let funType = type as! FunType
    let paramType = funType.paramType as! TupleType
    return type.context.funType(
      paramType: type.context.tupleType(paramType.elems[1...]),
      retType: funType.retType)
  }

  public weak var parentDeclScope: DeclScope?

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

  /// A set representing the properties of a function declaration.
  public struct FunDeclProps: OptionSet {

    public init(rawValue: Int) {
      self.rawValue = rawValue
    }

    public let rawValue: Int

    public static let isMutating = FunDeclProps(rawValue: 1 << 0)
    public static let isStatic   = FunDeclProps(rawValue: 1 << 1)

  }

}

/// A function declaration.
public final class FunDecl: AbstractFunDecl {

  public override func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A constructor declaration.
public final class CtorDecl: AbstractFunDecl {

  public override func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// The declaration of a function parameter.
public final class FunParamDecl: ValueDecl {

  public init(
    name: String,
    externalName: String? = nil,
    typeSign: TypeRepr? = nil,
    type: ValType,
    range: SourceRange
  ) {
    self.name = name
    self.externalName = externalName
    self.typeSign = typeSign
    self.type = type
    self.range = range
  }

  /// The internal name of the parameter.
  public var name: String

  /// The external name of the parameter.
  public var externalName: String?

  /// The signature of the parameter's type.
  public var typeSign: TypeRepr?

  public var isOverloadable: Bool { false }

  public var type: ValType

  public var range: SourceRange

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// The base class for nominal type declarations.
public class AbstractTypeDecl: TypeDecl, DeclScope {

  public init(
    name: String,
    inheritances: [UnqualTypeRepr] = [],
    members: [Decl] = [],
    type: ValType,
    range: SourceRange
  ) {
    self.name = name
    self.inheritances = inheritances
    self.members = members
    self.type = type
    self.range = range
  }

  /// The name of the type.
  public var name: String

  /// The views to which the type should conform.
  public var inheritances: [TypeRepr]

  /// The member declarations of the type.
  public var members: [Decl]

  /// The resolved type of the declaration.
  public unowned var type: ValType

  public var parentDeclScope: DeclScope?

  public var range: SourceRange

  // MARK: Conformance lookup

  var conformanceTable: ConformanceLookupTable?

  /// Returns the information describing the type's conformance to the specified view, or `nil` if
  /// it does not conform.
  public func conformance(to viewType: ViewType) -> ViewConformance? {
    if conformanceTable == nil {
      conformanceTable = ConformanceLookupTable()
      for repr in inheritances {
        conformanceTable!.insert(repr.type as! ViewType, range: repr.range)
      }

      // FIXME: Insert inherited conformance
    }

    return conformanceTable![viewType]
  }

  public func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A product type declaration.
public final class ProductTypeDecl: AbstractTypeDecl {

  public override func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}

/// A view type declaration.
public final class ViewTypeDecl: AbstractTypeDecl {

  public override func accept<V>(_ visitor: V) -> V.Result where V: NodeVisitor {
    return visitor.visit(self)
  }

}
