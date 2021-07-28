import Basic

/// The syntactic representation of a type signature in source code.
public protocol Sign: Node {

  /// The semantic type realized from this type signature.
  var type: ValType { get }

  /// Realize the semantic type denoted by this type signature.
  ///
  /// - Parameter useSite: The declaration space in which the type signature resides. `useSite` is
  ///   used to properly contextualize generic signatures.
  @discardableResult
  func realize(unqualifiedFrom useSite: DeclSpace) -> ValType

  /// Accepts the given visitor.
  ///
  /// - Parameter visitor: A type signature visitor.
  func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor

}

/// The signature of a tuple type (e.g., `(foo: A, bar: B)`).
public final class TupleSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The elements of the tuple.
  public var elems: [TupleSignElem]

  public init(elems: [TupleSignElem], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let elems = elems.map({ (elem: TupleSignElem) -> TupleType.Elem in
      TupleType.Elem(label: elem.label, type: elem.sign.realize(unqualifiedFrom: useSite))
    })

    type = type.context.tupleType(elems)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The element of a tuple type signature.
public struct TupleSignElem {

  /// The label of the element.
  public var label: String?

  /// The signature of the element.
  public var sign: Sign

  /// The source range of this element’s textual representation.
  public var range: SourceRange

  public init(label: String? = nil, sign: Sign, range: SourceRange) {
    self.label = label
    self.sign = sign
    self.range = range
  }

}

/// The signature of a function type (e.g., `A -> B`).
public final class FunSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The signature of the function's domain.
  public var paramSign: Sign

  /// The signature of the function's codomain.
  public var retSign: Sign

  public init(paramSign: Sign, retSign: Sign, type: ValType, range: SourceRange) {
    self.paramSign = paramSign
    self.retSign = retSign
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let paramType = paramSign.realize(unqualifiedFrom: useSite)
    let retType = retSign.realize(unqualifiedFrom: useSite)

    type = type.context.funType(paramType: paramType, retType: retType)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of an asynchronous type (e.g., `async A`).
public final class AsyncSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The signature of the underyling type.
  public var base: Sign

  /// The source range of the `async` keyword at the start of the signature.
  public var modifierRange: SourceRange

  public init(base: Sign, type: ValType, modifierRange: SourceRange, range: SourceRange) {
    self.base = base
    self.type = type
    self.modifierRange = modifierRange
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let baseType = base.realize(unqualifiedFrom: useSite)
    if baseType is AsyncType {
      baseType.context.report(.superfluousTypeModifier(range: modifierRange))
    }

    type = baseType.context.asyncType(of: baseType)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a mutating (a.k.a. in-out) parameter type (e.g., `mut A`).
public final class InoutSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The signature of the underyling type.
  public var base: Sign

  /// The source range of the `inout` keyword at the start of the signature.
  public var modifierRange: SourceRange

  public init(base: Sign, type: ValType, modifierRange: SourceRange, range: SourceRange) {
    self.base = base
    self.type = type
    self.modifierRange = modifierRange
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let baseType = base.realize(unqualifiedFrom: useSite)
    type = baseType.context.inoutType(of: baseType)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a union type (e.g., `A | B`).
public final class UnionSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The elements of the union.
  public var elems: [Sign]

  public init(elems: [Sign], type: ValType, range: SourceRange) {
    self.elems = elems
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
   assert(!elems.isEmpty, "ill-formed AST; type union is empty")
   let elems = elems.map({ sign in sign.realize(unqualifiedFrom: useSite) })
   type = type.context.unionType(elems)
   return type
 }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a view composition (e.g., `V & U`).
public final class ViewCompSign: Sign {

  public var range: SourceRange

  public var type: ValType

  /// The views of the composition.
  public var views: [Sign]

  public init(views: [Sign], type: ValType, range: SourceRange) {
    self.views = views
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    assert(!views.isEmpty, "ill-formed AST; composition is empty")
    let context = type.context

    var viewTypes: [ViewType] = []
    for sign in views {
      switch sign.realize(unqualifiedFrom: useSite) {
      case let view as ViewType:
        viewTypes.append(view)

      case is ErrorType:
        type = context.errorType
        return context.errorType

      case let t:
        context.report(.nonViewTypeInViewComposition(type: t, range: sign.range))
        type = context.errorType
        return context.errorType
      }
    }

    type = context.viewCompositionType(viewTypes)
    return type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// A type identifier composed of one or several components.
///
/// This protocol abstracts over unqualified and compound type identifiers.
public protocol IdentSign: Sign {

  /// The components of the identifier.
  var components: [IdentCompSign] { get }

  /// The last component of the identifier.
  var lastComponent: IdentCompSign { get }

}

extension IdentSign {

  /// Creates either a `CompoundIdentSign` with the given array, or returns its last element if it
  /// contains just one entry.
  ///
  /// - Parameter components: The components of the identifier.
  public static func create(_ components: [IdentCompSign]) -> IdentSign {
    precondition(!components.isEmpty)
    if components.count == 1 {
      return components.first!
    } else {
      return CompoundIdentSign(
        components: components,
        range: components.first!.range.lowerBound ..< components.last!.range.upperBound)
    }
  }

}

/// A single component in a compound type identifier.
public protocol IdentCompSign: IdentSign {

  /// The name of the type.
  var name: String { get }

  /// The semantic type realized from this signature.
  var type: ValType { get set }

}

extension IdentCompSign {

  public var components: [IdentCompSign] { [self] }

  public var lastComponent: IdentCompSign { self }

  /// Realizes an unqualified identifier.
  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let context = type.context

    // Bypass name lookup if the signature is `Any` or `Nothing`.
    switch name {
    case "Any":
      type = context.anyType
      return context.anyType

    case "Nothing":
      type = context.nothingType
      return context.nothingType

    default:
      break
    }

    // Search for a type declaration.
    let matches = useSite.lookup(unqualified: name, in: context).types
    guard !matches.isEmpty else {
      context.report(.cannotFind(type: name, range: range))
      type = context.errorType
      return context.errorType
    }

    assert(matches.count == 1)

    // If the signature is specialized (e.g., `Foo<Bar>`), we must realize its arguments as well.
    if let specialized = self as? SpecializedIdentSign {
      let baseDecl = matches[0] as! GenericTypeDecl
      specialized.realize(from: useSite, baseDecl: baseDecl)
    } else {
      type = matches[0].instanceType
    }

    return type
  }

  /// Resolves the type declaration to which this component refers.
  ///
  /// - Parameter typeDecl: The declaration of the type that qualifies the component.
  public func resolve(qualifiedIn typeDecl: TypeDecl) -> TypeDecl? {
    // FIXME: Handle other type declarations.
    guard let typeDecl = typeDecl as? NominalTypeDecl else {
      fatalError("not implemented")
    }

    let matches = typeDecl.lookup(qualified: name).types
    guard !matches.isEmpty else {
      return nil
    }

    assert(matches.count == 1)
    return matches[0]
  }

  /// Realizes the semantic type denoted by the component, assuming it is qualified by a type.
  ///
  /// The main purpose of this method is to realize the component as part of a compound identifier.
  /// Unlike `realize(unqualifiedFrom:)`, it looks for the referred type using a *qualified* name
  /// lookup from the given type declaration.
  ///
  /// - Parameters:
  ///   - typeDecl: The declaration of the type that qualifies the component.
  ///   - useSite: The declaration space in which the type signature resides.
  public func realize(qualifiedIn typeDecl: TypeDecl, from useSite: DeclSpace) -> ValType {
    let context = type.context

    guard let decl = resolve(qualifiedIn: typeDecl) else {
      context.report(.cannotFind(type: name, in: typeDecl.instanceType, range: range))
      type = context.errorType
      return type
    }

    if let specialized = self as? SpecializedIdentSign {
      let baseDecl = decl as! NominalTypeDecl
      specialized.realize(from: useSite, baseDecl: baseDecl)
    } else {
      type = decl.instanceType
    }

    type = decl.instanceType
    return type
  }

}

/// An simple, unqualified type identifier (e.g., `Int64`).
public final class UnqualIdentSign: IdentCompSign {

  public var range: SourceRange

  public var type: ValType

  public var name: String

  public init(name: String, type: ValType, range: SourceRange) {
    self.name = name
    self.type = type
    self.range = range
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// An unqualified type identifier with generic arguments (e.g., `Array<Int64>`).
public final class SpecializedIdentSign: IdentCompSign {

  public var range: SourceRange

  public var type: ValType

  public var name: String

  /// The generic arguments of the type.
  public var args: [Sign]

  public init(name: String, args: [Sign], type: ValType, range: SourceRange) {
    self.name = name
    self.args = args
    self.type = type
    self.range = range
  }

  public func realize(from space: DeclSpace, baseDecl: GenericTypeDecl) {
    let context = baseDecl.type.context

    // Make sure we didn't get too many arguments.
    guard let clause = baseDecl.genericClause else {
      context.report(
        .tooManyGenericArguments(
          type: baseDecl.instanceType, got: args.count, expected: 0, range: range))
      type = context.errorType
      return
    }

    guard clause.params.count >= args.count else {
      context.report(
        .tooManyGenericArguments(
          type: baseDecl.instanceType,
          got: args.count,
          expected: clause.params.count,
          range: range))
      type = context.errorType
      return
    }

    // Realize the generic arguments.
    var argTypes: [ValType] = []
    for arg in args {
      argTypes.append(arg.realize(unqualifiedFrom: space))
      guard arg.type !== context.errorType else {
        type = context.errorType
        return
      }
    }

    type = context.boundGenericType(decl: baseDecl, args: argTypes)
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// A type compond identifier, composed of multiple components (e.g., `Builtin::i64`).
///
/// This always refers to the type of the last component. The others serves as explicit qualifiers.
public final class CompoundIdentSign: IdentSign {

  public var range: SourceRange

  /// The components of the compound.
  public var components: [IdentCompSign]

  public init(components: [IdentCompSign], range: SourceRange) {
    precondition(components.count > 1)
    self.components = components
    self.range = range
  }

  public var lastComponent: IdentCompSign {
    return components[components.count - 1]
  }

  public var type: ValType {
    return lastComponent.type
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let context = type.context

    // Realize the base component, unqualified.
    let baseType = components[0].realize(unqualifiedFrom: useSite)
    guard !(baseType is ErrorType) else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      components.forEach({ $0.type = context.errorType })
      return context.errorType
    }

    // Handle built-ins.
    if baseType === context.builtin.instanceType {
      guard let builtinType = context.getBuiltinType(named: components[1].name) else {
        context.report(
          .cannotFind(builtin: components[1].name, range: components[1].range))
        components[2...].forEach({ $0.type = context.errorType })
        return context.errorType
      }
      components[1].type = builtinType

      // Built-in symbols are not namespaces.
      guard components.count == 2 else {
        context.report(.builtinTypesAreNotNamespaces(range: components[1].range))
        components[2...].forEach({ $0.type = context.errorType })
        return context.errorType
      }

      return builtinType
    }

    // Realize each subsequent component using their predecessor as a qualified based.
    guard var baseDecl: TypeDecl = (baseType as? NominalType)?.decl else {
      context.report(
        .cannotFind(type: components[1].name, in: baseType, range: components[1].range))
      components[1...].forEach({ $0.type = context.errorType })
      return context.errorType
    }

    for i in 1 ..< components.count {
      let componentType = components[i].realize(qualifiedIn: baseDecl, from: useSite)
      guard componentType !== context.errorType else {
        components[i...].forEach({ $0.type = context.errorType })
        return context.errorType
      }

      // Components always have a nominal type.
      baseDecl = (componentType as! NominalType).decl
    }

    return lastComponent.type
  }

  public func accept<V>(_ visitor: V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}
