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
  func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor

}

/// The signature of a tuple type (e.g., `(foo: A, bar: B)`).
public final class TupleSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The elements of the tuple.
  public var elems: [TupleSignElem]

  public init(elems: [TupleSignElem], type: ValType, range: SourceRange? = nil) {
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

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
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
  public var range: SourceRange?

  public init(label: String? = nil, sign: Sign, range: SourceRange? = nil) {
    self.label = label
    self.sign = sign
    self.range = range
  }

}

/// The signature of a function type (e.g., `A -> B`).
public final class FunSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The signature of the function's domain.
  public var paramSign: Sign

  /// The signature of the function's codomain.
  public var retSign: Sign

  /// A Boolean value that indicates whether the function is volatile.
  public var isVolatile: Bool

  public init(
    paramSign: Sign,
    retSign: Sign,
    isVolatile: Bool = false,
    type: ValType,
    range: SourceRange? = nil
  ) {
    self.paramSign = paramSign
    self.retSign = retSign
    self.isVolatile = isVolatile
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let paramType = paramSign.realize(unqualifiedFrom: useSite)
    let retType = retSign.realize(unqualifiedFrom: useSite)

    type = type.context.funType(paramType: paramType, retType: retType)
    return type
  }

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of an asynchronous type (e.g., `async A`).
public final class AsyncSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The signature of the underyling type.
  public var base: Sign

  /// The source range of the `async` keyword at the start of the signature.
  public var modifierRange: SourceRange?

  public init(base: Sign, type: ValType, range: SourceRange? = nil) {
    self.base = base
    self.type = type
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

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a mutating (a.k.a. in-out) parameter type (e.g., `mut A`).
public final class InoutSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The signature of the underyling type.
  public var base: Sign

  /// The source range of the `inout` keyword at the start of the signature.
  public var modifierRange: SourceRange?

  public init(base: Sign, type: ValType, range: SourceRange? = nil) {
    self.base = base
    self.type = type
    self.range = range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let baseType = base.realize(unqualifiedFrom: useSite)
    type = baseType.context.inoutType(of: baseType)
    return type
  }

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a union type (e.g., `A | B`).
public final class UnionSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The elements of the union.
  public var elems: [Sign]

  public init(elems: [Sign], type: ValType, range: SourceRange? = nil) {
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

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// The signature of a view composition (e.g., `V & U`).
public final class ViewCompSign: Sign {

  public var range: SourceRange?

  public var type: ValType

  /// The views of the composition.
  public var views: [Sign]

  public init(views: [Sign], type: ValType, range: SourceRange? = nil) {
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

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
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

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    let context = type.context

    // Bypass name lookup if the signature is `Any`, `Unit`, or `Nothing`.
    switch name {
    case "Any":
      type = context.anyType
      return type
    case "Unit":
      type = context.unitType
      return type
    case "Nothing":
      type = context.nothingType
      return type
    default:
      break
    }

    // Search for a type declaration.
    let matches = useSite.lookup(unqualified: name, in: context).types
    if matches.count != 1 {
      return matches.isEmpty
        ? fail(.cannotFind(type: name, range: range))
        : fail(.ambiguousReference(to: name, range: range))
    }

    // If the signature is specialized, we must realize its arguments as well.
    if let specialized = self as? SpecializedIdentSign {
      specialized.realize(from: useSite, baseDecl: matches[0] as! GenericTypeDecl)
      return type
    }

    // If the signature refers to an abstract type, we must create an associated type to remember
    // its relationship with the view. In other words, if `A` is abstract, we realize `Self::A`
    // rather than just `A`.
    if let decl = matches[0] as? AbstractTypeDecl {
      let viewDecl = decl.parentDeclSpace as! ViewTypeDecl
      let viewSelf = viewDecl.selfTypeDecl.instanceType
      type = context.assocType(interface: decl.instanceType as! GenericParamType, base: viewSelf)
      return type
    }

    // The signature refers to any other nominal type.
    type = matches[0].instanceType
    return type
  }

  /// Realizes the semantic type denoted by this component, in the context of the specified type.
  ///
  /// The purpose of this method is to realize a component as part of a compound identifier. Unlike
  /// `realize(unqualifiedFrom:)`, it looks for the referred type using a *qualified* name lookup
  /// in the context of the specified type.
  ///
  /// The strategy that's used to resolve the component depends on `parenType`:
  /// - If `parentType` is a nominal type, a standard qualified lookup and search for type members.
  /// - If `parentType` is a generic type parameter, we search for an abstract type requirement in
  ///   the views to which the parameter conforms (note: concrete type declarations can't be nested
  ///   in a view) and produce an associated type, rooted at `parentType`.
  ///
  /// - Parameters:
  ///   - parentType: The type of the parent component.
  ///   - useSite: The declaration space in which the whole type signature resides.
  func realize(in parentType: ValType, from useSite: DeclSpace) -> ValType {
    let context = type.context

    switch parentType {
    case let parentType as NominalType:
      guard let member = parentType.decl.typeMemberTable[name] else {
        return fail(.cannotFind(type: name, in: parentType, range: range))
      }

      if let member = member as? GenericParamDecl {
        // FIXME: If the parent type is not a bound generic, it probably means that we're realizing
        // a signature of the form `A::X`, where `A` is a generic type parameterized by `X`. We
        // should either accept this signature, or provide a different kind of error message.
        let param = member.instanceType as! GenericParamType
        guard let arg = (parentType as? BoundGenericType)?.bindings[param] else {
          return fail(.cannotFind(type: name, in: parentType, range: range))
        }
        return arg
      }

      if let specialized = self as? SpecializedIdentSign {
        specialized.realize(from: useSite, baseDecl: member as! GenericTypeDecl)
      } else {
        type = member.instanceType
      }

    case let parentType as GenericParamType:
      guard let conformances = parentType.genericEnv?.conformances(of: parentType) else {
        return fail(.cannotFind(type: name, in: parentType, range: range))
      }

      var candidates: [AssocType] = []
      for conf in conformances {
        if let member = conf.viewDecl.typeMemberTable[name] {
          let interface = member.instanceType as! GenericParamType
          candidates.append(context.assocType(interface: interface, base: parentType))
        }
      }

      if candidates.count != 1 {
        return candidates.isEmpty
          ? fail(.cannotFind(type: name, in: parentType, range: range))
          : fail(.ambiguousReference(to: name, range: range))
      }

      if self is SpecializedIdentSign {
        return fail(.cannotSpecializeNonGenericType(type: candidates[0], range: range))
      } else {
        type = candidates[0]
      }

    default:
      return fail(.cannotFind(type: name, in: parentType, range: range))
    }

    return type
  }

  private func fail(_ diag: Diag) -> ValType {
    type.context.report(diag)
    type = type.context.errorType
    return type
  }

}

/// An bare, unqualified type identifier (e.g., `Int64`).
public final class BareIdentSign: IdentCompSign {

  public var type: ValType

  /// An identifier.
  public var ident: Ident

  public init(ident: Ident, type: ValType) {
    self.ident = ident
    self.type = type
  }

  public var name: String { ident.name }

  public var range: SourceRange? { ident.range }

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// An unqualified type identifier with generic arguments (e.g., `Array<Int64>`).
public final class SpecializedIdentSign: IdentCompSign {

  public var type: ValType

  public var range: SourceRange?

  /// An identifier.
  public var ident: Ident

  /// The generic arguments of the type.
  public var args: [Sign]

  public init(ident: Ident, args: [Sign], type: ValType, range: SourceRange? = nil) {
    self.ident = ident
    self.args = args
    self.type = type
    self.range = range
  }

  public var name: String { ident.name }

  /// Realizes the specialized semantic type denoted by this component.
  ///
  /// - Parameters:
  ///   - useSite: The declaration space in which the type signature resides.
  ///   - baseDecl: The declaration of the generic type denoted by this component.
  public func realize(from space: DeclSpace, baseDecl: GenericTypeDecl) {
    let context = baseDecl.type.context

    // Make sure the type denoted by the base declaration is in fact generic.
    guard let clause = baseDecl.genericClause else {
      context.report(.cannotSpecializeNonGenericType(type: baseDecl.instanceType, range: range))
      type = context.errorType
      return
    }

    // Make sure we didn't get too many arguments.
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

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

}

/// A type compond identifier, composed of multiple components (e.g., `Builtin::i64`).
///
/// This always refers to the type of the last component. The others serves as explicit qualifiers.
public final class CompoundIdentSign: IdentSign {

  public var range: SourceRange?

  /// The components of the compound.
  public var components: [IdentCompSign]

  public init(components: [IdentCompSign], range: SourceRange? = nil) {
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
    guard !baseType.isError else {
      // The diagnostic is emitted by the failed attempt to realize the base.
      components.forEach({ $0.type = context.errorType })
      return context.errorType
    }

    if baseType === context.builtin.instanceType {
      // Built-in symbols are not namespaces.
      guard components.count == 2 else {
        context.report(.builtinTypesAreNotNamespaces(range: components[1].range))
        components[2...].forEach({ $0.type = context.errorType })
        return context.errorType
      }

      // Search for the built-in symbol.
      guard let builtinType = context.getBuiltinType(named: components[1].name) else {
        context.report(.cannotFind(builtin: components[1].name, range: components[1].range))
        components[2...].forEach({ $0.type = context.errorType })
        return context.errorType
      }

      components[1].type = builtinType
      return builtinType
    }

    var parentType = baseType
    for i in 1 ..< components.count {
      parentType = components[i].realize(in: parentType, from: useSite)
      guard parentType !== context.errorType else {
        components[i...].forEach({ $0.type = context.errorType })
        return context.errorType
      }
    }

    return parentType
  }

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    return visitor.visit(self)
  }

  /// Creates either a `CompoundIdentSign` with the given array, or returns its last element if it
  /// contains just one entry.
  ///
  /// - Parameter components: The components of the identifier.
  public static func create(_ components: [IdentCompSign]) -> IdentSign {
    precondition(!components.isEmpty)
    if components.count == 1 {
      return components.first!
    } else {
      let range: SourceRange?
      if let lowerLoc = components.first!.range?.lowerBound,
         let upperLoc = components.last!.range?.upperBound
      {
        range = lowerLoc ..< upperLoc
      } else {
        range = nil
      }

      return CompoundIdentSign(components: components, range: range)
    }
  }

}

/// An ill-formed signature.
///
/// The compiler should emit a diagnostic every time this type is assigned to a node, so that later
/// stages need not to reason about the cause of the error.
public final class ErrorSign: Sign {

  public var range: SourceRange?

  public var type: ValType  {
    didSet { assert(type.isError) }
  }

  public init(type: ErrorType, range: SourceRange? = nil) {
    self.type = type
    self.range = range
  }

  public init(replacing sign: Sign) {
    self.type = sign.type.context.errorType
    self.range = sign.range
  }

  public func realize(unqualifiedFrom useSite: DeclSpace) -> ValType {
    return type
  }

  public func accept<V>(_ visitor: inout V) -> V.SignResult where V: SignVisitor {
    visitor.visit(self)
  }

}
