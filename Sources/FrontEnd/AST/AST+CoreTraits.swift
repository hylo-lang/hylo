/// The description of a core trait that has been loaded in an AST.
public protocol CoreTraitDescription: Codable, Sendable {

  /// The identifier of the trait declaration in the AST.
  var decl: TraitDecl.ID { get }

}

/// A view of the traits in Hylo's standard library that are known by the compiler.
public struct CoreTraits: Codable, Sendable {

  /// `Hylo.Collection`.
  public let collection: CollectionDescription

  /// `Hylo.Copyable`.
  public let copyable: CopyableDescription

  /// `Hylo.Deinitializable`.
  public let deinitializable: DeinitializableDescription

  /// `Hylo.EquatableTrait`.
  public let equatable: EquatableDescription

  /// `Hylo.ExpressibleByFloatLiteral`.
  public let expressibleByFloatLiteral: ExpressibleByFloatLiteralDescription

  /// `Hylo.ExpressibleByIntegerLiteral`.
  public let expressibleByIntegerLiteral: ExpressibleByIntegerLiteralDescription

  /// `Hylo.ForeignConvertible`.
  public let foreignConvertible: ForeignConvertibleDescription

  /// `Hylo.Iterator`.
  public let iterator: IteratorDescription

  /// `Hylo.Movale`.
  public let movable: MovableDescription

  /// Creates an instance referring to the declarations in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.collection = .init(ast)
    self.copyable = .init(ast)
    self.deinitializable = .init(ast)
    self.equatable = .init(ast)
    self.expressibleByFloatLiteral = .init(ast)
    self.expressibleByIntegerLiteral = .init(ast)
    self.foreignConvertible = .init(ast)
    self.iterator = .init(ast)
    self.movable = .init(ast)
  }

}

/// A view of `Hylo.Collection`'s declaration from the standard library.
public struct CollectionDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// the `Element` requirement.
  public let element: AssociatedTypeDecl.ID

  /// The `Position` requirement.
  public let position: AssociatedTypeDecl.ID

  /// The `start_position` requirement.
  public let startPosition: FunctionDecl.ID

  /// The `start_position` requirement.
  public let endPosition: FunctionDecl.ID

  /// The `position(after:)` requirement.
  public let positionAfter: FunctionDecl.ID

  /// The `[].let` requirement.
  public let access: SubscriptImpl.ID

  /// Creates an instance referring to the declaration of `Hylo.Collection` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Collection")!.decl

    self.element = AssociatedTypeDecl.ID(
      ast.requirements(Name(stem: "Element"), in: self.decl)[0])!
    self.position = AssociatedTypeDecl.ID(
      ast.requirements(Name(stem: "Position"), in: self.decl)[0])!
    self.startPosition = FunctionDecl.ID(
      ast.requirements(Name(stem: "start_position"), in: self.decl)[0])!
    self.endPosition = FunctionDecl.ID(
      ast.requirements(Name(stem: "end_position"), in: self.decl)[0])!
    self.positionAfter = FunctionDecl.ID(
      ast.requirements(Name(stem: "position", labels: ["after"]), in: self.decl)[0])!
    self.access = SubscriptImpl.ID(
      ast.requirements(Name(stem: "[]", labels: [nil], introducer: .let), in: self.decl)[0])!
  }

}

/// A view of `Hylo.Copyable`'s declaration from the standard library.
public struct CopyableDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// The `copy()` requirement.
  public let copy: FunctionDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.Copyable` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Copyable")!.decl
    self.copy = FunctionDecl.ID(ast.requirements(Name(stem: "copy"), in: self.decl)[0])!
  }

}

/// A view of `Hylo.Deinitializable`'s declaration from the standard library.
public struct DeinitializableDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// The `deinit` requirement.
  public let deinitialize: FunctionDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.Deinitializable` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Deinitializable")!.decl
    self.deinitialize = FunctionDecl.ID(ast[self.decl].members[0])!
    assert(ast[self.deinitialize].identifier?.value == "deinit")
  }

}

/// A view of `Hylo.Equatable`'s declaration from the standard library.
public struct EquatableDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// The `infix==(_:)` requirement.
  public let equal: FunctionDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.Equatable` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Equatable")!.decl
    self.equal = FunctionDecl.ID(ast[self.decl].members[0])!
    assert(ast[self.equal].identifier?.value == "==")
  }

}

/// A view of `Hylo.ExpressibleByFloatLiteral`'s declaration from the standard library.
public struct ExpressibleByFloatLiteralDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.ExpressibleByFloatLiteral` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("ExpressibleByFloatLiteral")!.decl
  }

}

/// A view of `Hylo.ExpressibleByIntegerLiteral`'s declaration from the standard library.
public struct ExpressibleByIntegerLiteralDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.ExpressibleByIntegerLiteral` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("ExpressibleByIntegerLiteral")!.decl
  }

}

/// A view of `Hylo.ForeignConvertible`'s declaration from the standard library.
public struct ForeignConvertibleDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// The `ForeignRepresentation` requirement.
  public let foreignRepresentation: AssociatedTypeDecl.ID

  /// The `init(foreign_value:)` requirement.
  public let initialize: InitializerDecl.ID

  /// The `foreign_value` requirement.
  public let transform: FunctionDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.ForeignConvertible` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("ForeignConvertible")!.decl

    self.foreignRepresentation = AssociatedTypeDecl.ID(
      ast.requirements(Name(stem: "ForeignRepresentation"), in: self.decl)[0])!
    self.initialize = InitializerDecl.ID(
      ast.requirements(Name(stem: "init", labels: ["foreign_value"]), in: self.decl)[0])!
    self.transform = FunctionDecl.ID(
      ast.requirements(Name(stem: "foreign_value"), in: self.decl)[0])!
  }

}

/// A view of `Hylo.Iterator`'s declaration from the standard library.
public struct IteratorDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// the `Element` requirement.
  public let element: AssociatedTypeDecl.ID

  /// The `next` requirement.
  public let next: FunctionDecl.ID

  /// Creates an instance referring to the declaration of `Hylo.Iterator` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Iterator")!.decl

    self.element = AssociatedTypeDecl.ID(
      ast.requirements(Name(stem: "Element"), in: self.decl)[0])!
    self.next = FunctionDecl.ID(
      ast.requirements(Name(stem: "next"), in: self.decl)[0])!
  }

}

/// A view of `Hylo.Movable`'s declaration from the standard library.
public struct MovableDescription: CoreTraitDescription, Sendable {

  /// The identifier of the trait declaration.
  public let decl: TraitDecl.ID

  /// The `take_value(from:).set` requirement.
  public let moveInitialize: MethodImpl.ID

  /// The `take_value(from:).inout` requirement.
  public let moveAssign: MethodImpl.ID

  /// Creates an instance referring to the declaration of `Hylo.Movable` in `ast`.
  ///
  /// - Requires: The standard library must have been loaded in `ast`.
  public init(_ ast: AST) {
    self.decl = ast.coreTrait("Movable")!.decl

    let move = MethodDecl.ID(ast[self.decl].members[0])!
    assert(ast[move].identifier.value == "take_value")

    self.moveInitialize = ast.implementation(.set, of: move)!
    self.moveAssign = ast.implementation(.inout, of: move)!
  }

}
