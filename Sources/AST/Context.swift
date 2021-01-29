import Basic

/// A structure that holds AST nodes along with other long-lived metadata.
public final class Context {

  /// Creates a new AST context.
  public init(sourceManager: SourceManager) {
    self.sourceManager = sourceManager
  }

  /// A flag that indicates whether the compiler is processing the standard library.
  ///
  /// The standard library requires the compiler to behave slightly differently, so as to to handle
  /// built-in definitions. This flag enables this
  public var isCompilingStdLib = false

  /// The manager handling the source files loaded in the context.
  public let sourceManager: SourceManager

  /// The consumer for all in-flight diagnostics.
  public var diagnosticConsumer: DiagnosticConsumer?

  /// The modules loaded in the context.
  public var modules: [String: Module] = [:]

  // MARK: Types

  /// The types uniqued in the context.
  private var types: Set<HashableBox<ValType, ValType.HashWitness>> = []

  private func uniqued<T>(_ newType: T) -> T where T: ValType {
    let (_, box) = types.insert(HashableBox(newType))
    return box.value as! T
  }

  public func kindType(type: ValType) -> KindType {
    return uniqued(KindType(context: self, type: type))
  }

  public func productType(decl: ProductTypeDecl) -> ProductType {
    return uniqued(ProductType(context: self, decl: decl))
  }

  public func viewType(decl: ViewTypeDecl) -> ViewType {
    return uniqued(ViewType(context: self, decl: decl))
  }

  public func genericParamType(decl: GenericParamDecl) -> GenericParamType {
    return uniqued(GenericParamType(context: self, decl: decl))
  }

  public func existentialType() -> ExistentialType {
    return uniqued(ExistentialType(context: self))
  }

  public func tupleType<S>(_ elems: S) -> TupleType
  where S: Sequence, S.Element == TupleType.Elem
  {
    return uniqued(TupleType(context: self, elems: Array(elems)))
  }

  public func tupleType<S>(labelAndTypes: S) -> TupleType
  where S: Sequence, S.Element == (String?, ValType)
  {
    return tupleType(labelAndTypes.map({ (label, type) in
      TupleType.Elem(label: label, type: type)
    }))
  }

  public func tupleType<S>(types: S) -> TupleType
  where S: Sequence, S.Element == ValType
  {
    return tupleType(types.map({ type in
      TupleType.Elem(type: type)
    }))
  }

  public func funType(paramType: ValType, retType: ValType) -> FunType {
    return uniqued(FunType(context: self, paramType: paramType, retType: retType))
  }

  public func inoutType(of type: ValType) -> InoutType {
    precondition(!(type is InoutType), "cannot nest in-out types")
    return uniqued(InoutType(context: self, base: type))
  }

  public private(set) lazy var unitType: TupleType = {
    return tupleType([])
  }()

  public private(set) lazy var unresolvedType: UnresolvedType = {
    return UnresolvedType(context: self)
  }()

  public private(set) lazy var errorType: ErrorType = {
    return ErrorType(context: self)
  }()

  // MARK: Built-ins

  /// The built-in module.
  public private(set) lazy var builtin: Module = {
    // TODO: Load built-in function declarations.
    return Module(id: "Builtin", context: self)
  }()

  /// The built-in types that have been cached.
  private var builtinTypes: [String: BuiltinType] = [:]

  /// The built-in declarations that have been cached.
  var builtinDecls: [String: ValueDecl] = [:]

  /// Returns the built-in type with the specified name.
  public func getBuiltinType(named name: String) -> BuiltinType? {
    if let type = builtinTypes[name] {
      return type
    }

    if name == "IntLiteral" {
      let type = BuiltinIntLiteralType(context: self)
      builtinTypes[name] = type
      return type
    }

    if name.starts(with: "i") {
      if let bitWidth = Int(name.dropFirst()), (bitWidth > 0) && (bitWidth <= 64) {
        let type = BuiltinIntType(context: self, name: name, bitWidth: bitWidth)
        builtinTypes[name] = type
        return type
      }
    }

    return nil
  }

  // Returns the type of an assignment operator for the specified built-in type.
  public func getBuiltinAssignOperatorType(_ type: BuiltinType) -> FunType {
    return funType(paramType: tupleType(types: [type, type]), retType: type)
  }

  /// Returns the declaration of the given built-in symbol.
  ///
  /// - Parameter name: A built-in name.
  /// - Returns: A named declaration if `name` is a built-in symbol; otherwise `nil`.
  public func getBuiltinDecl(for name: String) -> ValueDecl? {
    if builtinDecls.isEmpty {
      loadBuiltinDecls()
    }
    return builtinDecls[name]
  }

  // MARK: Standard library

  /// The standard library.
  public var stdlib: Module?

  /// Returns the requested type from the standard library.
  ///
  /// - Note: This method always returns `nil` until the standard library has been parsed and
  ///   loaded into the context.
  public func getTypeDecl(for type: KnownStdLibTypes) -> TypeDecl? {
    guard let stdlib = self.stdlib else { return nil }
    return stdlib.lookup(unqualified: type.name, in: self).types.first
  }

  // MARK: Diagnostics

  /// Reports an in-flight diagnostic.
  public func report(_ diagnostic: Diagnostic) {
    diagnosticConsumer?.consume(diagnostic)
  }

}
