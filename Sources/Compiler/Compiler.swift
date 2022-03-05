import Foundation
import Utils

/// A central repository for AST nodes and data shared across compilation phases.
public final class Compiler {

  /// Creates a new compiler instance.
  public init() {}

  // MARK: General properties

  /// A flag that indicates whether the compiler is processing the standard library.
  ///
  /// The standard library requires the compiler to behave slightly differently, so as to to handle
  /// built-in definitions. This flag enables this
  public var isCompilingStdlib = false

  /// The modules loaded in the context.
  public var modules: [String: ModuleDecl] = [:]

  /// The current generation number of the context, denoting the number of times new modules have
  /// been loaded.
  ///
  /// This number serves to determine whether name lookup caches are up to date.
  public var generation = 0

  // MARK: Types

  /// The types uniqued in the context.
  private var types: Set<HashableBox<ValType.HashWitness>> = []

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

  public func aliasType(decl: AliasTypeDecl) -> AliasType {
    return uniqued(AliasType(context: self, decl: decl))
  }

  public func genericParamType(decl: GenericParamDecl) -> GenericParamType {
    return uniqued(GenericParamType(context: self, decl: decl))
  }

  public func assocType(interface: GenericParamType, base: ValType) -> AssocType {
    return uniqued(AssocType(context: self, interface: interface, base: base))
  }

  public func skolemType(interface: GenericParamType, genericEnv: GenericEnv) -> SkolemType {
    return uniqued(SkolemType(context: self, interface: interface, genericEnv: genericEnv))
  }

  public func witnessType(interface: ValType) -> WitnessType {
    return uniqued(WitnessType(context: self, interface: interface))
  }

  public func viewCompositionType<S>(_ views: S) -> ViewCompositionType
  where S: Sequence, S.Element == ViewType
  {
    return uniqued(ViewCompositionType(context: self, views: Array(views)))
  }

  public func unionType<S>(_ elems: S) -> UnionType
  where S: Sequence, S.Element == ValType
  {
    return uniqued(UnionType(context: self, elems: Array(elems)))
  }

  public func boundGenericType(decl: GenericTypeDecl, args: [ValType]) -> BoundGenericType {
    return uniqued(BoundGenericType(context: self, decl: decl, args: args))
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

  public func funType(params: [FunType.Param], retType: ValType) -> FunType {
    return uniqued(FunType(context: self, params: params, retType: retType))
  }

  public func funParamType(policy: PassingPolicy, rawType: ValType) -> FunParamType {
    return uniqued(FunParamType(policy: policy, rawType: rawType))
  }

  public func asyncType(of type: ValType) -> AsyncType {
    return uniqued(AsyncType(context: self, base: type))
  }

  public private(set) lazy var unitType: TupleType = {
    return tupleType([])
  }()

  public private(set) lazy var anyType: ViewCompositionType = {
    return viewCompositionType([])
  }()

  public private(set) lazy var nothingType: UnionType = {
    return unionType([])
  }()

  public var nilType: ProductType {
    return getTypeDecl(for: .Nil)!.instanceType as! ProductType
  }

  public var copyableType: ViewType {
    return getTypeDecl(for: .Copyable)!.instanceType as! ViewType
  }

  public private(set) lazy var unresolvedType: UnresolvedType = {
    return UnresolvedType(context: self)
  }()

  public private(set) lazy var errorType: ErrorType = {
    return ErrorType(context: self)
  }()

  // MARK: Built-ins

  /// The built-in module.
  public private(set) lazy var builtin = ModuleDecl(ident: "Builtin", generation: 0, context: self)

  /// The built-in types that have been cached.
  private var builtinTypes: [String: BuiltinType] = [:]

  /// The built-in declarations that have been cached.
  private var builtinDecls: [String: FunDecl] = [:]

  /// Returns the built-in type with the specified name.
  public func getBuiltinType(named name: String) -> BuiltinType? {
    if let type = builtinTypes[name] {
      return type
    }

    if name == "Pointer" {
      let type = BuiltinPointerType(context: self)
      builtinTypes[name] = type
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

  /// Returns the declaration of the given built-in symbol.
  ///
  /// - Parameter name: A built-in name.
  /// - Returns: A named declaration if `name` is a built-in symbol; otherwise `nil`.
  public func getBuiltinDecl(for name: String) -> FunDecl? {
    if builtinDecls.isEmpty {
      loadBuiltinDecls()
    }
    return builtinDecls[name]
  }

  /// Synthesize built-in declarations.
  private func loadBuiltinDecls() {
    guard let url = Bundle.module.url(forResource: "Builtins", withExtension: "json"),
          let config = try? JSONDecoder().decode(BuiltinConfig.self, from: Data(contentsOf: url))
    else { preconditionFailure("I coudn't load builtin definitions") }

    let unit = BuiltinUnit()
    builtin.units.append(unit)

    for spec in config.functions {
      let decl = createBuiltinFunDecl(
        ident: spec[0], params: spec[1 ..< (spec.count - 1)], ret: spec[spec.count - 1])
      decl.parentDeclSpace = unit
      unit.decls.append(decl)
      builtinDecls[decl.ident] = decl
    }
  }

  private func createBuiltinFunDecl(
    ident: String,
    params: ArraySlice<String>,
    ret: String
  ) -> FunDecl {
    // Create the declaration of the function.
    let funDecl = FunDecl(ident: ident, type: unresolvedType)
    funDecl.props.insert(.isBuiltin)

    // Create the declaration(s) of the function's parameter.
    var funTypeParams: [FunType.Param] = []
    for (i, param) in params.enumerated() {
      // Create the parameter's type.
      funTypeParams.append(FunType.Param(type: parse(paramTypeNamed: param)))

      // Create the declaration of the parameter.
      let decl = FunParamDecl(ident: "_\(i)", policy: .consuming, type: funTypeParams.last!.type)
      decl.parentDeclSpace = funDecl
      decl.state = .typeChecked

      funDecl.params.append(decl)
    }

    // Create the function's type.
    funDecl.type = funType(
      params: funTypeParams,
      retType: ret.isEmpty ? unitType : parse(typeNamed: ret[ret.startIndex...]))
    funDecl.state = .typeChecked

    return funDecl
  }

  private func parse<S>(typeNamed name: S) -> ValType where S: StringProtocol {
    return name == "Unit"
      ? unitType
      : getBuiltinType(named: String(name))!
  }

  private func parse(paramTypeNamed name: String) -> ValType {
    switch name.last {
    case "&":
      return funParamType(policy: .local, rawType: parse(typeNamed: name.dropLast()))
    case "*":
      return funParamType(policy: .inout, rawType: parse(typeNamed: name.dropLast()))
    default:
      return funParamType(policy: .consuming, rawType: parse(typeNamed: name))
    }
  }

  // MARK: Standard library

  /// The standard library.
  public var stdlib: ModuleDecl?

  /// Returns the declaration of a type from the standard library.
  ///
  /// - Parameter id: The identifier of a type from the standard library.
  ///
  /// - Note: This method always returns `nil` until the standard library has been parsed and
  ///   loaded into the context.
  public func getTypeDecl(for id: KnownStdTypes) -> TypeDecl? {
    guard let stdlib = self.stdlib else { return nil }
    return stdlib.lookup(unqualified: id.name, in: self).types.first
  }

  // MARK: Debugging

  /// Dumps the contents of the context into the standard output.
  public func dump() {
    var stream = StandardOutput()
    print(to: &stream)
  }

  /// Dumps the AST into the given stream.
  public func dumpAST<S>(to stream: inout S) where S: TextOutputStream {
    var printer = NodePrinter()

    stream.write("[")
    var isFirst = true
    for module in modules.values {
      if isFirst {
        isFirst = false
      } else {
        stream.write(",")
      }
      stream.write(printer.visit(module))
    }
    stream.write("]")
  }

}

fileprivate struct BuiltinConfig: Codable {

  /// The signatures of all built-in functions.
  ///
  /// Signatures are represented as sequence of strings composed of the function's name (in first
  /// position), the type of its parameters and its return type (in last position). If the function
  /// doesn't return any value (e.g., `i64_cpy`), then the last element is an empty string.
  let functions: [[String]]

}
