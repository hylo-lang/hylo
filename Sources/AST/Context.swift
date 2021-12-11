import Foundation
import Basic

/// A central repository for long-lived objects and shared information about a compiling pipeline.
public final class Context {

  /// Creates a new AST context.
  ///
  /// - Parameter sourceManager: The source manager handling the source files that will be loaded
  ///   in this context.
  public init(sourceManager: SourceManager? = nil) {
    self.sourceManager = sourceManager ?? SourceManager()
  }

  deinit {
    for buffer in buffers.values {
      buffer.deinitializer(buffer.baseAddress)
      buffer.baseAddress.deallocate()
    }
  }

  // MARK: Shared memory

  /// A key identifying a shared buffer.
  public enum SharedBufferKey: Hashable {

    case vilConstantValueStore

  }

  /// A memory buffer.
  private struct SharedBuffer {

    /// An opaque pointer to a buffer.
    public let baseAddress: UnsafeMutableRawPointer

    /// A closure that accepts `pointer` and deinitialize its memory before it can be deallocated.
    public let deinitializer: (UnsafeMutableRawPointer) -> Void

  }

  /// The shared buffers own by this context.
  private var buffers: [SharedBufferKey: SharedBuffer] = [:]

  /// Allocates a memory buffer in the context.
  public func allocateBuffer<T>(
    forKey key: SharedBufferKey,
    ofType: T.Type,
    capacity: Int = 1,
    deinitializer: ((UnsafeMutablePointer<T>) -> Void)? = nil
  ) -> Bool {
    guard buffers[key] == nil else { return false }

    let addr = UnsafeMutablePointer<T>.allocate(capacity: capacity)
    let drop: (UnsafeMutableRawPointer) -> Void

    if let d = deinitializer {
      drop = { (ptr: UnsafeMutableRawPointer) -> Void in
        d(ptr.assumingMemoryBound(to: T.self))
      }
    } else {
      drop = { (ptr: UnsafeMutableRawPointer) -> Void in
        ptr.assumingMemoryBound(to: T.self).deinitialize(count: capacity)
      }
    }

    buffers[key] = SharedBuffer(baseAddress: addr, deinitializer: drop)
    return true
  }

  /// Deallocates a memory buffer in the context.
  public func deallocateBuffer(forKey key: SharedBufferKey) -> Bool {
    guard let buffer = buffers[key] else { return false }
    buffer.deinitializer(buffer.baseAddress)
    buffer.baseAddress.deallocate()
    buffers[key] = nil
    return true
  }

  /// Accesses the contents of a buffer in the context.
  public func withBuffer<T, R>(
    forKey key: SharedBufferKey,
    of: T.Type,
    _ action: (UnsafeMutablePointer<T>) throws -> R
  ) rethrows -> R? {
    return try (buffers[key]?.baseAddress.assumingMemoryBound(to: T.self)).map(action)
  }

  // MARK: General properties

  /// A flag that indicates whether the compiler is processing the standard library.
  ///
  /// The standard library requires the compiler to behave slightly differently, so as to to handle
  /// built-in definitions. This flag enables this
  public var isCompilingStdlib = false

  /// The manager handling the source files loaded in the context.
  public let sourceManager: SourceManager

  /// The modules loaded in the context.
  public var modules: [String: ModuleDecl] = [:]

  /// The current generation number of the context, denoting the number of times new modules have
  /// been loaded.
  ///
  /// This number serves to determine whether name lookup caches are up to date.
  public var generation = 0

  // MARK: Delegates

  /// A closure that is called to prepare generic environments.
  public var prepareGenericEnv: ((GenericEnv) -> Bool)?

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
  public private(set) lazy var builtin = ModuleDecl(name: "Builtin", generation: 0, context: self)

  /// The built-in types that have been cached.
  private var builtinTypes: [String: BuiltinType] = [:]

  /// The built-in declarations that have been cached.
  private var builtinDecls: [String: FunDecl] = [:]

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
        name: spec[0], params: spec[1 ..< (spec.count - 1)], ret: spec[spec.count - 1])
      decl.parentDeclSpace = unit
      unit.decls.append(decl)
      builtinDecls[decl.name] = decl
    }
  }

  private func createBuiltinFunDecl(
    name: String,
    params: ArraySlice<String>,
    ret: String
  ) -> FunDecl {
    // Create the declaration of the function.
    let funDecl = FunDecl(ident: Ident(name: name), type: unresolvedType)
    funDecl.props.insert(.isBuiltin)

    // Create the declaration(s) of the function's parameter.
    var funTypeParams: [FunType.Param] = []
    for (i, param) in params.enumerated() {
      // Create the parameter's type.
      let rawType = parse(typeNamed: param)
      funTypeParams.append(FunType.Param(policy: .consuming, rawType: rawType))

      // Create the declaration of the parameter.
      let decl = FunParamDecl(name: "_\(i)", policy: .consuming, type: funTypeParams.last!.type)
      decl.parentDeclSpace = funDecl
      decl.setState(.typeChecked)

      funDecl.params.append(decl)
    }

    // Create the function's type.
    funDecl.type = funType(
      params: funTypeParams,
      retType: ret.isEmpty ? unitType : parse(typeNamed: ret))
    funDecl.setState(.typeChecked)

    return funDecl
  }

  private func parse(typeNamed name: String) -> ValType {
    return name == "Unit"
      ? unitType
      : getBuiltinType(named: name)!
  }

  // MARK: Standard library

  /// The standard library.
  public var stdlib: ModuleDecl?

  /// Returns the requested type from the standard library.
  ///
  /// - Note: This method always returns `nil` until the standard library has been parsed and
  ///   loaded into the context.
  public func getTypeDecl(for type: KnownStdTypes) -> TypeDecl? {
    guard let stdlib = self.stdlib else { return nil }
    return stdlib.lookup(unqualified: type.name, in: self).types.first
  }

  // MARK: Diagnostics

  /// The consumer for all in-flight diagnostics.
  public var diagConsumer: DiagConsumer?

  /// Reports an in-flight diagnostic.
  public func report(_ diagnostic: Diag) {
    diagConsumer?.consume(diagnostic)
  }

  /// Reports an in-flight diagnostic.
  public func report(_ message: String, level: Diag.Level = .error, anchor: SourceRange?) {
    diagConsumer?.consume(Diag(message, anchor: anchor))
  }

  /// Dumps the contents of the context into the standard output.
  public func dump() {
    var stream = StandardOutput()
    print(to: &stream)
  }

  /// Dumps the contents of the context into the given stream.
  ///
  /// - Parameter stream: A text output stream.
  public func dump<S>(to stream: inout S) where S: TextOutputStream {
    var printer = NodePrinter(context: self)

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
