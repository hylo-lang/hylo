/// Val's symbol mangler.
///
/// This mangler is implemented as a sort of string builder. The multiple overloads of the method
/// `append()` handle the logic involved in mangling a particular object (e.g., a declaration space)
/// along with its metadata. The method `finalize()` produces the mangled name.
public struct Mangler {

  /// A buffer accumulating the components of the name mangling.
  fileprivate var buffer: String = Mangler.prefix

  public init() {}

  public mutating func append(key: ManglingOperatorKey) {
    buffer.append(key.rawValue)
  }

  public mutating func append(name: String) {
    // Generate a "sanitized" name, without any non-ASCII letter.
    var sanitized = ""
    for scalar in name.unicodeScalars {
      if scalar.isManglable {
        sanitized.append(Character(scalar))
      } else {
        let string = String(scalar.value, radix: 36)
        sanitized.append("_\(string.count)\(string)")
      }
    }

    buffer.append(contentsOf: String(describing: sanitized.count))
    buffer.append(contentsOf: sanitized)
  }

  public mutating func append(index: Int) {
    assert(index > 0)
    buffer.append("\(index)_")
  }

  public mutating func append(space: DeclSpace) {
    switch space {
    case let decl as ModuleDecl:
      append(name: decl.name)

    case let decl as ViewTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)

    case let decl as ProductTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)

    case let decl as AliasTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)

    case let decl as ExtensionDecl:
      append(space: decl.parentDeclSpace!)

    case let decl as NamespaceDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)

    case let decl as BaseFunDecl:
      append(funDecl: decl)

    case is FileUnit, is BraceStmt:
      append(space: space.parentDeclSpace!)

    default:
      fatalError("unexpected declaration space")
    }
  }

  public mutating func append(funDecl: BaseFunDecl) {
    append(space: funDecl.parentDeclSpace!)
    if funDecl.name.isEmpty {
      append(index: funDecl.discriminator)
    } else {
      append(name: funDecl.name)
    }
    append(key: .ident)
    append(type: funDecl.type)
    append(key: .funDecl)
  }

  public mutating func append(witnessImpl impl: BaseFunDecl, for req: BaseFunDecl) {
    append(space: impl.parentDeclSpace!)
    append(name: impl.name)
    append(key: .ident)
    append(space: req.parentDeclSpace!)
    append(name: req.name)
    append(key: .ident)
    append(type: req.type)
    append(key: .funDecl)
    append(key: .witnessImpl)
  }

  public mutating func append(type: ValType) {
    type.accept(&self)
  }

  public mutating func append(policy: PassingPolicy) {
    switch policy {
    case .local:
      buffer.append("1l")
    case .inout:
      buffer.append("1i")
    case .consuming:
      buffer.append("1c")
    }
    append(key: .passingPolicy)
  }

  public mutating func finalize() -> String {
    defer { buffer = Mangler.prefix }
    return buffer
  }

  public mutating func finalize<S>(into stream: inout S) where S: TextOutputStream {
    stream.write(buffer)
    buffer = Mangler.prefix
  }

  /// The prefix for all mangled symbols.
  public static let prefix = "_V"

}

extension Mangler: TypeVisitor {

  public typealias Result = Void

  public mutating func visit(_ type: KindType) {
    type.type.accept(&self)
    append(key: .kindType)
  }

  public mutating func visit(_ type: BuiltinType) {
    buffer.append(type.name)
    append(key: .builtinType)
  }

  public mutating func visit(_ type: BuiltinPointerType) {
    visit(type as BuiltinType)
  }

  public mutating func visit(_ type: BuiltinIntLiteralType) {
    visit(type as BuiltinType)
  }

  public mutating func visit(_ type: BuiltinIntType) {
    visit(type as BuiltinType)
  }

  public mutating func visit(_ type: ModuleType) {
    append(name: type.module.name)
    append(key: .moduleType)
  }

  public mutating func visit(_ type: NamespaceType) {
    append(space: type.decl)
    append(key: .namespaceType)
  }

  public mutating func visit(_ type: ProductType) {
    append(space: type.decl)
    append(key: .productType)
  }

  public mutating func visit(_ type: ViewType) {
    append(space: type.decl)
    append(key: .viewType)
  }

  public mutating func visit(_ type: AliasType) {
    type.canonical.accept(&self)
  }

  public mutating func visit(_ type: ViewCompositionType) {
    for view in type.views {
      append(type: view)
    }
    append(key: .viewCompType)
  }

  public mutating func visit(_ type: UnionType) {
    for elem in type.elems {
      append(type: elem)
    }
    append(key: .unitType)
  }

  public mutating func visit(_ type: BoundGenericType) {
    append(space: type.decl)
    append(key: .ident)
    for arg in type.args {
      append(type: arg)
    }
    append(key: .boundGenericType)
  }

  public mutating func visit(_ type: GenericParamType) {
    append(name: type.decl.name)
    append(key: .genericParamType)
  }

  public mutating func visit(_ type: AssocType) {
    append(type: type.interface)
    append(key: .assocType)
  }

  public mutating func visit(_ type: SkolemType) {
    append(type: type.interface)
    append(key: .skolemType)
  }

  public mutating func visit(_ type: WitnessType) {
    fatalError("unreachable")
  }

  public mutating func visit(_ type: TupleType) {
    for elem in type.elems {
      if let label = elem.label {
        append(name: label)
        append(key: .tupleTypeLabel)
      }
      append(type: elem.type)
      append(key: .tupleTypeElem)
    }
    append(key: .tupleType)
  }

  public mutating func visit(_ type: FunType) {
    for param in type.params {
      if let label = param.label {
        append(name: label)
        append(key: .tupleTypeLabel)
      }
      append(type: param.type)
      append(key: .tupleTypeElem)
    }
    append(key: .tupleType)
    append(type: type.retType)
    append(key: .funType)
  }

  public mutating func visit(_ type: FunParamType) {
    append(policy: type.policy)
    append(type: type.rawType)
    append(key: .funParamType)
  }

  public mutating func visit(_ type: AsyncType) {
    type.base.accept(&self)
    append(key: .asyncType)
  }

  public mutating func visit(_ type: UnresolvedType) {
    append(key: .unresolvedType)
  }

  public mutating func visit(_ type: ErrorType) {
    append(key: .errorType)
  }

  public mutating func visit(_ type: TypeVar) {
    buffer.append(String(describing: type))
  }

}

public enum ManglingOperatorKey: String {

  case ident            = "I"

  case aliasDecl        = "A"
  case funDecl          = "F"
  case moduleDecl       = "M"
  case namespaceDecl    = "N"
  case productDecl      = "P"
  case viewDecl         = "V"
  case extnDecl         = "X"

  case witnessImpl      = "W"

  case passingPolicy    = "L"

  case assocType        = "a"
  case builtinType      = "b"
  case boundGenericType = "bG"
  case errorType        = "e"
  case funType          = "f"
  case funParamType     = "fP"
  case typeList         = "l"
  case genericParamType = "g"
  case kindType         = "k"
  case moduleType       = "m"
  case namespaceType    = "n"
  case productType      = "p"
  case skolemType       = "s"
  case tupleType        = "t"
  case tupleTypeElem    = "tE"
  case tupleTypeLabel   = "tL"
  case unionType        = "u"
  case viewType         = "v"
  case viewCompType     = "vC"
  case unresolvedType   = "x"
  case asyncType        = "y"
  case unitType         = "z"

}

extension UnicodeScalar {

  /// A flag that indicates whether this unicode scalar can be mangled as part of an identifier.
  ///
  /// This returns `true` if the scalar represents an ASCII letter or digit. Note that "_" is not
  /// included, so that it can be used as a prefix to signal the start of a non-ASCII character.
  var isManglable: Bool {
    return (97 ... 122) ~= value  // 'A' ... 'Z'
        || (65 ... 90)  ~= value  // 'a' ... 'z'
        || (48 ... 57)  ~= value  // '0' ... '9'
  }

}
