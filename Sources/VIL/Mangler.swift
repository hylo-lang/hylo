import AST

/// Val's symbol mangler.
///
/// This mangler is implemented as a sort of string builder. The multiple overloads of the method
/// `append()` handle the logic involved in mangling a particular object (e.g., a declaration space)
/// along with its metadata. The method `finalize()` produces the mangled name.
public struct Mangler {

  /// A buffer accumulating the components of the name mangling.
  private var buffer: String = Mangler.prefix

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
        sanitized.append("_")
        sanitized.append(String(repeating: "0", count: 4 - string.count))
        sanitized.append(string)
      }
    }

    buffer.append(contentsOf: String(describing: sanitized.count))
    buffer.append(contentsOf: sanitized)
  }

  public mutating func append(index: Int) {
    assert(index > 0)
    buffer.append(String(describing: index))
    buffer.append("_")
  }

  public mutating func append(space: DeclSpace) {
    switch space {
    case let decl as ModuleDecl:
      append(name: decl.name)
      append(key: .moduleDecl)

    case let decl as ViewTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)
      append(key: .viewDecl)

    case let decl as ProductTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)
      append(key: .productDecl)

    case let decl as AliasTypeDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)
      append(key: .aliasDecl)

    case let decl as TypeExtnDecl:
      append(space: decl.parentDeclSpace!)
      append(key: .extnDecl)

    case let decl as NamespaceDecl:
      append(space: decl.parentDeclSpace!)
      append(name: decl.name)
      append(key: .namespaceDecl)

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
    append(type: funDecl.type)
    append(key: .funDecl)
  }

  public mutating func append(witnessImpl impl: BaseFunDecl, for req: BaseFunDecl) {
    append(space: impl.parentDeclSpace!)
    append(space: req.parentDeclSpace!)
    append(name: impl.name)
    append(type: req.type)
    append(key: .funDecl)
    append(key: .witnessImpl)
  }

  public mutating func append(type: ValType) {
    switch type.dealiased {
    case let tupleType as TupleType:
      if tupleType == type.context.unitType {
        append(key: .unitType)
        break
      }

      for elem in tupleType.elems {
        if let label = elem.label {
          append(name: label)
          append(key: .tupleTypeLabel)
        }
        append(type: elem.type)
        append(key: .tupleTypeElem)
      }
      append(key: .tupleType)

    case let funType as FunType:
      for param in funType.params {
        if let label = param.label {
          append(name: label)
          append(key: .tupleTypeLabel)
        }
        append(type: param.type)
        append(key: .tupleTypeElem)
      }
      append(key: .funTypeParam)
      append(type: funType.retType)
      append(key: .funType)

    case is BuiltinIntLiteralType:
      buffer.append("il")
      append(key: .builtinType)

    case let builtinType as BuiltinType:
      buffer.append(builtinType.name)
      append(key: .builtinType)

    default:
      break
    }
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

public enum ManglingOperatorKey: String {

  case funDecl        = "F"
  case moduleDecl     = "M"
  case productDecl    = "P"
  case viewDecl       = "V"
  case aliasDecl      = "A"
  case extnDecl       = "X"
  case namespaceDecl  = "N"

  case witnessImpl    = "W"

  case builtinType    = "b"
  case tupleType      = "t"
  case tupleTypeElem  = "tE"
  case tupleTypeLabel = "tL"
  case funType        = "f"
  case funTypeParam   = "fP"
  case unitType       = "u"

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
