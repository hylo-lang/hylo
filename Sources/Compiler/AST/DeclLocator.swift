import Utils

/// A value that specifies the location of a declaration.
public struct DeclLocator: Hashable {

  /// A constituent of a declaration locator.
  public enum Component: Hashable {

    case conformance(target: DeclLocator, trait: String)

    case `extension`(target: DeclLocator)

    case function(name: String, labels: [String], notation: OperatorNotation?)

    case methodImpl(ImplIntroducer)

    case module(String)

    case namespace(String)

    case lambda(NodeID<FunDecl>)

    case product(String)

    case `subscript`(name: String, labels: [String])

    case subscriptImpl(ImplIntroducer)

    case trait(String)

    /// Creates a component identifying `decl` in `program`.
    init?<T: NodeIDProtocol>(identifying decl: T, in program: TypedProgram) {
      switch decl.kind {
      case .conformanceDecl, .extensionDecl:
        fatalError("not implemented")

      case .funDecl:
        let decl = NodeID<FunDecl>(rawValue: decl.rawValue)

        let labels: [String]
        switch program.declTypes[decl]! {
        case .lambda(let type):
          labels = Array(type.labels.map({ $0 ?? "_" }))
        case .method(let type):
          labels = Array(type.inputs.map({ $0.label ?? "_" }))
        default:
          labels = []
        }

        switch program.ast[decl].introducer.value {
        case .memberwiseInit, .`init`:
          self = .function(name: "init", labels: labels, notation: nil)
        case .deinit:
          self = .function(name: "deinit", labels: [], notation: nil)
        case .fun:
          if let name = program.ast[decl].identifier?.value {
            self = .function(
              name: name, labels: labels, notation: program.ast[decl].notation?.value)
          } else {
            self = .lambda(decl)
          }
        }

      case .methodImplDecl:
        let decl = NodeID<MethodImplDecl>(rawValue: decl.rawValue)
        self = .methodImpl(program.ast[decl].introducer.value)

      case .productTypeDecl:
        let decl = NodeID<ProductTypeDecl>(rawValue: decl.rawValue)
        self = .product(program.ast[decl].name)

      default:
        return nil
      }
    }

    /// A mangled description of this component.
    public var mangled: String {
      switch self {
      case .conformance(let target, let trait):
        return "C\(target)\(trait.mangled)"

      case .extension(let target):
        return "E\(target)"

      case .function(let name, let labels, let notation):
        let labels = labels.map({ $0.mangled }).joined()
        if let n = notation {
          return "O\(String(describing: n).mangled)\(name.mangled)\(labels.count)\(labels)a"
        } else {
          return "F\(name.mangled)\(labels.count)\(labels)a"
        }

      case .methodImpl(let introducer):
        switch introducer {
        case .let  : return "Il"
        case .inout: return "Ii"
        case .set  : return "Ia"
        case .sink : return "Is"
        }

      case .module(let name):
        return "M\(name.mangled)"

      case .namespace(let name):
        return "N\(name.mangled)"

      case .lambda(let discriminator):
        return "L\(discriminator.rawValue)"

      case .product(let name):
        return "P\(name.mangled)"

      case .subscript(let name, let labels):
        let ls = labels.map({ $0.mangled }).joined()
        return "S\(name.mangled)\(labels.count)\(ls)"

      case .subscriptImpl(let introducer):
        switch introducer {
        case .let   : return "Il"
        case .inout : return "Ii"
        case .set   : return "Ia"
        case .sink  : return "Is"
        }

      case .trait(let name):
        return "N\(name.mangled)"
      }
    }

  }

  /// The constituents of the locator.
  public var components: [Component]

  /// Creates a locator identifying `decl` in `program`.
  public init<T: DeclID>(identifying decl: T, in program: TypedProgram) {
    components = [Component(identifying: decl, in: program)!]

    if let parent = program.scopeHierarchy.container[decl] {
      for scopeID in program.scopeHierarchy.scopesToRoot(from: parent) {
        if let component = Component(identifying: scopeID, in: program) {
          components.append(component)
        }
      }
    }

    components.reverse()
  }

  /// The locator's value encoded as a string.
  public var mangled: String {
    components.lazy.map({ $0.mangled }).joined()
  }

}

extension DeclLocator: CustomStringConvertible {

  public var description: String {
    components.descriptions(joinedBy: ".")
  }

}

extension DeclLocator.Component: CustomStringConvertible {

  public var description: String {
    switch self {
    case .conformance(let target, let trait):
      return "(\(target)::\(trait)"

    case .extension(let target):
      return target.description

    case .function(let name, let labels, let notation):
      let n = notation.map(String.init(describing:)) ?? ""
      if labels.isEmpty {
        return n + name
      } else {
        return n + name + "(" + labels.lazy.map({ "\($0):" }).joined() + ")"
      }

    case .methodImpl(let introducer):
      return String(describing: introducer)

    case .module(let name):
      return name

    case .namespace(let name):
      return name

    case .lambda(let discriminator):
      return String(describing: discriminator.rawValue)

    case .product(let name):
      return name.description

    case .subscript(let name, let labels):
      if labels.isEmpty {
        return name
      } else {
        return name + "[" + labels.lazy.map({ "\($0):" }).joined() + "]"
      }

    case .subscriptImpl(let introducer):
      return String(describing: introducer)

    case .trait(let name):
      return name
    }
  }

}

extension String {

  fileprivate var mangled: String {
    // Substitute non alphanumeric character.
    var result = ""
    result.reserveCapacity(count)

    for character in self {
      if character.isMangledAllowed {
        result.append(character)
      } else {
        result.append(character.utf16.reduce(into: "u", { (u, point) in
          u += String(point, radix: 16)
        }))
      }
    }

    return String(describing: result.count) + result
  }

}

extension Character {

  /// Indicates whether the character is allowed to appear in a mangled identifier.
  fileprivate var isMangledAllowed: Bool {
    guard let code = asciiValue else { return false }
    return (0x61 ... 0x7a).contains(code) // a ... z
        || (0x41 ... 0x5a).contains(code) // A ... Z
        || (0x30 ... 0x39).contains(code) // 0 ... 9
        || (0x5f == code)                 // _
  }

}
