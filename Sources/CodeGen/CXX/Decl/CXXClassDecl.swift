import Core

/// A C++ class declaration.
public struct CXXClassDecl {

  /// The ID of a C++ class in its module.
  public typealias ID = Int

  /// The type of a CXX class member.
  public enum ClassMember {

    /// A CXX class attribute
    case attribute(CXXClassAttribute)

    /// A CXX class method
    case method

    /// A CXX constructor
    case constructor

  }

  /// The name of the function.
  public let name: CXXIdentifier

  /// The class membmers.
  public let members: [ClassMember]

  /// The original node in Val AST.
  let original: ProductTypeDecl.Typed

  /// Writes the signature of the class into `target`.
  public func writeSignature<Target: TextOutputStream>(into target: inout Target) {
    target.write("class \(name)")
  }

  /// Writes the definition of the class into `target`.
  public func writeDefinition<Target: TextOutputStream>(into target: inout Target) {
    target.write("{\n")
    target.write("public:\n")
    for member in members {
      switch member {
      case .attribute(let attribute):
        attribute.writeCode(into: &target)
      case .method:
        target.write("// method\n")
      case .constructor:
        target.write("// constructor\n")
      }
    }
    target.write("};\n")
  }

}
