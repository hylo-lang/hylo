import Core

/// A C++ class declaration.
struct CXXClassDecl: CXXTopLevelDecl {

  /// The ID of a C++ class in its module.
  typealias ID = Int

  /// The type of a CXX class member.
  enum ClassMember {

    /// A CXX class attribute
    case attribute(CXXClassAttribute)

    /// A CXX class method
    case method

    /// A CXX constructor
    case constructor

  }

  /// The name of the function.
  let name: CXXIdentifier

  /// The class membmers.
  let members: [ClassMember]

  /// The original node in Val AST.
  let original: ProductTypeDecl.Typed

  /// Writes the signature of the class into `target`.
  func writeSignature<Target: TextOutputStream>(into target: inout Target) {
    target.write("class \(name)")
  }

  func writeDeclaration<Target: TextOutputStream>(into target: inout Target) {
    writeSignature(into: &target)
    target.write(";\n")
  }
  func writeDefinition<Target: TextOutputStream>(into target: inout Target) {
    writeSignature(into: &target)
    target.write(" {\n")
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
