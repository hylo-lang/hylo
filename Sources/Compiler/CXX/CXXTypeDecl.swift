/// A C++ public type declaration.
struct CXXTypeDecl {

  public enum SectionAccess: Equatable {

    case `public`, `private`

  }

  /// The name of the type.
  private let name: String

  /// The stored properties.
  ///
  /// The order of this array is preserved in the C++ textual output.
  private let fields: [(definition: String, access: SectionAccess)] = []

  /// The public constructors.
  private let publicCtors: [String] = []

  /// The private constructors.
  private let privateCtors: [String] = []

  /// The public destructor, if any.
  private let dtor: String?

  /// The public methods, properties, and subscripts.
  private let methods: [String] = []

  /// Writes the C++ textual representation of `self`.
  func write<Target: TextOutputStream>(into output: inout Target) {
    // Emit the definition.
    output.write("class \(name) {\n")

    // Emit all stored properties.
    var sectionAccess: SectionAccess? = nil
    for (definition, access) in fields {
      if access != sectionAccess {
        output.write("\(access):\n")
        sectionAccess = access
      }
      output.write(definition)
      output.write("\n")
    }

    // Emit the public API.
    output.write("public:\n")

    // Disable the default constructor.
    output.write("\(name)() = delete;\n")

    // Disable the move constructor, unless the type is publicly sinkable.
    // TODO: Determine if conformance to Sinkable is external.
    output.write("\(name)(\(name)&&) = delete;\n")
    output.write("\(name)& operator=(\(name)&&) = delete;\n")

    // Disable implicit copying.
    output.write("\(name)(\(name) const&) = delete;\n")
    output.write("\(name)& operator=(\(name) const&) = delete;\n")

    // Emit public constructors.
    for ctor in publicCtors.sorted() {
      output.write(ctor)
      output.write("\n")
    }

    // Emit other public members.
    if let member = dtor {
      output.write(member)
      output.write("\n")
    }
    for member in methods.sorted() {
      output.write(member)
      output.write("\n")
    }

    // Emit private constructors.
    if !privateCtors.isEmpty {
      output.write("private:\n")
      for ctor in privateCtors.sorted() {
        output.write(ctor)
        output.write("\n")
      }
    }

    output.write("};\n")
  }

}
