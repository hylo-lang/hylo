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
    case constructor(CXXConstructor)

  }

  /// The name of the function.
  let name: CXXIdentifier

  /// The class membmers.
  let members: [ClassMember]

}
