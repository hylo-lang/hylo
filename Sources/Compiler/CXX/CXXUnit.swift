import Foundation

/// A C++ compilation unit.
struct CXXUnit {

  /// The name of the unit.
  let name: CXXIdentifier

  /// The textual representation of the unit's source.
  var source: String = ""

  /// The structs in the unit.
  var structures: [CXXStruct] = []

  /// A mapping from product type declarations to the corresponding C++ struct representation.
  var productTypeReprs: [NodeID<ProductTypeDecl>: Int] = [:]

  /// A mapping from tuple types to the corresponding C++ struct reepresentation.
  var tupleReprs: [TupleType: Int] = [:]

  /// A counter to generate unique identifiers.
  private var nextIdentifierSuffix = 0

  /// Creates a new C++ compilation unit with the specified name.
  init(name: CXXIdentifier) {
    self.name = name
  }

  /// Insert `newStruct` in the unit and returns its identifier.
  mutating func insert(struct newStruct: CXXStruct) -> Int {
    let id = structures.count
    structures.append(newStruct)
    return id
  }

  mutating func makeStructIdentifier() -> CXXIdentifier {
    let name = "S\(nextIdentifierSuffix)"
    nextIdentifierSuffix += 1
    return CXXIdentifier(name)
  }

  /// Returns the textual representation of the give C++ type.
  func describe(cxxType: CXXType) -> String {
    switch cxxType {
    case .fixedWidthInteger(let bitWidth):
      return "int\(bitWidth)_t"

    case .structure(let id):
      return structures[id].name.description

    case .never:
      return "void*"

    case .pointer(let baseType):
      return describe(cxxType: baseType) + "*"
    }
  }

}
