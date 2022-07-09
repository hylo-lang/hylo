import Foundation
import Utils

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
      switch bitWidth {
      case 1, 8 : return "int8_t"
      case 16   : return "int16_t"
      case 32   : return "int32_t"
      case 64   : return "int64_t"
      default   : unreachable()
      }

    case .structure(let id):
      return structures[id].name.description

    case .never:
      return "void*"

    case .pointer(let baseType):
      return describe(cxxType: baseType) + "*"
    }
  }

}
