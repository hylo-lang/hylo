/// A type compiled to C++.
enum CXXType: Hashable {

  /// A fixed-width signed integer type.
  case fixedWidthInteger(Int)

  /// A structure defined in a unit, represented by its identifier.
  case structure(Int)

  /// The never type.
  case never

  /// A pointer to the specified type.
  indirect case pointer(CXXType)

}
