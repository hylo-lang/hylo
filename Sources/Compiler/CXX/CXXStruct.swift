import Utils

/// The description of a C++ passive data structure.
struct CXXStruct {

  /// The name of the structure.
  let name: CXXIdentifier

  /// The fields of the structure.
  var fields: [CXXType] = []

}
