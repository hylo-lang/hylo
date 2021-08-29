import VIL

/// A data structure that supplies the implementation of fundamental operations for allocating,
/// copying and destrying values of a type.
struct ValueWitnessTable {

  /// The type to which the table associated.
  let type: VILType

  /// The size of the type.
  let size: Int

  /// The stride of the type.
  let stride: Int

  /// The alignment of the type.
  let alignment: Int

}
