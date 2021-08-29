import VIL

/// An existential container.
struct ExistentialContainer {

  /// The value packaged into the container.
  ///
  /// If the value fits into the payload and if its memory alignment is smaller than that of the
  /// container, then it is laid out directly into the container. Otherwise, the payload stores a
  /// pointer referencing the memory in which the value is packaged.
  var payload: (Int, Int, Int)

  /// The key identifying the value witness table of the he type stored in the container.
  var witnessKey: Interpreter.ValueWitnessTableKey

  /// Returns whether the payload of an existential container holds the given number of bytes.
  static func holdsInPayload(byteCount: Int, alignedAt alignment: Int) -> Bool {
    return byteCount < MemoryLayout<(Int, Int, Int)>.size
        && alignment <= MemoryLayout<ExistentialContainer>.alignment
  }

}
