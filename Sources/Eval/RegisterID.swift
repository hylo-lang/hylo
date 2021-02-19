/// The identifier of a register in the interpreter's local frames.
struct RegisterID: Hashable {

  unowned let value: AnyObject

  init(_ value: AnyObject) {
    self.value = value
  }

  func hash(into hasher: inout Hasher) {
    hasher.combine(ObjectIdentifier(value))
  }

  static func == (lhs: RegisterID, rhs: RegisterID) -> Bool {
    return lhs.value === rhs.value
  }

}
