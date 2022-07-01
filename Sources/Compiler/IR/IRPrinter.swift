/// A helper object to print Val IR.
public struct IRPrinter {

  /// A table that maps values to a unique name, generating the latter if necessary.
  struct NameMap<T: Hashable> {

    private var translations: [T: Int] = [:]

    var nextDiscriminator = 0

    mutating func translate(_ index: T) -> String {
      if let discriminator = translations[index] {
        return "\(discriminator)"
      } else {
        defer { nextDiscriminator += 1 }
        translations[index] = nextDiscriminator
        return "\(nextDiscriminator)"
      }
    }

  }

  /// A table mapping basic blocks to a unique name.
  var blockNames = NameMap<BlockID>()

  /// A table mapping instructions to a unique name.
  var instNames = NameMap<InstID>()

  mutating func translate(block: BlockID) -> String {
    "bb" + blockNames.translate(block)
  }

  mutating func translate(inst: InstID) -> String {
    "%" + instNames.translate(inst)
  }

}
