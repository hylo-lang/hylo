import Core

/// A logical path, in contiguous storage for zero or more instances of some element type with
/// stride spacing, from one element boundary to another at some `elementOffset`, or to
/// the address of storage for an inline sub-part of an element at that `elementOffset`.
///
/// An inline sub-part of a value `x` of type `T` is a part or part-of-a-part, etc., whose address
/// is contained in `Pointer<Byte>(address_of(x))...(Pointer<Byte>(address_of(x)) +
/// MemoryLayout<T>.size)`.
public struct InlineStoragePath: Hashable {

  /// Creates an instance that accesses the given subpart of the object at the source address.
  internal init(elementOffset: Operand, subPart: PartPath) {
    self.elementOffset = elementOffset
    self.subPart = subPart
  }

  /// Creates an instance that accesses the given subpart of the object at the source address.
  internal init(_ subPart: PartPath) {
    self.elementOffset = nil
    self.subPart = subPart
  }

  /// The number of `T`-strides from the source to the target.
  public let elementOffset: Operand?

  /// The path through inline-stored sub-parts of the target `T`.
  public let subPart: PartPath
}

extension InlineStoragePath: CustomStringConvertible {

  public var description: String {
    "\(list: [elementOffset.map(String.init(describing:)) ?? "0"] + subPart.map(String.init(describing:)))"
  }

}

/// Given a `source` element boundary in contiguous storage for zero or more elements with stride
/// spacing, returns an element boundary at a relative `elementOffset`, or the address of storage
/// for an inline sub-part of an element at that `elementOffset`.
///
/// An inline sub-part of a value `x` of type `T` is a part or part-of-a-part, etc., whose address
/// is contained in `Pointer<Byte>(address_of(x))...(Pointer<Byte>(address_of(x)) +
/// MemoryLayout<T>.size)`.
public struct InlineStorageViewInstruction: Instruction {

  /// The address of a record.
  public private(set) var source: Operand

  /// The logical path to the target pointee.
  public let targetPath: InlineStoragePath

  /// The pointee type of the result.
  public let targetType: LoweredType

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    source: Operand,
    targetPath path: InlineStoragePath,
    targetType type: LoweredType,
    site: SourceRange
  ) {
    self.source = source
    self.targetPath = path
    self.targetType = type
    self.site = site
  }

  public var types: [LoweredType] { [targetType] }

  public var operands: [Operand] {
    targetPath.elementOffset.map { [source, $0] } ?? [source]
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    precondition(i == 0)
    source = new
  }

}

extension InlineStorageViewInstruction: CustomStringConvertible {

  public var description: String {
    "inline_storage_view \(source), \(targetPath)"
  }

}

extension Module {

  /// Returns a new `inline_storage_view`, anchored at `anchor`, from the buffer storage element
  /// boundary at `source` to the address reached from `source` by following `targetPath`.
  func makeInlineStorageView(
    from source: Operand, via targetPath: InlineStoragePath, at anchor: SourceRange
  ) -> InlineStorageViewInstruction {
    precondition(type(of: source).isAddress)
    let l = AbstractTypeLayout(of: type(of: source).ast, definedIn: program)
    return .init(
      source: source,
      targetPath: targetPath,
      targetType: .address(l[targetPath.subPart].type),
      site: anchor)
  }

}
