import Core

/// Projects a value from a subscript bundle.
public struct ProjectBundle: Instruction {

  /// The subscript bundle implementing the projections.
  public let bundle: SubscriptBundleReference

  /// The pure functional type of the callee.
  public let pureCalleeType: LambdaType

  /// The subscripts implementing the projection.
  public let variants: [AccessEffect: Function.ID]

  /// The arguments of the call.
  ///
  /// Operands to non-`sink` inputs must be the result of an `access` instruction requesting a
  /// capability for each variant in `calle` and having no use before `project`.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    bundle: SubscriptBundleReference,
    pureCalleeType: LambdaType,
    variants: [AccessEffect: Function.ID],
    operands: [Operand],
    site: SourceRange
  ) {
    self.bundle = bundle
    self.pureCalleeType = pureCalleeType
    self.variants = variants
    self.operands = operands
    self.site = site
  }

  /// The capabilities of the projected value.
  public var capabilities: AccessEffectSet {
    .init(variants.keys)
  }

  /// The type of the projected value.
  public var projection: RemoteType {
    RemoteType(pureCalleeType.output)!
  }

  /// The parameters of the projection.
  public var parameters: LazyMapSequence<[CallableTypeParameter], ParameterType> {
    pureCalleeType.inputs.lazy.map({ ParameterType($0.type)! })
  }

  /// The types of the instruction's results.
  public var result: IR.`Type`? {
    .address(projection.bareType)
  }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension ProjectBundle: CustomStringConvertible {

  public var description: String {
    if operands.isEmpty {
      return "project_bundle \(capabilities) \(bundle)"
    } else {
      return "project_bundle \(capabilities) \(bundle), \(list: operands)"
    }
  }

}

extension Module {

  /// Creates a `project_bundle` anchored at `site` that projects a value by applying one of the
  /// given `variants` on `arguments`. The variants are defined in `bundle`, which is has type
  /// `bundleType`.
  ///
  /// - Requires: `bundleType` is canonical and `variants` is not empty.
  func makeProjectBundle(
    applying variants: [AccessEffect: Function.ID],
    of bundle: SubscriptBundleReference,
    typed bundleType: SubscriptType,
    to arguments: [Operand],
    at site: SourceRange
  ) -> ProjectBundle {
    precondition(bundleType[.isCanonical])
    return .init(
      bundle: bundle,
      pureCalleeType: bundleType.pure,
      variants: variants,
      operands: arguments,
      site: site)
  }

}
