import Core

/// Projects a value from a subscript bundle.
public struct ProjectBundle: Instruction {

  /// The subscript bundle implementing the projections.
  public let bundle: BundleReference<SubscriptDecl>

  /// The pure functional type of the callee.
  public let pureCalleeType: LambdaType

  /// The subscripts implementing the projection.
  public let variants: [AccessEffect: Function.ID]

  /// The arguments of the call.
  ///
  /// Operands to must be the result of an `access` instruction requesting a capability for each
  /// variant in `callee` and having no use before `project_bundle`.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    bundle: BundleReference<SubscriptDecl>,
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
  /// variants in `bundle` on `arguments`, canonicalizing types in `scopeOfUse`.
  mutating func makeProjectBundle(
    applying bundle: BundleReference<SubscriptDecl>,
    to arguments: [Operand],
    in scopeOfUse: AnyScopeID,
    at site: SourceRange
  ) -> ProjectBundle {
    var variants: [AccessEffect: Function.ID] = [:]
    for v in program[bundle.bundle].impls {
      variants[program[v].introducer.value] = demandDeclaration(lowering: v)
    }

    let bundleType = program.canonicalType(
      of: bundle.bundle, specializedBy: bundle.arguments, in: scopeOfUse)
    let t = SubscriptType(bundleType)!.pure

    return .init(
      bundle: bundle, pureCalleeType: t, variants: variants, operands: arguments, site: site)
  }

}
