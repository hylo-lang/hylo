import Core

/// Projects a value from a subscript bundle.
public struct ProjectBundle: Instruction {

  /// The subscript bundle implementing the projections.
  public let bundle: BundleReference<SubscriptDecl>

  /// The parameters of the subscript.
  public let parameters: [ParameterType]

  /// The type of the projected value.
  public let projection: AnyType

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
    variants: [AccessEffect: Function.ID],
    parameters: [ParameterType],
    projection: AnyType,
    operands: [Operand],
    site: SourceRange
  ) {
    self.bundle = bundle
    self.variants = variants
    self.parameters = parameters
    self.projection = projection
    self.operands = operands
    self.site = site
  }

  /// The capabilities of the projected value.
  public var capabilities: AccessEffectSet {
    .init(variants.keys)
  }

  /// The types of the instruction's results.
  public var result: IR.`Type`? {
    .address(projection)
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
      bundle: bundle, variants: variants,
      parameters: t.inputs.lazy.map({ ParameterType($0.type)! }),
      projection: RemoteType(t.output)!.bareType,
      operands: arguments, site: site)
  }

}
