import FrontEnd

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

extension Function {

  /// Creates a `project_bundle` anchored at `site` that applies one of the `variants` defined in `m`,
  /// of type `t`, to arguments `a`, canonicalizing types in `scopeOfUse`.
  func makeProjectBundle(
    applying m: BundleReference<SubscriptDecl>,
    ofType t: ArrowType,
    to a: [Operand],
    with variants: [AccessEffect: Function.ID] = [:],
    at site: SourceRange,
    canonicalizingTypesIn scopeOfUse: AnyScopeID
  ) -> ProjectBundle {
    precondition(!variants.isEmpty)
    return .init(
      bundle: m, variants: variants,
      parameters: t.inputs.lazy.map({ ParameterType($0.type)! }),
      projection: RemoteType(t.output)!.bareType,
      operands: a, site: site)
  }

  /// Creates a `project_bundle` anchored at `site` that applies one of the `variants` defined in `m`,
  /// of type `t`, to arguments `a`, canonicalizing types in `scopeOfUse`, inserting it at `p`.
  mutating func makeProjectBundle(
    applying m: BundleReference<SubscriptDecl>,
    ofType t: ArrowType,
    to a: [Operand],
    with variants: [AccessEffect: Function.ID] = [:],
    at site: SourceRange,
    canonicalizingTypesIn scopeOfUse: AnyScopeID,
    insertingAt p: InsertionPoint
  ) -> InstructionID {
    insert(
      makeProjectBundle(
        applying: m, ofType: t, to: a, with: variants, at: site,
        canonicalizingTypesIn: scopeOfUse), at: p)
  }

}
