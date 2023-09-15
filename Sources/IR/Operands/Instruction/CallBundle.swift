import Core
import Utils

/// Invokes one variant of `bundle` with `arguments` and writes its result to `output`.
public struct CallBundle: Instruction {

  /// The method bundle implementing the variant to call.
  public let bundle: BundleReference<MethodDecl>

  /// The type of the bundle.
  public let bundleType: MethodType

  /// The variants of the bundle.
  public let variants: [AccessEffect: Function.ID]

  /// The return storage and arguments of the call.
  public private(set) var operands: [Operand]

  /// The site of the code corresponding to that instruction.
  public let site: SourceRange

  /// Creates an instance with the given properties.
  fileprivate init(
    bundle: BundleReference<MethodDecl>,
    bundleType: MethodType,
    variants: [AccessEffect: Function.ID],
    output: Operand,
    arguments: [Operand],
    site: SourceRange
  ) {
    self.bundle = bundle
    self.bundleType = bundleType
    self.variants = variants
    self.operands = [output] + arguments
    self.site = site
  }

  /// The capabilities possibly requested on the receiver.
  public var capabilities: AccessEffectSet {
    .init(variants.keys)
  }

  /// The location at which the result of `callee` is stored.
  public var output: Operand { operands[0] }

  /// The arguments of the call.
  public var arguments: ArraySlice<Operand> { operands[1...] }

  public mutating func replaceOperand(at i: Int, with new: Operand) {
    operands[i] = new
  }

}

extension CallBundle: CustomStringConvertible {

  public var description: String {
    "call_bundle \(capabilities) \(bundle)(\(list: arguments)) to \(output)"
  }

}

extension Module {

  /// Creates a `call_bundle` anchored at `site` that calls one of the variants in `bundle` on
  /// `arguments`.
  mutating func makeCallBundle(
    applying bundle: ScopedValue<BundleReference<MethodDecl>>, to arguments: [Operand],
    writingResultTo output: Operand,
    at site: SourceRange
  ) -> CallBundle {
    var variants: [AccessEffect: Function.ID] = [:]
    for v in program[bundle.value.bundle].impls {
      variants[program[v].introducer.value] = demandDeclaration(lowering: v)
    }

    let bundleType = program.canonicalType(
      of: bundle.value.bundle, specializedBy: bundle.value.arguments, in: bundle.scope)
    let t = MethodType(bundleType)!

    precondition((t.inputs.count + 1) == arguments.count)
    precondition(arguments.allSatisfy({ self[$0] is Access }))
    precondition(isBorrowSet(output))

    return .init(
      bundle: bundle.value, bundleType: t, variants: variants,
      output: output, arguments: arguments,
      site: site)
  }

}
