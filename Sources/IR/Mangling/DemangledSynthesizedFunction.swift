import Utils

/// The demangled description of a synthesized function.
public struct DemangledSynthesizedFunction: Hashable {

  /// The kind of a synthesized declaration.
  public indirect enum Kind: Hashable {

    /// A deinitializer.
    case deinitialize

    /// A move-initialization method.
    case moveInitialization

    /// A move-assignment method.
    case moveAssignment

    /// A copy method.
    case copy

    /// An equality method.
    case equal

    /// A global initializer for a binding declaration.
    case globalInitialization(DemangledSymbol)

    /// Lambda generated for an autoclosure argument.
    case autoclosure(Int)

  }

  /// The type of this declaration.
  public let type: DemangledType

  /// The scope in which the declaration is defined.
  public let scope: Indirect<DemangledSymbol>

  /// The kind of the declaration.
  public let kind: Kind

}

extension DemangledSynthesizedFunction: CustomStringConvertible {

  public var description: String {
    "\(scope).\(kind)"
  }

}
