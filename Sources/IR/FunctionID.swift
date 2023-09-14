import Core

extension Function {

  /// The global identity of an IR function.
  public struct ID: Hashable {

    /// The value of a function IR identity.
    public enum Value: Hashable {

      /// The identity of a lowered Hylo function, initializer, or method variant.
      case lowered(AnyDeclID)

      /// The identity of a lowered subscript variant.
      case loweredSubscript(SubscriptImpl.ID)

      /// The identity of a synthesized declaration.
      case synthesized(SynthesizedFunctionDecl)

      /// The identity of an existentialized function.
      indirect case existentialized(base: ID)

      /// The identity of a monomorphized function.
      indirect case monomorphized(base: ID, arguments: GenericArguments)

    }

    /// The value of this identity.
    public let value: Value

    /// Creates the identity of the lowered form of `f`.
    public init(_ f: FunctionDecl.ID) {
      self.value = .lowered(AnyDeclID(f))
    }

    /// Creates the identity of the lowered form of `f` used as an initializer.
    public init(_ f: InitializerDecl.ID) {
      self.value = .lowered(AnyDeclID(f))
    }

    /// Creates the identity of the lowered form of `s`.
    public init(_ s: SubscriptImpl.ID) {
      self.value = .loweredSubscript(s)
    }

    /// Creates the identity of the lowered form of `s`.
    public init(_ s: SynthesizedFunctionDecl) {
      self.value = .synthesized(s)
    }

    /// Creates the identity of the existentialized form of `base`.
    public init(existentialized base: Function.ID) {
      self.value = .existentialized(base: base)
    }

    /// Creates the identity of the monomorphized form of `base` for `arguments`.
    ///
    /// - Requires: `arguments` is not empty.
    public init(monomorphized base: Function.ID, for arguments: GenericArguments) {
      precondition(!arguments.isEmpty)
      self.value = .monomorphized(base: base, arguments: arguments)
    }

    /// `true` if `self` is the identity of a monomorphized function.
    public var isMonomorphized: Bool {
      switch value {
      case .monomorphized:
        return true
      default:
        return false
      }
    }

  }

}

extension Function.ID: CustomStringConvertible {

  public var description: String {
    switch value {
    case .lowered(let d):
      return d.description
    case .loweredSubscript(let d):
      return d.description
    case .synthesized(let d):
      return d.description
    case .existentialized(let b):
      return "\(b).existentialized"
    case .monomorphized(let b, let a):
      return "<\(list: a.values)>(\(b))"
    }
  }

}
