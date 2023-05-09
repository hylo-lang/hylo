import Core

extension Function {

  /// The global identity of an IR function.
  public struct ID: Hashable {

    /// The value of a function IR identity.
    public enum Value: Hashable {

      /// The identity of a lowered Val function, initializer, or method variant.
      case lowered(AnyDeclID)

      /// The identity of a lowered subscript variant.
      case loweredSubscript(SubscriptImpl.ID)

      /// The identity of a requirement synthesized for some type.
      ///
      /// The payload is a pair (D, U) where D is the declaration of a requirement and T is a type
      /// conforming to the trait defining D.
      case synthesized(AnyDeclID, for: AnyType)

      /// The identity of a monomorphized function.
      indirect case monomorphized(base: ID, arguments: GenericArguments)

    }

    /// The value of this identity.
    public let value: Value

    /// Creates the identity of the lowered form of `f`.
    public init(_ f: FunctionDecl.ID) {
      self.value = .lowered(AnyDeclID(f))
    }

    /// Creates the identity of the lowered form of `s`.
    public init(_ s: SubscriptImpl.ID) {
      self.value = .loweredSubscript(s)
    }

    /// Creates the identity of the lowered form of `f` used as an initializer.
    public init(initializer f: InitializerDecl.ID) {
      self.value = .lowered(AnyDeclID(f))
    }

    /// Creates the identity of synthesized requirement `r` for type `t`.
    public init<T: DeclID>(synthesized r: T, for t: AnyType) {
      self.value = .synthesized(AnyDeclID(r), for: t)
    }

    /// Creates the identity of the monomorphized form of `source` for `arguments`.
    ///
    /// - Requires: `arguments` is not empty.
    public init(monomorphized base: Function.ID, for arguments: GenericArguments) {
      precondition(!arguments.isEmpty)
      self.value = .monomorphized(base: base, arguments: arguments)
    }

  }

}

extension Function.ID: CustomStringConvertible {

  public var description: String {
    switch value {
    case .lowered(let d):
      return "\(d).lowered"
    case .loweredSubscript(let d):
      return "\(d).lowered"
    case .synthesized(let r, let t):
      return "\"synthesized \(r) for \(t)\""
    case .monomorphized(let b, let a):
      return "<\(list: a.values)>(\(b))"
    }
  }

}
