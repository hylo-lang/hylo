import FrontEnd
import Utils

extension Function {

  /// The global identity of an IR function.
  public struct ID: Hashable {

    /// The value of a function IR identity.
    public enum Value: Hashable {

      /// The identity of a lowered function, initializer, method variant, or subscript variant.
      case lowered(AnyDeclID)

      /// The identity of a synthesized declaration.
      case synthesized(SynthesizedFunctionDecl)

      /// The identity of an existentialized function.
      indirect case existentialized(base: ID)

      /// The identity of a monomorphized function.
      indirect case monomorphized(base: ID, arguments: GenericArguments)

    }

    /// The value of this identity.
    public let value: Value

    /// Creates the identity of the lowered form of `d`, which is the declaration of a function,
    /// initializer, method implementation, or subscript implementation.
    init<T: DeclID>(_ d: T) {
      switch d.kind {
      case FunctionDecl.self, InitializerDecl.self, MethodImpl.self, SubscriptImpl.self:
        self.value = .lowered(AnyDeclID(d))
      default:
        unreachable()
      }
    }

    /// Creates the identity of the lowered form of `s`.
    init(_ s: SynthesizedFunctionDecl) {
      precondition(s.type.isCanonical)
      self.value = .synthesized(s)
    }

    /// Creates the identity of the existentialized form of `base`.
    init(existentialized base: Function.ID) {
      self.value = .existentialized(base: base)
    }

    /// Creates the identity of the monomorphized form of `base` for `arguments`.
    ///
    /// - Requires: `arguments` is not empty.
    init(monomorphized base: Function.ID, for arguments: GenericArguments) {
      precondition(!arguments.isEmpty)
      precondition(arguments.allSatisfy(\.value.isCanonical))
      self.value = .monomorphized(base: base, arguments: arguments)
    }

    /// `true` if `self` is the identity of a synthesized function.
    public var isSynthesized: Bool {
      if case .synthesized = value {
        return true
      } else {
        return false
      }
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
    case .synthesized(let d):
      return d.description
    case .existentialized(let b):
      return "\(b).existentialized"
    case .monomorphized(let b, let a):
      return "<\(list: a.values)>(\(b))"
    }
  }

}
