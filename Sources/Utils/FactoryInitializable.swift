/// Classes whose initializers actually create derived classes
public protocol FactoryInitializable {

  /// The type of the least-derived class declared to be FactoryInitializable.
  ///
  /// - Warning: Do not define this in your FactoryInitializable type!
  associatedtype FactoryBase: Sendable,  AnyObject, FactoryInitializable = Self

  // This associatedtype is a trick that captures `Self` at the point where
  // `FactoryInitializable` enters a class hierarchy; in other contexts, `Self`
  // refers to the most-derived type.

}

extension FactoryInitializable where Self: AnyObject {

  /// Optimally “creates” an instance that is just another reference to `me`.
  ///
  /// - Requires: `me is Self`.
  ///
  /// Taking `FactoryBase` as a parameter prevents, at compile-time, the
  /// category of bugs where `me` is not derived from the least-derived ancestor
  /// of `Self` conforming to `FactoryInitializable`.
  ///
  /// However, there are still ways `me` might not be derived from `Self`.  If
  /// you have factory initializers at more than one level of your class
  /// hierarchy and you can't control exactly what is passed here, use
  /// `init(aliasing:)` instead.
  public init(unsafelyAliasing me: FactoryBase) {
    self = unsafeDowncast(me, to: Self.self)
  }

  /// Safely “creates” an instance that is just another reference to `me`.
  ///
  /// - Requires: `me is Self`.
  public init(aliasing me: FactoryBase) {
    self = me as! Self
  }

}
