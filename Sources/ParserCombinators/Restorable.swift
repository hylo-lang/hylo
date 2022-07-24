/// A type whose notional value can restored.
public protocol Restorable {

  /// An type capturing the saliant parts of `Self`.
  associatedtype Backup

  /// Creates a backup of `self`.
  func backup() -> Backup

  /// Restore the value of `self` from `backup`.
  mutating func restore(from backup: Backup)

}

extension Restorable where Backup == Self {

  public func backup() -> Self { self }

  public mutating func restore(from backup: Self) {
    self = backup
  }

}
