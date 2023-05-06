/// An access effect, specifying how a parameter, receiver, or remote part is accessed
public enum AccessEffect: UInt8, Codable {

  /// Value is accessed immutably.
  case `let` = 1

  /// Value is assigned but never read.
  case `set` = 2

  /// Value is accessed mutably.
  case `inout` = 4

  /// Value is consumed.
  case sink = 8

  /// Value may be accessed with any of the other effects, depending on the context.
  case yielded = 16

}

extension AccessEffect: Comparable {

  public static func < (l: Self, r: Self) -> Bool {
    l.rawValue < r.rawValue
  }

}
