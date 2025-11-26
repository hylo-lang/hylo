/// A type that can provide an ANSI color syntax-highlighted description of itself.
///
/// Types conforming to this protocol provide a `coloredDescription` property that returns
/// a string representation with ANSI escape codes for terminal display.
public protocol ColoredDescribable {
  
  /// An ANSI color syntax-highlighted description of this instance.
  var coloredDescription: String { get }
  
}
