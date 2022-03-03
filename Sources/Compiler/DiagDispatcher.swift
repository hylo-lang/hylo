import Utils

/// A singleton dispatching in-flight diagnostics to registered consumers.
public struct DiagDispatcher {

  /// A set of diagnostic consumers.
  private var consumers: [Int: DiagConsumer] = [:]

  /// A factory for consumer handles.
  private var handleFactory = AutoIncrementFactory()

  private init() {}

  /// Registers a diagnostic consumer and returns a handle on it.
  @discardableResult
  public mutating func register(consumer: DiagConsumer) -> Int {
    let id = handleFactory.makeID()
    consumers[id] = consumer
    return id
  }

  /// Unregister a diagnostic consumer.
  @discardableResult
  public mutating func unregister(consumer id: Int) -> DiagConsumer? {
    defer { consumers[id] = nil }
    return consumers[id]
  }

  /// Dispatches an in-flight diagnostic to all registered consumers.
  public mutating func report(_ diag: Diag) {
    for key in consumers.keys {
      consumers[key]!.consume(diag)
    }
  }

  /// The shared instance of the dispatcher,
  public static var instance = DiagDispatcher()

}
