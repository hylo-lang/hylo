/// An error that occurred while running the driver.
public enum DriverError: Error {

  /// Occurs when the driver attempts to load a module with the name that was already bound to
  /// another module.
  case moduleAlreadyLoaded(moduleName: String)

}
