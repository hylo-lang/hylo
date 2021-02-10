/// An error that occurred while running the driver.
public enum DriverError: Error {

  /// Occurs when the driver attempts to load a module with the name that was already bound to
  /// another module.
  case moduleAlreadyLoaded(moduleID: String)

  /// Occurs when the driver attempts to retrieve a module that does not exists in its context.
  case moduleNotFound(moduleID: String)

}
