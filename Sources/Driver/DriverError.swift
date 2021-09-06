/// An error that occurred while running the driver.
public enum DriverError: Error {

  /// Occurs when the driver attempts to load a module with the name that was already bound to
  /// another module.
  case moduleAlreadyLoaded(moduleName: String)

  /// Occurs when the driver attempts to retrieve a module that does not exists in its context.
  case moduleNotFound(moduleName: String)

  /// Occurs when the driver attempts to lower a module that has not been type checked.
  case moduleNotTypeChecked(moduleName: String)

  /// Occurs when the driver failed to lower a module to verified VIL.
  case moduleLoweringFailed(moduleName: String)

}
