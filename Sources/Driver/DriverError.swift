/// An error that occurred while running the driver.
public enum DriverError: Error {

  /// The given module could not be loaded because its name was already bound to another module.
  case moduleAlreadyLoaded(String)

  /// The given module could not be retrieved because it does not exist in this context.
  case moduleNotFound(String)

  /// The given module could not be lowered because it hasn't been type checked.
  case moduleNotTypeChecked(String)

  /// The given module could not be lowered to verified VIL.
  case moduleLoweringFailed(String)

}
