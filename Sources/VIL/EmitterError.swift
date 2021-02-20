/// An error that occured during VIL code emission.
public enum EmitterError: Error {

  case immutableLocation

  case immutableSelf

  case immutableExpr

}
