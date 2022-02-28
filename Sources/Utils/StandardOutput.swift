public struct StandardOutput: TextOutputStream {

  public init() {}

  public func write(_ string: String) {
    Swift.print(string, terminator: "")
  }

}
