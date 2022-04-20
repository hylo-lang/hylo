import Utils

/// The type of a lambda.
public struct LambdaType: TypeProtocol, Hashable {

  /// A parameter in a lambda type.
  public struct Parameter: Hashable {

    /// The label of the parameter.
    public let label: String?

    /// The type of the parameter.
    public let type: Type

  }

  /// The environment of the lambda.
  public let environment: Type

  /// The input types of the lambda.
  public let inputs: [Parameter]

  /// The output type of the lambda.
  public let output: Type

  public let flags: TypeFlags

  public init(environment: Type = .unit, inputs: [Parameter], output: Type) {
    self.environment = environment
    self.inputs = inputs
    self.output = output

    var fs = environment.flags
    inputs.forEach({ fs.merge($0.type.flags) })
    fs.merge(output.flags)
    flags = fs
  }

}

extension LambdaType: CustomStringConvertible {

  public var description: String {
    let e = (environment == .unit) ? "thin" : "[\(environment)]"
    let i = String.joining(inputs, separator: ", ")
    let o = "\(output)"
    return "\(e) (\(i)) -> \(o)"
  }


}

extension LambdaType.Parameter: CustomStringConvertible {

  public var description: String {
    if let label = label {
      return "\(label): \(type)"
    } else {
      return "\(type)"
    }
  }

}
