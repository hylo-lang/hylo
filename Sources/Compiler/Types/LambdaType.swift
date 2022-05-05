import Utils

/// The type of a lambda.
public struct LambdaType: TypeProtocol, Hashable {

  public let environment: Type

  public let inputs: [CallableTypeParameter]

  public let output: Type

  public let flags: TypeFlags

  public init(environment: Type = .unit, inputs: [CallableTypeParameter], output: Type) {
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
