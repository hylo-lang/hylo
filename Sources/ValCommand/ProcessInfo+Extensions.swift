import Foundation

extension ProcessInfo {

  static let terminalIsConnected = processInfo.environment["TERM"] != nil

}
