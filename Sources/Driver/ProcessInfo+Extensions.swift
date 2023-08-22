import Foundation

extension ProcessInfo {

  static let ansiTerminalIsConnected = !["", "dumb", nil].contains(processInfo.environment["TERM"])

}
