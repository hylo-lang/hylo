import class Foundation.Bundle

/// The root URL of Hylo's core library.
public let core = Bundle.module.url(forResource: "Hylo/Core", withExtension: nil)

/// The root URL of Hylo's standard library.
public let standardLibrary = Bundle.module.url(forResource: "Hylo", withExtension: nil)
