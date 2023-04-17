import class Foundation.Bundle

/// The root URL of Val's core library.
public let core = Bundle.module.url(forResource: "Core", withExtension: nil)

/// The root URL of Val's support library.
public let support = Bundle.module.url(forResource: "Core", withExtension: nil)

/// The root URL of Val's support library for C++ transpiled sources.
public let cxxSupport = Bundle.module.url(forResource: "CXX/Support", withExtension: nil)
