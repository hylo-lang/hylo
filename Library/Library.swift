import class Foundation.Bundle

/// The root URL of Val's core library.
public let core = Bundle.module.url(forResource: "Val/Core", withExtension: nil)

/// The root URL of Val's standard library.
public let standardLibrary = Bundle.module.url(forResource: "Val", withExtension: nil)

/// The root URL of Val's support library for C++ transpiled sources.
public let cxxSupport = Bundle.module.url(forResource: "CXX/Support", withExtension: nil)
