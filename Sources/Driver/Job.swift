/// The description of an individual sub-process.
public protocol Job {

  /// Executes the job.
  ///
  /// - Parameter driver: The driver that executes the job.
  func run(with driver: inout Driver) throws

}
