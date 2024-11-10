
/// The type of metric to report
public enum MeasurementType : Int, CaseIterable {
    case Lexer = 0
    case Parser
    case TypeChecker
    case IRLowering
    case Depolymorphize
    case IRConversion        
    case MandatoryPass    
    case Optimizations
    case EmitPhase
    case LinkPhase

    public static var count : Int{
        get {
            return MeasurementType.allCases.count
        }
    }
}


public struct TimeMeasurementProbe : ~Copyable {

    var beginning : ContinuousClock.Instant
    var measure_type : MeasurementType
    var pool : ProfilingMeasurements

    init(_ measure_type : MeasurementType, _ pool  : ProfilingMeasurements){
        self.beginning = ContinuousClock.Instant.now
        self.pool = pool
        self.measure_type = measure_type
    }

    deinit {
        let time : Duration = ContinuousClock.Instant.now - beginning
        pool.addProfiledDuration(self.measure_type, time)
    }

}

/// A container for profiling measurement
public class ProfilingMeasurements {

    public init() {}

    var cumulatedDurations : [Duration] = Array<Duration>(repeating: Duration.seconds(0), count: MeasurementType.count)

    public func createTimeMeasurementProbe(_ measure_type : MeasurementType) -> TimeMeasurementProbe {
        return TimeMeasurementProbe(measure_type, self)
    }

    public func addProfiledDuration(_ measure_type : MeasurementType, _ duration: Duration) -> Void{
        self.cumulatedDurations[measure_type.rawValue] += duration
    }

    public func printProfilingReport() -> Void{
        let profilingReport = """
            **Compile time profiling summary**
              Frontend:
                Lexer: \(formattedMeasurement(MeasurementType.Lexer))
                Parser: \(formattedMeasurement(MeasurementType.Parser))
                TypeChecker: \(formattedMeasurement(MeasurementType.TypeChecker))
              Backend:
                IR Lowering: \(formattedMeasurement(MeasurementType.IRLowering))
                Depolymorphize: \(formattedMeasurement(MeasurementType.Depolymorphize))
                LLVM IR Conversion: \(formattedMeasurement(MeasurementType.IRConversion))                   
                LLVM Mandatory pass: \(formattedMeasurement(MeasurementType.MandatoryPass))                
                LLVM Optimizations: \(formattedMeasurement(MeasurementType.Optimizations))
                LLVM Emit phase: \(formattedMeasurement(MeasurementType.EmitPhase))
                Linking phase: \(formattedMeasurement(MeasurementType.LinkPhase))

            """
        print(profilingReport)               
    }

    public func measurement(_ measure_type : MeasurementType) -> Duration{
        switch(measure_type){
            case .Parser:
                return cumulatedDurations[MeasurementType.Parser.rawValue] - cumulatedDurations[MeasurementType.Lexer.rawValue]
            default:
                return cumulatedDurations[measure_type.rawValue]                                                                        
        }
    }

    public func formattedMeasurement(_ measure_type : MeasurementType) -> String{
        let measure = measurement(measure_type)
        let measurementDouble = Double(measure.components.seconds) + Double(measure.components.attoseconds) / 1e18
        let unitArrays = [ "s", "ms", "us", "ns", "ps"]
        var factor = 1
        for i in 0...unitArrays.count {
            let scaledMeasurement = measurementDouble * Double(factor);
            if scaledMeasurement > 1.0 {
                return "\(String(format: "%.03f", scaledMeasurement)) \(unitArrays[i])"
            }
            factor *= 1000
        }
        return "N/A"
    }    
}
