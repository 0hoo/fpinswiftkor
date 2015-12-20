import Foundation

enum Encoding {
    case ASCII
    case NEXTSTEP
    case JapaneseEUC
    case UTF8
}

func toNSStringEncoding(encoding: Encoding) -> NSStringEncoding {
    switch encoding {
        case .ASCII:
            return NSASCIIStringEncoding
        case .NEXTSTEP:
            return NSNEXTSTEPStringEncoding
        case .JapaneseEUC:
            return NSJapaneseEUCStringEncoding
        case .UTF8:
            return NSUTF8StringEncoding
    }
}

func createEncoding(enc: NSStringEncoding) -> Encoding? {
    switch enc {
        case NSASCIIStringEncoding:
            return .ASCII
        case NSNEXTSTEPStringEncoding:
            return .NEXTSTEP
        case NSJapaneseEUCStringEncoding:
            return .JapaneseEUC
        case NSUTF8StringEncoding:
            return .UTF8
        default:
            return nil
    }
}

func localizedEncodingName(encoding: Encoding) -> String {
    return .localizedNameOfStringEncoding(
        toNSStringEncoding(encoding))
}

let cities = [
    "Paris": 2243, "Madrid": 3216, "Amsterdam": 881, "Berlin": 3397
]

let capitals = ["France": "Paris", "Spain": "Madrid", "The Netherlands": "Amsterdam",
    "Belgium": "Brussels"]

enum LookupError: ErrorType {
    case CapitalNotFound
    case PopulationNotFound
}

enum PopulationResult {
    case Success(Int)
    case Error(LookupError)
}

let exampleSuccess: PopulationResult = .Success(1000)

func populationOfCapital(country: String) -> PopulationResult {
    guard let capital = capitals[country] else {
        return .Error(.CapitalNotFound)
    }
    
    guard let population = cities[capital] else {
        return .Error(.PopulationNotFound)
    }
    
    return .Success(population)
}

switch populationOfCapital("France") {
    case let .Success(population):
        print("France's capital has \(population) thousand inhabitants")
    case let .Error(error):
        print("Error: \(error)")
}


let mayors = [
    "Paris": "Hidalgo", "Madrid": "Carmena", "Amsterdam": "van der Laan", "Berlin": "Müller"
]

func mayorOfCapital(country: String) -> String? {
    return capitals[country].flatMap { mayors[$0] }
}

enum MayorResult {
    case Success(Int)
    case Error(ErrorType)
}

enum Result<T> {
    case Success(T)
    case Error(ErrorType)
}

func populationOfCapital1(country: String) throws -> Int {
    guard let capital = capitals[country] else {
        throw LookupError.CapitalNotFound
    }
    
    guard let population = cities[capital] else {
        throw LookupError.PopulationNotFound
    }
    
    return population
}

do {
    let population = try populationOfCapital1("France")
    print("France's population is \(population)")
} catch {
    print("Lookup error: \(error)")
}

func ??<T>(result: Result<T>, handleError: ErrorType -> T) -> T {
    switch result {
    case let .Success(value):
        return value
    case let .Error(error):
        return handleError(error)
    }
}

enum Add<T, U> {
    case InLeft(T)
    case InRight(U)
}

enum Zero { }

struct Times<T, U> {
    let fst: T
    let snd: U
}

typealias One = ()