//: Playground - noun: a place where people can play

import Cocoa

let cities = ["Paris": 2243, "Madrid": 3216, "Amsterdam": 881, "Berlin": 3397]

//needs to be wrapped
//let madridPopulation: Int = cities["Madrid"]

let madridPopulation: Int? = cities["Madrid"]

if madridPopulation != nil {
    print("The population of Madrid is " +
        "\(madridPopulation! * 1000)")
} else {
    print("Unknown city: Madrid")
}


if let madridPopulation = cities["Madrid"] {
    print("The population of Madrid is " +
        "\(madridPopulation * 1000)")
} else {
    print("Unknown city: Madrid")
}

/*
infix operator ?? { associativity right }

func ??<T>(optional: T?, defaultValue: T) -> T {
    if let x = optional {
        return x
    } else {
        return defaultValue
    }
}

func ??<T>(optional: T?, defaultValue: () -> T) -> T {
    if let x = optional {
        return x
    } else {
        return defaultValue()
    }
}
*/

infix operator ?? { associativity right precedence 110 }
func ??<T>(optional: T?,
    @autoclosure defaultValue: () -> T) -> T {
        if let x = optional { return x
        } else {
            return defaultValue()
        }
}

struct Order {
    let orderNumber: Int
    let person: Person? // ...
}

struct Person {
    let name: String
    let address: Address? // ...
}

struct Address {
    let streetName: String
    let city: String
    let state: String?
    // ...
}

/* ugly
￼order.person!.address!.state!

￼if let myPerson = order.person {
    if let myAddress = myPerson.address {
        if let myState = myAddress.state { // ...
*/

/* better
if let myState = order.person?.address?.state {
    print("This order will be shipped to \(myState)")
} else {
    print("Unknown person, address, or state.")
}
*/

switch madridPopulation {
    case 0?: print("Nobody in Madrid")
    case (1..<1000)?: print("Less than a million in Madrid")
    case .Some(let x): print("\(x) people in Madrid")
    case .None: print("We don't know about Madrid")
}

func populationDescriptionForCity(city: String) -> String? {
    guard let population = cities[city] else { return "Unknown city: \(city)" }
    return "The population of Madrid is \(population * 1000)"
}

populationDescriptionForCity("Madrid")

 func incrementOptional(optional: Int?) -> Int? {
    guard let x = optional else { return nil }
    return x + 1
}

extension Optional {
    func map<U>(transform: Wrapped -> U) -> U? {
        guard let x = self else { return nil }
        return transform(x)
    }
}

func incrementOptional2(optional: Int?) -> Int? {
    return optional.map { x in x + 1 }
}

/* invalid code
let x: Int? = 3
let y: Int? = nil
let z: Int? = x + y
*/

func addOptionals(optionalX: Int?, optionalY: Int?) -> Int? {
    if let x = optionalX {
        if let y = optionalY {
            return x + y
        }
    }
    return nil
}

/* multiple optionals binding
func addOptionals(optionalX: Int?, optionalY: Int?) -> Int? {
    if let x = optionalX, y = optionalY {
        return x + y
    }
    return nil
}
*/

/* use of guard
func addOptionals(optionalX: Int?, optionalY: Int?) -> Int? {
    guard let x = optionalX, y = optionalY else { return nil }
    return x + y
}
*/

let capitals = ["France": "Paris", "Spain": "Madrid", "The Netherlands": "Amsterdam", "Belgium": "Brussels"]

func populationOfCapital(country: String) -> Int? {
    guard let capital = capitals[country], population = cities[capital] else { return nil }
    return population * 1000
}

extension Optional {
    func flatMap<U>(f: Wrapped -> U?) -> U? {
        guard let x = self else { return nil }
        return f(x)
    }
}

func addOptionals2(optionalX: Int?, optionalY: Int?) -> Int? {
    return optionalX.flatMap { x in
        optionalY.flatMap { y in
            x+y
        }
    }
}

func populationOfCapital2(country: String) -> Int? {
    return capitals[country].flatMap { capital in
        cities[capital].flatMap { population in
            population * 1000
        }
    }
}

func populationOfCapital3(country: String) -> Int? {
    return capitals[country].flatMap { capital in
        cities[capital] }.flatMap { population in
            population * 1000
    }
}