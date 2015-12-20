// You might want to turn this down to a really low number when running this in a playground.
let numberOfIterations = 10

import Foundation

protocol Smaller {
    func smaller() -> Self?
}

protocol Arbitrary: Smaller {
    static func arbitrary() -> Self
}

struct ArbitraryI<T> {
    let arbitrary: () -> T
    let smaller: T -> T?
}

func checkHelper<A>(arbitraryInstance: ArbitraryI<A>,
    prop: A -> Bool, message: String) -> () {
        
        for _ in 0..<numberOfIterations {
            let value = arbitraryInstance.arbitrary()
            if !prop(value) {
                let smallerValue = iterateWhile({ !prop($0) },
                    initialValue: value, next: arbitraryInstance.smaller)
                print("\"\(message)\" doesn't hold: \(smallerValue)")
                return
            }
        }
        print("\"\(message)\" passed \(numberOfIterations) tests.")
}

func check<X: Arbitrary>(message: String, prop: X -> Bool) -> () {
    let instance = ArbitraryI(arbitrary: { X.arbitrary() }, smaller: { $0.smaller() })
    checkHelper(instance, prop: prop, message: message)
}

func check<X: Arbitrary, Y: Arbitrary>(message: String, prop: (X, Y) -> Bool) -> () {
    let arbitraryTuple = { (X.arbitrary(), Y.arbitrary()) }
    let smaller: (X, Y) -> (X, Y)? = { (x, y) in
        if let newX = x.smaller() {
            if let newY = y.smaller() {
                return (newX, newY)
            }
        }
        return nil
    }
    
    let instance = ArbitraryI(arbitrary: arbitraryTuple, smaller: smaller)
    checkHelper(instance, prop: prop, message: message)
}

extension Int: Smaller {
    func smaller() -> Int? {
        return self == 0 ? nil : self / 2
    }
}

extension Int: Arbitrary {
    static func arbitrary() -> Int {
        return Int(arc4random())
    }
}

extension String: Smaller {
    func smaller() -> String? {
        return isEmpty ? nil : String(self.characters.dropFirst())
    }
}

extension CGSize: Arbitrary {
    func smaller() -> CGSize? {
        return nil
    }
    
    static func arbitrary() -> CGSize {
        return CGSizeMake(CGFloat.arbitrary(), CGFloat.arbitrary())
    }
}

extension CGFloat: Arbitrary {
    func smaller() -> CGFloat? {
        return nil
    }
    
    static func arbitrary() -> CGFloat {
        let random: CGFloat = CGFloat(arc4random())
        let maxUint = CGFloat(UInt32.max)
        return 10000 * ((random - maxUint/2) / maxUint)
    }
}

func plusIsCommutative(x: Int, y: Int) -> Bool {
    return x + y == y + x
}

func minusIsCommutative(x: Int, y: Int) -> Bool {
    return x - y == y - x
}

extension Character: Smaller {
    func smaller() -> Character? {
        return self
    }
}

extension Character: Arbitrary {
    static func arbitrary() -> Character {
        return Character(UnicodeScalar(Int.random(from: 65, to: 90)))
    }
}

func tabulate<A>(times: Int, transform: Int -> A) -> [A] {
    return (0..<times).map(transform)
}

extension Int {
    static func random(from from: Int, to: Int) -> Int {
        return from + (Int(arc4random()) % (to - from))
    }
}

extension String: Arbitrary {
    static func arbitrary() -> String {
        let randomLength = Int.random(from: 0, to: 40)
        let randomCharacters = tabulate(randomLength) { _ in
            Character.arbitrary()
        }
        return String(randomCharacters)
    }
}

func iterateWhile<A>(condition: A -> Bool,
    initialValue: A,
    next: A -> A?) -> A {
        if let x = next(initialValue) where condition(x) {
            return iterateWhile(condition, initialValue: x, next: next)
        }
    return initialValue
}

func check2<A: Arbitrary>(message: String, prop: A -> Bool) -> () {
    for _ in 0..<numberOfIterations {
        let value = A.arbitrary()
        if !prop(value) {
            let smallerValue = iterateWhile({ !prop($0) }, initialValue: value) {
                $0.smaller()
            }
            print("\"\(message)\" doesn't hold: \(smallerValue)")
            return
        }
    }
    print("\"\(message)\" passed \(numberOfIterations) tests.")
}

func qsort(var array: [Int]) -> [Int] {
    if array.isEmpty { return [] }
    let pivot = array.removeAtIndex(0)
    let lesser = array.filter { $0 < pivot }
    let greater = array.filter { $0 >= pivot }
    return qsort(lesser) + [pivot] + qsort(greater)
}

extension Array: Smaller {
    func smaller() -> [Element]? {
        guard !isEmpty else { return nil }
        return Array(dropFirst())
    }
}

extension Array where Element: Arbitrary {
    static func arbitrary() -> [Element] {
        let randomLength = Int(arc4random() % 50)
        return tabulate(randomLength) { _ in Element.arbitrary() }
    }
}


func arbitraryArray<X: Arbitrary>() -> [X] {
    let randomLength = Int(arc4random() % 50)
    return tabulate(randomLength) { _ in return X.arbitrary() }
}

struct ArbitraryInstance<T> {
    let arbitrary: () -> T
    let smaller: T -> T?
}

func checkHelper<A>(arbitraryInstance: ArbitraryInstance<A>,
    _ property: A -> Bool, _ message: String) -> ()
{
    for _ in 0..<numberOfIterations {
        let value = arbitraryInstance.arbitrary()
        guard property(value) else {
            let smallerValue = iterateWhile({ !property($0) }, initialValue: value, next: arbitraryInstance.smaller)
            print("\"\(message)\" doesn't hold: \(smallerValue)")
            return
        }
    }
    print("\"\(message)\" passed \(numberOfIterations) tests.")
}

func check<X: Arbitrary>(message: String, property: X -> Bool) -> () {
    let instance = ArbitraryInstance(arbitrary: X.arbitrary,
        smaller: { $0.smaller() })
    checkHelper(instance, property, message)
}

func check<X: Arbitrary>(message: String, _ property: [X] -> Bool) -> () {
    let instance = ArbitraryInstance(arbitrary: Array.arbitrary,
        smaller: { (x: [X]) in x.smaller() })
    checkHelper(instance, property, message)
}

