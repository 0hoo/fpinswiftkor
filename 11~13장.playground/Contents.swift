//: Playground - noun: a place where people can play

//11장

import Cocoa

class CountdownGenerator: GeneratorType {
    var element: Int
    init<T>(array: [T]) {
        self.element = array.count - 1
    }
    func next() -> Int? {
        return self.element < 0 ? nil : element--
    }
}

let xs = ["A", "B", "C"]
let generator = CountdownGenerator(array: xs)
while let i = generator.next() {
    print("Element \(i) of the array is \(xs[i])")
}

class PowerGenerator: GeneratorType {
    var power: NSDecimalNumber = 1
    let two: NSDecimalNumber = 2
    func next() -> NSDecimalNumber? {
        power = power.decimalNumberByMultiplyingBy(two)
        return power
    }
}

extension PowerGenerator {
    func findPower(predicate: NSDecimalNumber -> Bool) -> NSDecimalNumber {
        while let x = next() {
            if predicate(x) { return x }
        }
        return 0
    }
}

PowerGenerator().findPower { $0.integerValue > 1000 }

class FileLinesGenerator: GeneratorType {
    typealias Element = String
    var lines: [String] = []
    init(filename: String) throws {
        let contents: String = try String(contentsOfFile: filename)
        let newLine = NSCharacterSet.newlineCharacterSet()
        lines = contents
            .componentsSeparatedByCharactersInSet(newLine)
    }
    func next() -> Element? {
        guard !lines.isEmpty else { return nil }
        let nextLine = lines.removeAtIndex(0)
        return nextLine
    }
}

extension GeneratorType {
    mutating func find(predicate: Element -> Bool) -> Element? {
        while let x = self.next() { if predicate(x) {
            return x }
        }
        return nil
    }
}

class LimitGenerator<G: GeneratorType>: GeneratorType {
    var limit = 0
    var generator: G
    init(limit: Int, generator: G) {
        self.limit = limit
        self.generator = generator
    }
    func next() -> G.Element? {
        guard limit >= 0 else { return nil }
        limit--
        return generator.next()
    }
}


extension Int {
    func countDown() -> AnyGenerator<Int> {
        var i = self
        return anyGenerator { return i < 0 ? nil : i-- }
    }
}

func +<G: GeneratorType, H: GeneratorType where G.Element == H.Element> (
    var first: G, var second: H) -> AnyGenerator<G.Element>
{
    return anyGenerator {
        first.next() ?? second.next() }
}


struct ReverseSequence<T>: SequenceType {
    var array: [T]
    init(array: [T]) {
        self.array = array
    }
    func generate() -> CountdownGenerator {
        return CountdownGenerator(array: array)
    }
}

let reverseSequence = ReverseSequence(array: xs)
let reverseGenerator = reverseSequence.generate()

while let i = reverseGenerator.next() {
    print("Index \(i) is \(xs[i])")
}

for i in ReverseSequence(array: xs) {
    print("Index \(i) is \(xs[i])")
}

let reverseElements = ReverseSequence(array: xs).map { xs[$0] }
for x in reverseElements {
    print("Element is \(x)")
}


let three_: [Int] = Array(GeneratorOfOne(3))
let empty: [Int] = Array(GeneratorOfOne(nil))

func one<X>(x: X?) -> AnyGenerator<X> {
    return anyGenerator(GeneratorOfOne(x))
}

func one2<X>(x: X) -> AnyGenerator<X> {
    return anyGenerator(GeneratorOfOne(x))
}



indirect enum Tree<T> {
    case Leaf
    case Node(Tree<T>, T, Tree<T>)
}

extension Tree {
    var inOrder: AnyGenerator<T> {
        switch self {
        case Tree.Leaf:
            return anyGenerator { return nil }
        case let Tree.Node(left, x, right):
            return left.inOrder + one(x) + right.inOrder
        }
    }
}

protocol Smaller {
    func smaller() -> AnyGenerator<Self>
}

extension Array {
    func generateSmallerByOne() -> AnyGenerator<[Element]> {
        var i = 0
        return anyGenerator {
            guard i < self.count else { return nil }
            var result = self
            result.removeAtIndex(i)
            i++
            return result
        }
    }
}

[1, 2, 3].generateSmallerByOne()

Array([1, 2, 3].generateSmallerByOne())

//(본문에 없음)
extension Array {
    var decompose : (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

extension Array {
    func smaller1() -> AnyGenerator<[Element]> {
        guard let (head, tail) = self.decompose else { return one(nil) }
        return one(tail) + Array<[Element]>(tail.smaller1()).map { smallerTail in
            [head] + smallerTail
            }.generate()
    }
}

Array([1, 2, 3].smaller1())


extension Array where Element: Smaller {
    func smaller() -> AnyGenerator<[Element]> {
        guard let (head, tail) = self.decompose else { return one(nil) }
        let gen1 = one(tail).generate()
        let gen2 = Array<[Element]>(tail.smaller()).map { xs in
            [head] + xs }.generate()
        let gen3 = Array<Element>(head.smaller()).map { x in [x] + tail
            }.generate()
        return gen1 + gen2 + gen3
    }
}

//본문에 없음
extension Int: Smaller {
    func smaller() -> AnyGenerator<Int> {
        return self == 0 ? one(nil) : one(self / 2)
    }
}


Array([1, 2, 3].smaller())

func +<A>(l: AnySequence<A>, r: AnySequence<A>) -> AnySequence<A> {
    return AnySequence(l.generate() + r.generate())
}

let s = AnySequence([1, 2, 3]) + AnySequence([4, 5, 6])
print("First pass: ")
for x in s {
    print(x)
}
print("\nSecond pass:")
for x in s {
    print(x)
}


struct JoinedGenerator<A>: GeneratorType {
    typealias Element = A
    
    var generator: AnyGenerator<AnyGenerator<A>>
    var current: AnyGenerator<A>?
    
    init(_ g: AnyGenerator<AnyGenerator<A>>) {
        generator = g
        current = generator.next()
    }
    
    mutating func next() -> A? {
        if let c = current {
            if let x = c.next() {
                return x
            } else {
                current = generator.next()
                return next()
            }
        }
        return nil
    }
}

func map<A, B>(g: AnyGenerator<A>, f: A -> B) -> AnyGenerator<B> {
    return anyGenerator {
        g.next().map(f)
    }
}


func join<A>(s: AnySequence<AnySequence<A>>) -> AnySequence<A> {
    return AnySequence {
        JoinedGenerator(map(s.generate()) { g in
            g.generate()
            })
    }
}

func map<A, B>(s: AnySequence<A>, f: A -> B) -> AnySequence<B> {
    return AnySequence { map(s.generate(), f: f) }
}

func flatMap<A, B>(xs: AnySequence<A>, f: A -> AnySequence<B>) -> AnySequence<B> {
    return join(map(xs, f: f))
}

//12장


struct Parser<Token, Result> {
    let p: ArraySlice<Token> -> AnySequence<(Result, ArraySlice<Token>)>
}


func one<A>(x: A) -> AnySequence<A> {
    return AnySequence(GeneratorOfOne(x))
}

func none<A>() -> AnySequence<A> {
    return AnySequence(anyGenerator { nil } )
}

extension ArraySlice {
    var head: Element? {
        return self.isEmpty ? nil : self[0]
    }
    
    var tail: ArraySlice<Element> {
        if (self.isEmpty) {
            return self
        }
        return self[(self.startIndex+1)..<self.endIndex]
    }
    
    var decompose: (head: Element, tail: ArraySlice<Element>)? {
        return self.isEmpty ? nil
            : (self[self.startIndex], self.tail)
    }
}

func parseA() -> Parser<Character, Character> {
    let a: Character = "a"
    return Parser { x in
        guard let (head, tail) = x.decompose where head == a else {
            return none()
        }
        return one((a, tail))
    }
}

extension String {
    var slice: ArraySlice<Character> {
        let res = Array(self.characters)
        return res[0..<res.count]
    }
}

func testParser<A>(parser: Parser<Character, A>, _ input: String) -> String {
    
    var result: [String] = []
    for (x, s) in parser.p(input.slice) {
        result += ["Success, found \(x), remainder: \(Array(s))"]
    }
    
    return result.isEmpty ? "Parsing failed." : result.joinWithSeparator("\n")
}

func satisfy<Token>(condition: Token -> Bool) -> Parser<Token, Token> {
    return Parser { x in
        guard let (head, tail) = x.decompose where condition(head) else {
            return none()
        }
        return one((head, tail))
    }
}


func token<Token: Equatable>(t: Token) -> Parser<Token, Token> {
    return satisfy { $0 == t }
}

infix operator <|> { associativity right precedence 130 }
func <|> <Token, A>(l: Parser<Token, A>, r: Parser<Token, A>)
    -> Parser<Token, A> {
        return Parser { l.p($0) + r.p($0) }
}



func sequence<Token, A, B>(l: Parser<Token, A>,
    _ r: Parser<Token, B>)
    -> Parser<Token, (A, B)> {
        
        return Parser { input in
            let leftResults = l.p(input)
            return flatMap(leftResults) { a, leftRest in
                let rightResults = r.p(leftRest)
                return map(rightResults, f: { b, rightRest in
                    ((a, b), rightRest)
                })
            }
        }
}

func sequence3<Token, A, B, C>(p1: Parser<Token, A>,
    _ p2: Parser<Token, B>,
    _ p3: Parser<Token, C>)
    -> Parser<Token, (A, B, C)> {
        
        return Parser { input in
            let p1Results = p1.p(input)
            return flatMap(p1Results) { a, p1Rest in
                let p2Results = p2.p(p1Rest)
                return flatMap(p2Results) {b, p2Rest in
                    let p3Results = p3.p(p2Rest)
                    return map(p3Results, f: { c, p3Rest in
                        ((a, b, c), p3Rest)
                    })
                }
            }
        }
}


func integerParser<Token>() -> Parser<Token, Character -> Int> {
    return Parser { input in
        return one(({ x in Int(String(x))! }, input))
    }
}


func combinator<Token, A, B>(l: Parser<Token, A -> B>, _ r: Parser<Token, A>)
    -> Parser<Token, B> {
        
        return Parser { input in
            let leftResults = l.p(input)
            return flatMap(leftResults) { f, leftRemainder in
                let rightResults = r.p(leftRemainder)
                return map(rightResults) { x, rightRemainder in
                    (f(x), rightRemainder)
                }
            }
        }
}

func pure<Token, A>(value: A) -> Parser<Token, A> {
    return Parser { one((value, $0)) }
}

func toInteger(c: Character) -> Int {
    return Int(String(c))!
}
let three:Character = "3"
testParser(combinator(pure(toInteger), token(three)), "3")


func toInteger2(c1: Character)(c2: Character) -> Int {
    let combined = String(c1) + String(c2)
    return Int(combined)!
}


testParser(combinator(combinator(pure(toInteger2), token(three)), token(three)), "33")


infix operator <*> { associativity left precedence 150 }
func <*><Token, A, B>(l: Parser<Token, A -> B>,
    r: Parser<Token, A>) -> Parser<Token, B> {
        
        return Parser { input in
            let leftResults = l.p(input)
            return flatMap(leftResults) { f, leftRemainder in
                let rightResults = r.p(leftRemainder)
                return map(rightResults) { x, y in (f(x), y) }
            }
        }
}

testParser(pure(toInteger2) <*> token(three) <*> token(three), "33")



func member(set: NSCharacterSet, character: Character) -> Bool {
    let unichar = (String(character) as NSString).characterAtIndex(0)
    return set.characterIsMember(unichar)
}

func characterFromSet(set: NSCharacterSet)
    -> Parser<Character, Character> {
        
        return satisfy { return member(set, character: $0) }
}



func lazy<Token, A>(f: () -> Parser<Token, A>)
    -> Parser<Token, A> {
        
        return Parser { x in f().p(x) }
}

func prepend<A>(l: A) -> [A] -> [A] {
    return { (x: [A]) in [l] + x }
}


func zeroOrMore<Token, A>(p: Parser<Token, A>) -> Parser<Token, [A]> {
    return (pure(prepend) <*> p <*> lazy { zeroOrMore(p) } ) <|> pure([])
}

func oneOrMore<Token, A>(p: Parser<Token, A>) -> Parser<Token, [A]> {
    return pure(prepend) <*> p <*> zeroOrMore(p)
}

let decimals = NSCharacterSet.decimalDigitCharacterSet()
let decimalDigit = characterFromSet(decimals)

let number = pure { characters in Int(String(characters))! } <*> oneOrMore(decimalDigit)
infix operator </> { precedence 170 }
func </> <Token, A, B>(l: A -> B, r: Parser<Token, A>) -> Parser<Token, B> {
    return pure(l) <*> r
}

let plus: Character = "+"
func add(x: Int)(_: Character)(y: Int) -> Int
{
    return x + y
}
let parseAddition = add </> number <*> token(plus) <*> number

testParser(parseAddition, "41+1")

infix operator <* { associativity left precedence 150 }
func <* <Token, A, B>(p: Parser<Token, A>, q: Parser<Token, B>)
    -> Parser<Token, A> {
        return { x in { _ in x } } </> p <*> q }
infix operator *> { associativity left precedence 150 }
func *> <Token, A, B>(p: Parser<Token, A>, q: Parser<Token, B>)
    -> Parser<Token, B> {
        return { _ in { y in y } } </> p <*> q }

func curry<A, B, C>(f: (A, B) -> C) -> A -> B -> C {
    return { x in { y in f(x, y) } }
}

let multiply: Character = "*"
let parseMultiplication = curry(*) </> number <* token(multiply)
    <*> number
testParser(parseMultiplication, "8*8")






typealias Calculator = Parser<Character, Int>



func operator0(character: Character,
    evaluate: (Int, Int) -> Int,
    operand: Calculator) -> Calculator {
        
        return curry { evaluate($0, $1) } </> operand
            <* token(character) <*> operand
}
func pAtom0() -> Calculator { return number }
func pMultiply0() -> Calculator { return operator0("*", evaluate: *, operand: pAtom0()) }
func pAdd0() -> Calculator { return operator0("+", evaluate: +, operand: pMultiply0()) }
func pExpression0() -> Calculator { return pAdd0() }

func operator1(character: Character,
    evaluate: (Int, Int) -> Int,
    operand: Calculator) -> Calculator {
        
        let withOperator = curry { evaluate($0, $1) } </> operand
            <* token(character) <*> operand
        return withOperator <|> operand
}

func pAtom1() -> Calculator { return number }
func pMultiply1() -> Calculator { return operator1("*", evaluate: *, operand: pAtom1()) }
func pAdd1() -> Calculator { return operator1("+", evaluate: +, operand: pMultiply1()) }
func pExpression1() -> Calculator { return pAdd1() }

typealias Op = (Character, (Int, Int) -> Int)
let operatorTable: [Op] = [("*", *), ("/", /), ("+", +), ("-", -)]


func pExpression2() -> Calculator {
    return operatorTable.reduce(number) {
        (next: Calculator, op: Op) in
        operator1(op.0, evaluate: op.1, operand: next)
    }
}

//13장

infix operator </  { precedence 170 }
func </ <Token, A, B>(l: A,
    r: Parser<Token, B>) -> Parser<Token, A> {
        
        return pure(l) <* r
}

func optionallyFollowed<A>(l: Parser<Character, A>,
    r: Parser<Character, A -> A>)
    -> Parser<Character, A> {
        
        let apply: A -> (A -> A) -> A = { x in { f in f(x) } }
        return apply </> l <*> (r <|> pure { $0 })
}


func flip<A, B, C>(f: (B, A) -> C) -> (A, B) -> C {
    return { (x, y) in f(y, x) }
}


func op(character: Character,
    evaluate: (Int, Int) -> Int,
    operand: Calculator) -> Calculator {
        
        let withOperator = curry(flip(evaluate)) </ token(character)
            <*> operand
        return optionallyFollowed(operand, r: withOperator)
}


enum Token : Equatable { //== 필요
    case Number(Int)
    case Operator(String)
    case Reference(String, Int)
    case Punctuation(String)
    case FunctionName(String)
}

func const<A, B>(x: A) -> B -> A {
    return { _ in x }
}

func ==(lhs: Token, rhs: Token) -> Bool {
    switch (lhs,rhs) {
    case (.Number(let x), .Number(let y)):
        return x == y
    case (.Operator(let x), .Operator(let y)):
        return x == y
    case (.Reference(let row, let column), .Reference(let row1, let column1)):
        return row == row1 && column == column1
    case (.Punctuation(let x), .Punctuation(let y)):
        return x == y
    case (.FunctionName (let x), .FunctionName(let y)):
        return x == y
    default:
        return false
    }
}

func tokens<Token: Equatable>(t: [Token]) -> Parser<Token, [Token]> {
    if let (head, tail) = t.decompose {
        return prepend </> token(head) <*> tokens(tail)
    } else {
        return pure([])
    }
}

func string(s: String) -> Parser<Character, String> {
    return const(s) </> tokens(Array(s.characters))
}

func fail<Token, Result>() -> Parser<Token, Result> {
    return Parser { _ in none() }
}

func oneOf<Token, A>(parsers: [Parser<Token, A>]) -> Parser<Token, A> { //fail 필요
    return parsers.reduce(fail(), combine: <|>)
}

let pDigit = oneOf(Array(0...9).map { const($0) </> string("\($0)") })
func toNaturalNumber(digits: [Int]) -> Int { return digits.reduce(0) { $0 * 10 + $1 }
}
let naturalNumber = toNaturalNumber </> oneOrMore(pDigit)
let tNumber = { Token.Number($0) } </> naturalNumber


let operatorParsers = ["*", "/", "+", "-", ":"].map { string($0) }
let tOperator = { Token.Operator($0) } </> oneOf (operatorParsers)

let capitalSet = NSCharacterSet.uppercaseLetterCharacterSet()
let capital = characterFromSet(capitalSet)
let tReference = curry { Token.Reference(String($0), $1) } </> capital <*> naturalNumber
let punctuationParsers = ["(", ")"].map { string($0) }
let tPunctuation = { Token.Punctuation($0) }
    </> oneOf(punctuationParsers)

let tName = { Token.FunctionName(String($0)) } </> oneOrMore(capital)

let whitespaceSet = NSCharacterSet.whitespaceAndNewlineCharacterSet()
let whitespace = characterFromSet(whitespaceSet)
func ignoreLeadingWhitespace<A>(p: Parser<Character, A>) -> Parser<Character, A> {
    return zeroOrMore(whitespace) *> p
}

func tokenize() -> Parser<Character, [Token]> {
    let tokenParsers = [tNumber, tOperator, tReference,
        tPunctuation, tName]
    return zeroOrMore(ignoreLeadingWhitespace(oneOf(tokenParsers)))
}


indirect enum Expression {
    case Number(Int)
    case Reference(String, Int)
    case BinaryExpression(String, Expression, Expression)
    case FunctionCall(String, Expression)
}

typealias ExpressionParser = Parser<Token, Expression>

func optionalTransform<A, T>(f: T -> A?) -> Parser<T, A> {
    return { f($0)! } </> satisfy { f($0) != nil }
}

let pNumber: ExpressionParser = optionalTransform {
    guard case let .Number(number) = $0 else { return nil }
    return Expression.Number(number)
}

let pReference: ExpressionParser = optionalTransform {
    guard case let .Reference(column, row) = $0 else { return nil }
    return Expression.Reference(column, row)
}

let pNumberOrReference = pNumber <|> pReference

func eof<A>() -> Parser<A, ()> {
    return Parser { stream in
        if (stream.isEmpty) {
            return one(((), stream))
        }
        return none()
    }
}

func parse<A>(parser: Parser<Character, A>, _ input: String) -> A? { //eof필요
    for (result, _) in (parser <* eof()).p(input.slice) {
        return result
    }
    return nil
}

func parse<A, B>(parser: Parser<A, B>, _ input: [A]) -> B? {
    for (result, _) in (parser <* eof()).p(input[0..<input.count]) { //eof필요
        return result
    }
    return nil
}


parse(pNumberOrReference, parse(tokenize(), "42")!) //parse함수 필요

print(parse(pNumberOrReference, parse(tokenize(), "A5")!))

let pFunctionName: Parser<Token, String> = optionalTransform {
    guard case let .FunctionName(name) = $0 else { return nil }
    return name
}

func makeList(l: Expression, _ r: Expression) -> Expression {
    return Expression.BinaryExpression(":", l, r)
}

func makeListExpression(l: Expression, r: Expression) -> Expression {
    return Expression.BinaryExpression(":", l, r)
}

func curry<A, B, C, D>(f: (A, B, C) -> D) -> A -> B -> C -> D {
    return { x in { y in { z in f(x, y, z) } } }
}

func op(opString: String) -> Parser<Token, String> {
    return const(opString) </> token(Token.Operator(opString))
}

let pList: ExpressionParser = curry(makeList) </> pReference <* op(":") <*> pReference //op함수필요

func parenthesized<A>(p: Parser<Token, A>) -> Parser<Token, A> {
    return token(Token.Punctuation("(")) *> p <* token(Token.Punctuation(")"))
}

func makeFunctionCall(name: String, _ arg: Expression) -> Expression {
    return Expression.FunctionCall(name, arg)
}
let pFunctionCall = curry(makeFunctionCall) </> pFunctionName
    <*> parenthesized(pList)


parse(pFunctionCall, parse(tokenize(), "SUM(A1:A3)")!)

func combineOperands(first: Expression, rest: [(String, Expression)]) -> Expression {
    return rest.reduce(first, combine: { result, pair in
        let (op, exp) = pair
        return Expression.BinaryExpression(op, result, exp)
    })
}
let pMultiplier = curry { ($0, $1) } </> (op("*") <|> op("/")) <*> pPrimitive
let pProduct = curry(combineOperands) </> pPrimitive <*> zeroOrMore(pMultiplier)
let pSummand = curry { ($0, $1) } </> (op("-") <|> op("+")) <*> pProduct
let pSum = curry(combineOperands) </> pProduct <*> zeroOrMore(pSummand)


func expression() -> ExpressionParser {
    return pSum
}


let pParenthesizedExpression = parenthesized(lazy { expression() }) //pMultiplier,pProduct,pSummand  combineOperands, pSum, expression함수 필요
let pPrimitive = pNumberOrReference <|> pFunctionCall
    <|> pParenthesizedExpression



func parseExpression(input: String) -> Expression? {
    return parse(tokenize(), input).flatMap { parse(expression(), $0)
    }
}

enum Result {
    case IntResult(Int)
    case StringResult(String)
    case ListResult([Result])
    case EvaluationError(String)
}

typealias IntegerOperator = (Int, Int) -> Int
func lift(f: IntegerOperator) -> ((Result, Result) -> Result) {
    return { l, r in
        guard case let (.IntResult(x), .IntResult(y)) = (l,r) else {
            return .EvaluationError("Type error, " + "couldn't evaluate \(l, r)") }
        return .IntResult(f(x, y))
    }
}

func op(f: (Int, Int) -> Int) -> (Int, Int) -> Int {
    return f
}

let integerOperators: [String: IntegerOperator] = [ "+": op(+), "/": op(/), "*": op(*), "-": op(-) ] //op필요

func evaluateIntegerOperator(op: String, _ l: Expression,
    _ r: Expression,
    _ evaluate: Expression? -> Result) -> Result? {
        return integerOperators[op].map { lift($0)(evaluate(l), evaluate(r))
        } }

func evaluateListOperator(op: String, _ l: Expression,
    _ r: Expression,
    _ evaluate: Expression? -> Result) -> Result? {
        switch (op, l, r) {
        case (":", .Reference("A", let row1),
            .Reference("A", let row2)) where row1 <= row2: return Result.ListResult(Array(row1...row2).map {
                evaluate(Expression.Reference("A", $0))
                }) default:
                    return nil
        }
}

func evaluateBinary(op: String,
    _ l: Expression,
    _ r: Expression,
    _ evaluate: Expression? -> Result) -> Result {
        return evaluateIntegerOperator(op, l, r, evaluate)
            ?? evaluateListOperator(op, l, r, evaluate)
            ?? .EvaluationError("Couldn't find operator \(op)")
}

func evaluateFunction(functionName: String,
    _ parameter: Result) -> Result {
        switch (functionName, parameter) { case ("SUM", .ListResult(let list)):
            return list.reduce(Result.IntResult(0), combine: lift(+)) case ("MIN", .ListResult(let list)):
                return list.reduce(Result.IntResult(Int.max), combine: lift { min($0, $1) })
        default:
            return .EvaluationError("Couldn't evaluate function")
        } }

func evaluateExpression(context: [Expression?]) -> Expression? -> Result { //바뀜
    return {e in e.map { expression in
        let compute = evaluateExpression(context)
        switch (expression) {
        case .Number(let x): return Result.IntResult(x)
        case .Reference("A", let idx): return compute(context[idx])
        case .BinaryExpression(let s, let l, let r):
            return evaluateBinary(s, l, r, compute)
        case .FunctionCall(let f, let p):
            return evaluateFunction(f, compute(p))
        default:
            return .EvaluationError("Couldn't evaluate expression")
        }
        } ?? .EvaluationError("Couldn't parse expression")
    }
}

func evaluateExpressions(expressions: [Expression?]) -> [Result] { return expressions.map(evaluateExpression(expressions))
}

