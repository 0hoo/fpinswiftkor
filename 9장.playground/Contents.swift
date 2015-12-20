import Foundation

func empty<T>() -> [T] {
    return []
}

func isEmpty<T>(set: [T]) -> Bool {
    return set.isEmpty
}

func contains<T: Equatable>(x: T, _ set: [T]) -> Bool {
    return set.contains(x)
}

func insert<T: Equatable>(x: T, _ set:[T]) -> [T] {
    return contains(x, set) ? set : [x] + set
}


indirect enum BinarySearchTree<T: Comparable> {
    case Leaf
    case Node(BinarySearchTree<T>, T, BinarySearchTree<T>)
}

let leaf: BinarySearchTree<Int> = .Leaf
let five: BinarySearchTree<Int> = .Node(leaf, 5, leaf)

extension BinarySearchTree {
    init(_ value: T) {
        self = .Node(.Leaf, value, .Leaf)
    }
    init() {
        self = .Leaf
    }
}

extension BinarySearchTree {
    var count: Int {
        switch self {
            case .Leaf:
                return 0
            case let .Node(left, _, right):
                return 1 + left.count + right.count
        }
    }
}

extension BinarySearchTree {
    var elements: [T] {
        switch self {
            case .Leaf:
                return []
            case let .Node(left, x, right):
                return left.elements + [x] + right.elements
        }
    }
}

extension BinarySearchTree {
    var isEmpty: Bool {
        if case .Leaf = self {
            return true
        }
        return false
    }
}

extension Array {
    func all<T> (predicate : (T) -> Bool) -> Bool {
        for x in self {
            let t = x as! T;
            if !predicate(t) {
                return false
            }
        }
        return true
    }
}

extension BinarySearchTree where T : Comparable {
    var isBST: Bool {
        switch self {
        case .Leaf:
            return true
        case let .Node(left, x, right):
            return left.elements.all { y in y < x }
                && right.elements.all { y in y > x }
                && left.isBST
                && right.isBST
        }
    }
}

extension BinarySearchTree {
    func contains(x: T) -> Bool {
        switch self { case .Leaf:
            return false
        case let .Node(_, y, _) where x == y:
            return true
        case let .Node(left, y, _) where x < y:
            return left.contains(x)
        case let .Node(_, y, right) where x > y:
            return right.contains(x) default:
                fatalError("The impossible occurred") }
    }
}

extension BinarySearchTree {
    mutating func insert(x: T) {
        switch self {
        case .Leaf:
            self = BinarySearchTree(x)
        case .Node(var left, let y, var right):
            if x < y { left.insert(x) }
            if x > y { right.insert(x) }
            self = .Node(left, y, right)
        }
    }
}

let myTree: BinarySearchTree<Int> = BinarySearchTree()
var copied = myTree
copied.insert(5)
(myTree.elements, copied.elements)

func autocomplete(history: [String], textEntered: String) -> [String] { return history.filter { $0.hasPrefix(textEntered) }
}

struct Trie<T: Hashable> {
    let isElement: Bool
    let children: [T: Trie<T>]
}


extension Trie { init() {
    isElement = false
    children = [:] }
}

extension Trie {
    var elements: [[T]] {
        var result: [[T]] = isElement ? [[]] : []
        for (key, value) in children {
            result += value.elements.map { [key] + $0 } }
        return result }
}

extension Array {
    var decompose : (head: Element, tail: [Element])? {
        return isEmpty ? nil : (self[0], Array(self[1..<count])) }
}


func sum(xs: [Int]) -> Int {
    guard let (head, tail) = xs.decompose else { return 0 }
    return head + sum(tail)
}

func qsort(input: [Int]) -> [Int] {
    guard let (pivot, rest) = input.decompose else { return [] }
    let lesser = rest.filter { $0 < pivot }
    let greater = rest.filter { $0 >= pivot }
    return qsort(lesser) + [pivot] + qsort(greater)
}

extension Trie {
    func lookup(key: [T]) -> Bool {
        guard let (head, tail) = key.decompose else { return isElement }
        guard let subtrie = children[head] else { return false }
        return subtrie.lookup(tail)
    }
}

extension Trie {
    func withPrefix(prefix: [T]) -> Trie<T>? {
        guard let (head, tail) = prefix.decompose else { return self }
        guard let remainder = children[head] else { return nil }
        return remainder.withPrefix(tail)
    }
}

extension Trie {
    func autocomplete(key: [T]) -> [[T]] {
        return withPrefix(key)?.elements ?? [] }
}

extension Trie {
    init(_ key: [T]) {
        if let (head,tail) = key.decompose {
            let children = [head: Trie(tail)]
            self = Trie(isElement: false, children: children)
        } else {
            self = Trie(isElement: true, children: [:])
        }
    }
}

extension Trie {
    func insert(key: [T]) -> Trie<T> {
        guard let (head,tail) = key.decompose else {
            return Trie(isElement: true, children: children)
        }
        var newChildren = children
        if let nextTrie = children[head] {
            newChildren[head] = nextTrie.insert(tail)
        } else {
            newChildren[head] = Trie(tail)
        }
        return Trie(isElement: isElement, children: newChildren)
    }
}

func buildStringTrie(words: [String]) -> Trie<Character> {
    let emptyTrie = Trie<Character>()
    return words.reduce(emptyTrie) { trie, word in
        trie.insert(Array(word.characters))
    }
}

func autocompleteString(knownWords: Trie<Character>, word: String) -> [String]
{
    let chars = Array(word.characters)
    let completed = knownWords.autocomplete(chars)
    return completed.map { chars in
        word + String(chars)
    }
}

let contents = ["cat", "car", "cart", "dog"]
let trieOfWords = buildStringTrie(contents)
autocompleteString(trieOfWords, word: "car")

