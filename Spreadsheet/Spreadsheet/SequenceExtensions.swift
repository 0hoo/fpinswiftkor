//
//  SequenceExtensions.swift
//  Parsing
//
//  Created by Chris Eidhof on 01.07.14.
//  Copyright (c) 2014 Unsigned Integer. All rights reserved.
//

import Foundation

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

func map<A, B>(s: AnySequence<A>, f: A -> B) -> AnySequence<B> {
    return AnySequence { map(s.generate(), f: f) }
}


func join<A>(s: AnySequence<AnySequence<A>>) -> AnySequence<A> {
    return AnySequence {
        JoinedGenerator(map(s.generate()) { g in
            g.generate()
            })
    }
}

func apply<A, B>(ls: AnySequence<A>, f: A -> AnySequence<B>) -> AnySequence<B> {
    return join(map(ls, f: f))
}

func +<A>(l: AnySequence<A>, r: AnySequence<A>) -> AnySequence<A> {
    return join(AnySequence([l, r]))
}

func one<A>(x: A) -> AnySequence<A> {
    return AnySequence(GeneratorOfOne(x))
}

func none<A>() -> AnySequence<A> {
    return AnySequence(anyGenerator { nil } )
}