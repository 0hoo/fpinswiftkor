//
//  ArrayExtensions.swift
//  Spreadsheet
//
//  Created by Florian on 20/08/14.
//  Copyright (c) 2014 Unsigned Integer. All rights reserved.
//

import Foundation

extension Array {
    var decompose : (head: Element, tail: [Element])? {
        return (count > 0) ? (self[0], Array(self[1..<count])) : nil
    }
}

//extension ArraySlice {
//    var head: Element? {
//        return self.isEmpty ? nil : self[0]
//    }
//    
//    var tail: ArraySlice<Element> {
//        if (self.isEmpty) {
//            return self
//        }
//        return self[(self.startIndex+1)..<self.endIndex]
//    }
//    
//    var decompose: (head: Element, tail: ArraySlice<Element>)? {
//        return self.isEmpty ? nil
//            : (self[self.startIndex], self.tail)
//    }
//}


