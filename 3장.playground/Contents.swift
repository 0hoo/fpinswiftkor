//: Playground - noun: a place where people can play

import Cocoa
import QuartzCore

typealias Filter = CIImage -> CIImage


func blur(radius: Double) -> Filter {
    return { image in
        let parameters = [ kCIInputRadiusKey: radius, kCIInputImageKey: image]
        
        let filter = CIFilter(name: "CIGaussianBlur",
            withInputParameters: parameters)!
        return filter.outputImage!
    }
}

/* original code
func colorGenerator(color: NSColor) -> Filter {
    return { _ in
        let parameters = [kCIInputColorKey: color]
        let filter = CIFilter(name: "CIConstantColorGenerator", withInputParameters: parameters)!
        return filter.outputImage! }
}
*/

func colorGenerator(color: NSColor) -> Filter {
    return { _ in
        return CIImage(color: CIColor(color: color)!)
    }
}

func compositeSourceOver(overlay: CIImage) -> Filter {
    return { image in
        let parameters = [ kCIInputBackgroundImageKey: image, kCIInputImageKey: overlay]
        let filter = CIFilter(name: "CISourceOverCompositing",
            withInputParameters: parameters)!
        let cropRect = image.extent
        return filter.outputImage!.imageByCroppingToRect(cropRect)
    }
}

func colorOverlay(color: NSColor) -> Filter {
    return { image in
    let overlay = colorGenerator(color)(image)
    return compositeSourceOver(overlay)(image)
    }
}

let url = NSURL(string: "http://www.objc.io/images/covers/16.jpg")!
let image = CIImage(contentsOfURL: url)!

let blurRadius = 5.0
let overlayColor = NSColor.redColor().colorWithAlphaComponent(0.2)
let blurredImage = blur(blurRadius)(image)
let overlaidImage = colorOverlay(overlayColor)(blurredImage)

let result = colorOverlay(overlayColor)(blur(blurRadius)(image))

func composeFilters(filter1: Filter, _ filter2: Filter) -> Filter {
    return { img in filter2(filter1(img)) }
}

let myFilter1 = composeFilters(blur(blurRadius), colorOverlay(overlayColor))
let result1 = myFilter1(image)

infix operator >>> { associativity left }
func >>> (filter1: Filter, filter2: Filter) -> Filter { return { img in filter2(filter1(img)) }
}

let myFilter2 = blur(blurRadius) >>> colorOverlay(overlayColor)
let result2 = myFilter2(image)

func add1(x: Int, _ y: Int) -> Int {
    return x + y
}


func add2(x: Int) -> (Int -> Int) {
    return { y in return x + y }
}

add1(1, 2)
add2(1)(2)

/*
func add2(x: Int) -> Int -> Int {
    return { y in x + y }
}
*/

func add3(x: Int)(_ y: Int) -> Int {
    return x + y
}

add3(1)(2)

