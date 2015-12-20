import Cocoa

class Box<T> {
    let unbox: T
    init(_ value: T) { self.unbox = value }
}

extension NSGraphicsContext {
    var cgContext : CGContextRef {
        let opaqueContext = COpaquePointer(self.graphicsPort)
        return Unmanaged<CGContextRef>.fromOpaque(opaqueContext)
            .takeUnretainedValue()
    }
}

func *(l: CGPoint, r: CGRect) -> CGPoint {
    return CGPointMake(r.origin.x + l.x*r.size.width,
        r.origin.y + l.y*r.size.height)
}

func *(l: CGFloat, r: CGPoint) -> CGPoint {
    return CGPointMake(l*r.x, l*r.y)
}
func *(l: CGFloat, r: CGSize) -> CGSize {
    return CGSizeMake(l*r.width, l*r.height)
}

func pointWise(f: (CGFloat, CGFloat) -> CGFloat,
    l: CGSize, r: CGSize) -> CGSize {
        
        return CGSizeMake(f(l.width, r.width), f(l.height, r.height))
}

func pointWise(f: (CGFloat, CGFloat) -> CGFloat,
    l: CGPoint, r:CGPoint) -> CGPoint {
        
        return CGPointMake(f(l.x, r.x), f(l.y, r.y))
}

//수정
func /(l: CGSize, r: CGSize) -> CGSize {
    return pointWise(/, l: l, r: r)
}

//수정
func *(l: CGSize, r: CGSize) -> CGSize {
    return pointWise(*, l: l, r: r)
}

//수정
func +(l: CGSize, r: CGSize) -> CGSize {
    return pointWise(+, l: l, r: r)
}

//수정
func -(l: CGSize, r: CGSize) -> CGSize {
    return pointWise(-, l: l, r: r)
}

//수정
func -(l: CGPoint, r: CGPoint) -> CGPoint {
    return pointWise(-, l: l, r: r)
}

//수정
func +(l: CGPoint, r: CGPoint) -> CGPoint {
    return pointWise(+, l: l, r: r)
}

//수정
func *(l: CGPoint, r: CGPoint) -> CGPoint {
    return pointWise(*, l: l, r: r)
}


extension CGSize {
    var point : CGPoint {
        return CGPointMake(self.width, self.height)
    }
}

func isHorizontalEdge(edge: CGRectEdge) -> Bool {
    switch edge {
    case .MaxXEdge, .MinXEdge:
        return true
    default:
        return false
    }
}

func splitRect(rect: CGRect, sizeRatio: CGSize,
    edge: CGRectEdge) -> (CGRect, CGRect) {
        
        let ratio = isHorizontalEdge(edge) ? sizeRatio.width
            : sizeRatio.height
        let multiplier = isHorizontalEdge(edge) ? rect.width
            : rect.height
        let distance : CGFloat = multiplier * ratio
        var mySlice : CGRect = CGRectZero
        var myRemainder : CGRect = CGRectZero
        CGRectDivide(rect, &mySlice, &myRemainder, distance, edge)
        return (mySlice, myRemainder)
}

//수정
func splitHorizontal(rect: CGRect,
    ratio: CGSize) -> (CGRect, CGRect) {
        
        return splitRect(rect, sizeRatio: ratio, edge: CGRectEdge.MinXEdge)
}

//수정
func splitVertical(rect: CGRect,
    ratio: CGSize) -> (CGRect, CGRect) {
        
        return splitRect(rect, sizeRatio: ratio, edge: CGRectEdge.MinYEdge)
}

extension CGRect {
    init(center: CGPoint, size: CGSize) {
        let origin = CGPointMake(center.x - size.width/2,
            center.y - size.height/2)
        self.init(origin: origin, size: size)
    }
}

// A 2-D Vector
struct Vector2D {
    let x: CGFloat
    let y: CGFloat
    
    var point : CGPoint { return CGPointMake(x, y) }
    
    var size : CGSize { return CGSizeMake(x, y) }
}

func *(m: CGFloat, v: Vector2D) -> Vector2D {
    return Vector2D(x: m * v.x, y: m * v.y)
}

extension Dictionary {
    var keysAndValues: [(Key, Value)] {
        var result: [(Key, Value)] = []
        for item in self {
            result.append(item)
        }
        return result
    }
}

func normalize(input: [CGFloat]) -> [CGFloat] {
    let maxVal = input.reduce(0) { max($0, $1) }
    return input.map { $0 / maxVal }
}

enum Primitive {
    case Ellipse
    case Rectangle
    case Text(String)
}

enum Diagram {
    case Prim(CGSize, Primitive)
    case Beside(Box<Diagram>, Box<Diagram>)
    case Below(Box<Diagram>, Box<Diagram>)
    case Attributed(Attribute, Box<Diagram>)
    case Align(Vector2D, Box<Diagram>)
}

enum Attribute {
    case FillColor(NSColor)
}

extension Diagram {
    var size: CGSize {
        switch self {
        case .Prim(let size, _):
            return size
        case .Attributed(_, let x):
            return x.unbox.size
        case .Beside(let l, let r):
            let sizeL = l.unbox.size
            let sizeR = r.unbox.size
            return CGSizeMake(sizeL.width + sizeR.width,
                max(sizeL.height, sizeR.height))
        case .Below(let l, let r):
            let sizeL = l.unbox.size
            let sizeR = r.unbox.size
            return CGSizeMake(max(sizeL.width, sizeR.width),
                sizeL.height+sizeR.height)
        case .Align(_, let r):
            return r.unbox.size
        }
    }
}

func fit(alignment: Vector2D,
    inputSize: CGSize, rect: CGRect) -> CGRect {
        
        let scaleSize = rect.size / inputSize
        let scale = min(scaleSize.width, scaleSize.height)
        let size = scale * inputSize
        let space = alignment.size * (size - rect.size)
        return CGRect(origin: rect.origin - space.point, size: size)
}

fit(Vector2D(x: 0.5, y: 0.5), inputSize: CGSizeMake(1, 1), rect: CGRectMake(0, 0, 200, 100))

fit(Vector2D(x: 0, y: 0.5), inputSize: CGSizeMake(1, 1), rect: CGRectMake(0, 0, 200, 100))

func draw(context: CGContextRef, bounds: CGRect, diagram: Diagram) {
    switch diagram {
    case .Prim(let size, .Ellipse):
        let frame = fit(Vector2D(x: 0.5, y: 0.5), inputSize: size, rect: bounds)
        CGContextFillEllipseInRect(context, frame)
    case .Prim(let size, .Rectangle):
        let frame = fit(Vector2D(x: 0.5, y: 0.5), inputSize: size, rect :bounds)
        CGContextFillRect(context, frame)
    case .Prim(let size, .Text(let text)):
        let frame = fit(Vector2D(x: 0.5, y: 0.5), inputSize: size, rect: bounds)
        let font = NSFont.systemFontOfSize(12)
        let attributes = [NSFontAttributeName: font]
        let attributedText = NSAttributedString(
            string: text, attributes: attributes)
        attributedText.drawInRect(frame)
    case .Attributed(.FillColor(let color), let d):
        CGContextSaveGState(context)
        color.set()
        draw(context, bounds: bounds, diagram: d.unbox)
        CGContextRestoreGState(context)
    case .Beside(let left, let right):
        let l = left.unbox
        let r = right.unbox
        let (lFrame, rFrame) = splitHorizontal(
            bounds, ratio: l.size/diagram.size)
        draw(context, bounds: lFrame, diagram: l)
        draw(context, bounds: rFrame, diagram: r)
    case .Below(let top, let bottom):
        let t = top.unbox
        let b = bottom.unbox
        let (lFrame, rFrame) = splitVertical(
            bounds, ratio: b.size/diagram.size)
        draw(context, bounds: lFrame, diagram: b)
        draw(context, bounds: rFrame, diagram: t)
    case .Align(let vec, let d):
        let diagram = d.unbox
        let frame = fit(vec, inputSize: diagram.size, rect: bounds)
        draw(context, bounds: frame, diagram: diagram)
    }
}

class Draw: NSView {
    let diagram: Diagram
    
    init(frame frameRect: NSRect, diagram: Diagram) {
        self.diagram = diagram
        super.init(frame:frameRect)
    }
    
    required init(coder: NSCoder) {
        fatalError("NSCoding not supported")
    }
    override func drawRect(dirtyRect: NSRect) {
        if let context = NSGraphicsContext.currentContext() {
            draw(context.cgContext, bounds: self.bounds, diagram: diagram)
        }
    }
}

func pdf(diagram: Diagram, width: CGFloat) -> NSData {
    let unitSize = diagram.size
    let height = width * (unitSize.height/unitSize.width)
    let v: Draw = Draw(frame: NSMakeRect(0, 0, width, height),
        diagram: diagram)
    return v.dataWithPDFInsideRect(v.bounds)
}

func rect(width width: CGFloat, height: CGFloat) -> Diagram {
    return Diagram.Prim(CGSizeMake(width, height), .Rectangle)
}

func circle(diameter diameter: CGFloat) -> Diagram {
    return Diagram.Prim(CGSizeMake(diameter, diameter), .Ellipse)
}

func text(width width: CGFloat,
    height: CGFloat, text theText: String) -> Diagram {
        
        return Diagram.Prim(CGSizeMake(width, height), .Text(theText))
}

func square(side side: CGFloat) -> Diagram {
    return rect(width: side, height: side)
}

infix operator ||| { associativity left }
func ||| (l: Diagram, r: Diagram) -> Diagram {
    return Diagram.Beside(Box(l), Box(r))
}

infix operator --- { associativity left }
func --- (l: Diagram, r: Diagram) -> Diagram {
    return Diagram.Below(Box(l), Box(r))
}

extension Diagram {
    func fill(color: NSColor) -> Diagram {
        return Diagram.Attributed(Attribute.FillColor(color),
            Box(self))
    }
    
    func alignTop() -> Diagram {
        return Diagram.Align(Vector2D(x: 0.5, y: 1), Box(self))
    }
    
    func alignBottom() -> Diagram {
        return Diagram.Align(Vector2D(x:0.5, y: 0), Box(self))
    }
}

let empty: Diagram = rect(width: 0, height: 0)

func hcat(diagrams: [Diagram]) -> Diagram {
    return diagrams.reduce(empty, combine: |||)
}

let blueSquare = square(side: 1).fill(NSColor.blueColor())
let redSquare = square(side: 2).fill(NSColor.redColor())
let greenCircle = circle(diameter: 1).fill(NSColor.greenColor())
let example1 = blueSquare ||| redSquare ||| greenCircle
let cyanCircle = circle(diameter: 1).fill(NSColor.cyanColor())
let example2 = blueSquare ||| cyanCircle |||
    redSquare ||| greenCircle

extension String {
    func stringByAppendingPathComponent(path: String) -> String {
        let s = self as NSString
        return s.stringByAppendingPathComponent(path)
    }
}

func barGraph(input: [(String, Double)]) -> Diagram {
    let values: [CGFloat] = input.map { CGFloat($0.1) }
    let nValues = normalize(values)
    let bars = hcat(nValues.map { (x: CGFloat) -> Diagram in
        return rect(width: 1, height: 3 * x)
            .fill(NSColor.blackColor()).alignBottom()
        })
    let labels = hcat(input.map { x in
        return text(width: 1, height: 0.3, text: x.0).alignTop()
        })
    return bars --- labels
}
let cities = ["Shanghai": 14.01, "Istanbul": 13.3,
    "Moscow": 10.56, "New York": 8.33, "Berlin": 3.43]
let example3 = barGraph(cities.keysAndValues)
