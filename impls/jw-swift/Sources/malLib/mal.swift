// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// mal - main mal type and supporting

public indirect enum Mal: Equatable {
    case int(Int)
    case list(ArraySlice<Mal>)
    case vec(ArraySlice<Mal>)
    case hashmap([String: Mal])
    case bool(Bool)
    case null // Can't use nil as it is used in Swift
    case str(String) // includes keywords with prefix
    case sym(String)
    case closure((ArraySlice<Mal>) throws -> Mal)

    public init(hashmapFromAlternatingList xs: [Mal]) {
        if !xs.count.isMultiple(of: 2) {
            self = .list([.sym("throw"), .str("Need an even number of elements for hash-map")])
        } else {
            var m = [String: Mal]()
            var ok = true
            for i in stride(from: xs.startIndex, to: xs.endIndex, by: 2) {
                let v = xs[i + 1]
                switch xs[i] {
                case .str(let k):
                    m[k] = v
                default:
                    ok = false
                }
            }
            self = ok ? .hashmap(m) : .list([.sym("throw"), .str("Bad key type in hash-map")])
        }
    }

    internal static let keywordPrefix: Character = "\u{29E}"

    //
    // Add convenience methods to extract values from a mal type
    //

    /// If this is a non-empty list return the head and tail
    public var headTail: (Mal, ArraySlice<Mal>)? {
        switch self {
        case .list(let xs):
            if let firstVal = xs.first {
                return (firstVal, xs.dropFirst())
            }
        default:
            break
        }
        return .none
    }

    /// If this is a list or vector, return the sequence
    public var sequence: ArraySlice<Mal>? {
        switch self {
        case .list(let xs):
            return ArraySlice(xs)
        case .vec(let xs):
            return ArraySlice(xs)
        default:
            return .none
        }
    }

    /// Is this an empty list?
    public var isEmptyList: Bool {
        switch self {
        case .list(let xs):
            return xs.isEmpty
        default:
            return false
        }

    /// Add specialized equality to out Mal type
    public static func == (lhs: Mal, rhs: Mal) -> Bool {
        switch (lhs, rhs) {
        // For most types we just compare the contents
        case (.int(let x), .int(let y)):
            return x == y
        case (.bool(let x), .bool(let y)):
            return x == y
        case (.null, .null):
            return true
        case (.str(let x), .str(let y)):
            return x == y
        case (.sym(let x), .sym(let y)):
            return x == y
        case (.hashmap(let xs), .hashmap(let ys)):
            return xs == ys

        // Lists and vectors are interchangeable for equality
        case (.list(let xs), .list(let ys)):
            return xs == ys
        case (.vec(let xs), .list(let ys)):
            return xs == ys
        case (.list(let xs), .vec(let ys)):
            return xs == ys
        case (.vec(let xs), .vec(let ys)):
            return xs == ys

        // Comparing two values of different types or two closures is false
        default:
            return false
        }
    }

    private static func seqEquals(_ x: [Mal], _ y: [Mal]) -> Bool {
        false
    }
}

/// Mal errors can either be a thrown value or a simple string message (for use when thrown from swift code)
public enum MalError: Error {
    case val(Mal)
    case msg(String)
}

///
extension ArraySlice {

    /// if the slice has exactly two elements, return them as a 2-tuple
    public var asPair: (Element, Element)? {
        if self.count == 2 {
            return (self[self.startIndex], self[self.startIndex + 1])
        }
        return .none
    }

    /// if the slice has exactly three elements, return them as a 3-tuple
    public var asTriple: (Element, Element, Element)? {
        if self.count == 3 {
            return (self[self.startIndex], self[self.startIndex + 1], self[self.startIndex + 2])
        }
        return .none
    }
}
