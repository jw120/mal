// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// types - program-wide types

public indirect enum Mal: Equatable {
    case int(Int)
    case list([Mal])
    case vec([Mal])
    case hashmap([String: Mal])
    case bool(Bool)
    case null // Can't use nil as it is used in Swift
    case str(String)
    case sym(String)
    case closure(MalFunc)

    public init(hashmapFromAlternatingList xs: [Mal]) {
        if !xs.count.isMultiple(of: 2) {
            self = .list([.sym("throw"), .str("Need an even number of elements for hash-map")])
        } else {
            var m = [String: Mal]()
            var ok = true
            for i in stride(from: 0, to: xs.count - 1, by: 2) {
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

    /// Is this an empty list?
    public var isEmptyList: Bool {
        switch self {
        case .list(let xs):
            return xs.isEmpty
        default:
            return false
        }
    }

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

/// Swift type of a mal function call
public typealias MalFunc = (ArraySlice<Mal>) throws -> Mal

/// Mal errors can either be a thrown value or a simple string message (for use when thrown from swift code)
public enum MalError: Error {
    case val(Mal)
    case msg(String)
}
