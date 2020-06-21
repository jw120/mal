// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// types - program-wide types

public indirect enum Mal: Equatable {
    case int(Int)
    case list([Mal])
    case vec([Mal])
    case hashmap([Mal])
    case bool(Bool)
    case null // Can't use nil as it is used in Swift
    case str(String)
    case sym(String)
    case closure(MalFunc)

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

public struct MalError: Error {
    public let val: Mal

    public init(_ v: Mal) {
        self.val = v
    }
}
