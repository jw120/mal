// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// Mal - main mal type and supporting

public indirect enum Mal: Equatable {
    case int(Int)
    case seq(Bool, ArraySlice<Mal>) // bool is true for list, false for vector
    case hashmap([String: Mal])
    case bool(Bool)
    case null // Can't use the same name as mal ("nil") as it is used in Swift
    case str(String) // includes keywords with prefix
    case sym(String)
    case closure(MalClosure)
    case atom(MalAtom)

    public init?(hashmapFromAlternatingList xs: [Mal]) {
        if !xs.count.isMultiple(of: 2) {
            return nil
        } else {
            var m = [String: Mal]()
            for i in stride(from: xs.startIndex, to: xs.endIndex, by: 2) {
                let v = xs[i + 1]
                switch xs[i] {
                case .str(let k):
                    m[k] = v
                default:
                    return nil
                }
            }

            self = .hashmap(m)
        }
    }

    internal static let keywordPrefix: Character = "\u{29E}"

    //
    // Add convenience methods to extract values from a mal type
    //

    /// If this is a non-empty sequence return the head and tail
    public var seqHeadTail: (Mal, ArraySlice<Mal>)? {
        guard let xs = self.sequence, let firstVal = xs.first else {
            return .none
        }
        return (firstVal, xs.dropFirst())
    }

    /// If this is a non-empty list return the head and tail
    public var listHeadTail: (Mal, ArraySlice<Mal>)? {
        guard case let .seq(true, xs) = self, let firstVal = xs.first else {
            return .none
        }
        return (firstVal, xs.dropFirst())
    }

    /// If this is a list, vector or nil, return the sequence
    public var sequence: ArraySlice<Mal>? {
        switch self {
        case .seq(_, let xs):
            return ArraySlice(xs)
        case .null:
            return ArraySlice([])
        default:
            return .none
        }
    }

    /// Is this an empty list?
    public var isEmptyList: Bool {
        guard case let .seq(true, xs) = self else {
            return false
        }
        return xs.isEmpty
    }

    /// Does this represent a macro in the given environment
    public func isMacroCall(env: Env) throws -> Bool {
    if
        case let .seq(true, xs) = self,
        case let .some(.sym(s)) = xs.first,
        env.find(s) != nil, // needed to avoid exception from get if no such symbol
        case let .closure(c) = try env.get(s) {
        return c.isMacro
    }
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
        case (.seq(_, let xs), .seq(_, let ys)):
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

/// For a closure we hold both the mal code needed for evaluation (ast, params, env) and
/// a swift closure that can be evaluated. For a swift-defined function there is no mal part
public struct MalClosure {
    /// mal version of the closure (ast, parameter names and environemtn)
    public let mal: (Mal, [String], Env)?

    /// swift version of the closure
    public let swift: (ArraySlice<Mal>) throws -> Mal

    /// Is the closure a macro?
    public let isMacro: Bool
}

/// Mal errors can either be a thrown value or a simple string message (for use when thrown from swift code)
public enum MalError: Error {
    case val(Mal)
    case msg(String)
}

/// Mal atoms are a mutable reference to (immutable) data
public class MalAtom {
    /// The contents of the atom
    public var contents: Mal

    /// Initialize the atom with the given value as its contents
    public init(_ val: Mal) {
        contents = val
    }
}
