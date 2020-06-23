// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// core - provcide the core set of definitions

/// Definitions to be included in repl environment
public let core: [String: Mal] = [
    // Arithmetic and logical functions on integers
    "+": wrapInt2Int("+", +),
    "-": wrapInt2Int("-", -),
    "*": wrapInt2Int("*", *),
    "/": wrapInt2Int("/", /),
    "<": wrapInt2Bool("/", <),
    ">": wrapInt2Bool("/", >),
    "<=": wrapInt2Bool("/", <=),
    ">=": wrapInt2Bool("/", >=),

    // Equality
    "=": .closure({ xs in
        if xs.count != 2 {
            throw MalError.msg("Need two arguments for =")
        }
        return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
    }),

    // Sequence functions
    "list": .closure({ xs in .list(xs) }),
    "list?": .closure({ xs in
        switch xs.first {
        case .list:
            return .bool(true)
        default:
            return .bool(false)
        }
    }),
    "empty?": .closure { args in
        if let seq = args.first?.sequence {
            return .bool(seq.isEmpty)
        }
        throw MalError.msg("Expected a list as the argument for empty?")
    },
    "count": .closure { args in
        if let seq = args.first?.sequence {
            return .int(seq.count)
        }
        throw MalError.msg("Expected a list as the argument for count")
    },

    // I/O functions
    "pr-str": .closure { args in
        let stringArgs = args.map { a in a.pr_str(readable: true) }
        return .str(stringArgs.joined(separator: " "))
    },
    "str": .closure { args in
        let stringArgs = args.map { a in a.pr_str(readable: false) }
        return .str(stringArgs.joined())
    },
    "prn": .closure { args in
        let stringArgs = args.map { a in a.pr_str(readable: true) }
        print(stringArgs.joined(separator: " "))
        return .null
    },
    "println": .closure { args in
        let stringArgs = args.map { a in a.pr_str(readable: false) }
        print(stringArgs.joined(separator: " "))
        return .null
    }
]

/// Wrap an (Int, Int) -> Int function in a Mal closure
fileprivate func wrapInt2Int(_ name: String, _ f: @escaping (Int, Int) -> Int) -> Mal {
    .closure {
        (args: ArraySlice<Mal>) throws -> Mal in
            if args.count != 2 {
                throw MalError.msg("Need two arguments for \(name)")
            }
            switch (args[args.startIndex], args[args.startIndex + 1]) {
            case (.int(let x), .int(let y)):
                return .int(f(x, y))
            default:
                throw MalError.msg("Arguments for \(name) must be integers")
            }
    }
}

// Wrap an (Int, Int) -> Bool function in a Mal closure
fileprivate func wrapInt2Bool(_ name: String, _ f: @escaping (Int, Int) -> Bool) -> Mal {
    .closure {
        (args: ArraySlice<Mal>) throws -> Mal in
            if args.count != 2 {
                throw MalError.msg("Need two arguments for \(name)")
            }
            switch (args[args.startIndex], args[args.startIndex + 1]) {
            case (.int(let x), .int(let y)):
                return .bool(f(x, y))
            default:
                throw MalError.msg("Arguments for \(name) must be integers")
            }
    }
}
