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
        guard xs.count == 2 else {
            throw MalError.msg("Need two arguments for =")
        }
        return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
    }),

    // Sequence functions
    "list": .closure({ xs in .list(xs) }),
    "list?": .closure({ xs in
        if case .list = xs.first {
            return .bool(true)
        } else {
            return .bool(false)
        }
    }),
    "empty?": .closure { args in
        guard let seq = args.first?.sequence else {
            throw MalError.msg("Expected a list as the argument for empty?")
        }
        return .bool(seq.isEmpty)
    },
    "count": .closure { args in
        guard let seq = args.first?.sequence  else {
            throw MalError.msg("Expected a list as the argument for count")
        }
        return .int(seq.count)
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
            guard args.count == 2 else {
                throw MalError.msg("Need two arguments for \(name)")
            }
            guard case let (.int(let x), .int(let y)) = (args[args.startIndex], args[args.startIndex + 1]) else {
                throw MalError.msg("Arguments for \(name) must be integers")
            }
            return .int(f(x, y))
    }
}

// Wrap an (Int, Int) -> Bool function in a Mal closure
fileprivate func wrapInt2Bool(_ name: String, _ f: @escaping (Int, Int) -> Bool) -> Mal {
    .closure {
        (args: ArraySlice<Mal>) throws -> Mal in
            guard args.count == 2 else {
                throw MalError.msg("Need two arguments for \(name)")
            }
            guard case let (.int(let x), .int(let y)) = (args[args.startIndex], args[args.startIndex + 1]) else {
                throw MalError.msg("Arguments for \(name) must be integers")
            }
            return .bool(f(x, y))
    }
}
