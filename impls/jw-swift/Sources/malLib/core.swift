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
    "=": swiftClosure { xs in
        guard xs.count == 2 else {
            throw MalError.msg("Need two arguments for =")
        }
        return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
    },

    // Sequence functions
    "list": swiftClosure { xs in .list(xs) },
    "list?": swiftClosure { xs in
        if case .list = xs.first {
            return .bool(true)
        } else {
            return .bool(false)
        }
    },
    "empty?": swiftClosure { args in
        guard let seq = args.first?.sequence else {
            throw MalError.msg("Expected a list as the argument for empty?")
        }
        return .bool(seq.isEmpty)
    },
    "count": swiftClosure { args in
        guard let seq = args.first?.sequence  else {
            throw MalError.msg("Expected a list as the argument for count")
        }
        return .int(seq.count)
    },

    // I/O functions
    "pr-str": swiftClosure { args in
        let stringArgs = args.map { a in a.pr_str(readable: true) }
        return .str(stringArgs.joined(separator: " "))
    },
    "str": swiftClosure { args in
        let stringArgs = args.map { a in a.pr_str(readable: false) }
        return .str(stringArgs.joined())
    },
    "prn": swiftClosure { args in
        let stringArgs = args.map { a in a.pr_str(readable: true) }
        print(stringArgs.joined(separator: " "))
        return .null
    },
    "println": swiftClosure { args in
        let stringArgs = args.map { a in a.pr_str(readable: false) }
        print(stringArgs.joined(separator: " "))
        return .null
    }
]

/// convert a swift closure into a Mal closure
fileprivate func swiftClosure(_ f: @escaping (ArraySlice<Mal>) throws -> Mal) -> Mal {
    .closure(MalClosure(mal: nil, swift: f, isMacro: false))
}

/// Wrap an (Int, Int) -> Int function in a Mal closure
fileprivate func wrapInt2Int(_ name: String, _ f: @escaping (Int, Int) -> Int) -> Mal {
    swiftClosure { (args: ArraySlice<Mal>) throws -> Mal in
        guard args.count == 2 else {
            throw MalError.msg("Need two arguments for \(name)")
        }
        guard case let (.int(x), .int(y)) = (args[args.startIndex], args[args.startIndex + 1]) else {
            throw MalError.msg("Arguments for \(name) must be integers")
        }
        return .int(f(x, y))
    }
}

// Wrap an (Int, Int) -> Bool function in a Mal closure
fileprivate func wrapInt2Bool(_ name: String, _ f: @escaping (Int, Int) -> Bool) -> Mal {
    swiftClosure { (args: ArraySlice<Mal>) throws -> Mal in
        guard args.count == 2 else {
            throw MalError.msg("Need two arguments for \(name)")
        }
        guard case let (.int(x), .int(y)) = (args[args.startIndex], args[args.startIndex + 1]) else {
            throw MalError.msg("Arguments for \(name) must be integers")
        }
        return .bool(f(x, y))
    }
}
