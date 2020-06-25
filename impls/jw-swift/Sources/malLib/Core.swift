// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// Core - provcide the core set of definitions

/// Definitions to be included in repl environment
public let core: [String: Mal] = [
    // MARK: - arithmetic and logical functions on integers

    "+": wrapInt2Int("+", +),
    "-": wrapInt2Int("-", -),
    "*": wrapInt2Int("*", *),
    "/": wrapInt2Int("/", /),
    "<": wrapInt2Bool("/", <),
    ">": wrapInt2Bool("/", >),
    "<=": wrapInt2Bool("/", <=),
    ">=": wrapInt2Bool("/", >=),

    // MARK: - sequence functions

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
    "cons": swiftClosure { args in
        guard case let .some((x, y)) = args.asPair, let ys = y.sequence else {
            throw MalError.msg("Expected a value and a list as arguments for cons")
        }
        var newArray = ArraySlice(ys)
        newArray.insert(x, at: newArray.startIndex)
        return .list(newArray)
    },
    "concat": swiftClosure { args in
        var newArray: [Mal] = []
        for a in args {
            guard let xs = a.sequence else {
                throw MalError.msg("Arguments to concat must be lists")
            }
            newArray.append(contentsOf: xs)
        }
        return .list(ArraySlice(newArray))
    },

    // MARK: - I/O functions

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
    },
    "read-string": swiftClosure { args in
        guard case let .some(.str(s)) = args.asSingleton else {
            throw MalError.msg("Need one string argument for read-string")
        }
        switch read_str(s) {
        case .value(let v):
            return v
        case .err(let msg):
            throw MalError.msg(msg)
        case .nothing:
            return .null
        }
    },
    "slurp": swiftClosure { args in
        guard case let .some(.str(s)) = args.asSingleton else {
            throw MalError.msg("Need one string argument for slurp")
        }
        return .str(try String(contentsOfFile: s))
    },

    // MARK: - atom functions

    "atom": swiftClosure { args in
        guard case let .some(v) = args.asSingleton else {
            throw MalError.msg("Need one argument for atom")
        }
        return .atom(MalAtom(v))
    },
    "atom?": swiftClosure { args in
        guard args.count == 1 else {
            throw MalError.msg("Need one argument for atom?")
        }
        if case .some(.atom) = args.first {
            return .bool(true)
        }
        return .bool(false)
    },
    "deref": swiftClosure { args in
        guard case let .some(.atom(a)) = args.asSingleton else {
            throw MalError.msg("Need one atom argumente for deref")
        }
        return a.contents
    },
    "reset!": swiftClosure { args in
        guard case let .some((.atom(a), v)) = args.asPair else {
            throw MalError.msg("Need an atom and a value for reset!")
        }
        a.contents = v
        return v
    },
    "swap!": swiftClosure { args in
        guard
            args.count >= 2,
            case let .some(.atom(a)) = args.first,
            case let .closure(c) = args[args.startIndex + 1] else {
                throw MalError.msg("Need an atom, a function and zero or more other arguments for swap!")
        }
        var fnArgs = args.dropFirst(2)
        fnArgs.insert(a.contents, at: fnArgs.startIndex)
        a.contents = try c.swift(fnArgs)
        return a.contents
    },

    // MARK: - Misc functions

    "=": swiftClosure { xs in
        guard xs.count == 2 else {
            throw MalError.msg("Need two arguments for =")
        }
        return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
    }
]

/// convert a swift closure into a Mal closure
public func swiftClosure(_ f: @escaping (ArraySlice<Mal>) throws -> Mal) -> Mal {
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
