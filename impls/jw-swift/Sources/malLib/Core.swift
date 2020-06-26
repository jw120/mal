// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// Core - provcide the core set of definitions

import Foundation // needed for Date()

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

    "list": swiftClosure { xs in .seq(true, xs) },
    "vector": swiftClosure { xs in .seq(false, xs) },
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
        return .seq(true, newArray)
    },
    "concat": swiftClosure { args in
        var newArray: [Mal] = []
        for a in args {
            guard let xs = a.sequence else {
                throw MalError.msg("Arguments to concat must be lists")
            }
            newArray.append(contentsOf: xs)
        }
        return .seq(true, ArraySlice(newArray))
    },
    "nth": swiftClosure { args in
        guard case let .some((x, y)) = args.asPair, let xs = x.sequence, case let .int(i) = y else {
            throw MalError.msg("Expected a sequence and an integer as arguments for nth")
        }
        guard i >= 0 && i < xs.count else {
            throw MalError.msg("Out of range in nth")
        }
        return xs[xs.startIndex + i]
    },
    "first": swiftClosure { args in
        guard let xs = args.asSingleton?.sequence else {
            throw MalError.msg("Expected a sequence as argument for first")
        }
        guard let x = xs.first else {
            return .null
        }
        return x
    },
    "rest": swiftClosure { args in
        guard let xs = args.asSingleton?.sequence else {
            throw MalError.msg("Expected a sequence as argument for first")
        }
        guard !xs.isEmpty else {
            return .seq(true, [])
        }
        return .seq(true, xs.dropFirst())
    },
    "seq": swiftClosure { args in
        guard let x = args.asSingleton else {
            throw MalError.msg("seq takes one argument")
        }
        switch x {
        case .null, .str(""), .seq(_, []):
            return .null
        case .seq(_, let ys):
            return .seq(true, ys)
        case .str(let s):
            let splitStrings: [Mal] = s.map { .str(String($0)) }
            return .seq(true, ArraySlice(splitStrings))
        default:
            throw MalError.msg("seq takes a string, sequence or nil")
        }
    },
    "conj": swiftClosure { args in
        guard case let .seq(isList, xs) = args.first else {
            throw MalError.msg("conj takes a sequence and additional arguments")
        }
        let otherArgs: ArraySlice<Mal> = args.dropFirst()
        if isList {
            let newSeq = otherArgs.reversed() + xs
            return .seq(true, newSeq)
        } else {
            let newSeq: ArraySlice<Mal> = xs + otherArgs
            return .seq(false, newSeq)
        }
    },

    // MARK: - hashmap functions
    "hash-map": swiftClosure { args in
        guard let m = Mal(hashmapFromAlternatingList: Array(args)) else {
            throw MalError.msg("Bad arguments to hash-map")
        }
        return m
    },
    "assoc": swiftClosure { args in
        guard case let .hashmap(m) = args.first else {
            throw MalError.msg("First argument of assoc must be a hashmap")
        }
        let keyValueList = args.dropFirst()
        guard keyValueList.count.isMultiple(of: 2) else {
            throw MalError.msg("Need an even number of key-value elements for assoc")
        }
        var newM = m
        for i in stride(from: keyValueList.startIndex, to: keyValueList.endIndex, by: 2) {
            let v = keyValueList[i + 1]
            guard case let .str(k) = keyValueList[i] else {
                throw MalError.msg("Bad key type in hash-map")
            }
            newM[k] = v
        }
        return .hashmap(newM)
    },
    "dissoc": swiftClosure { args in
        guard case let .hashmap(m) = args.first else {
            throw MalError.msg("First argument of dissoc must be a hashmap")
        }
        var newM = m
        for key in args.dropFirst() {
            if case let .str(s) = key {
                newM.removeValue(forKey: s)
            }
        }
        return .hashmap(newM)
    },
    "get": swiftClosure { args in
        if args.first == .null {
            return .null
        }
        guard case let .some((.hashmap(m), .str(s))) = args.asPair else {
            throw MalError.msg("get takes a hash-map and a string or keyword")
        }
        if let val = m[s] {
            return val
        }
        return .null
    },
    "contains?": swiftClosure { args in
        guard case let .some((.hashmap(m), .str(s))) = args.asPair else {
            throw MalError.msg("contains? takes a hash-map and a string or keyword")
        }
        return .bool(m[s] != nil)
    },
    "keys": swiftClosure { args in
        guard case let .some(.hashmap(m)) = args.asSingleton else {
            throw MalError.msg("keys takes a hash-map")
        }
        let keys: [Mal] = m.keys.map { .str($0) }
        return .seq(true, ArraySlice(keys))
    },
    "vals": swiftClosure { args in
        guard case let .some(.hashmap(m)) = args.asSingleton else {
            throw MalError.msg("vals takes a hash-map")
        }
        return .seq(true, ArraySlice(m.values))
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
    "readline": swiftClosure { args in
        guard case let .some(.str(s)) = args.asSingleton else {
            throw MalError.msg("Need one string argument for readline")
        }
        print(s, terminator: "")
        guard let input = readLine() else {
            return .null
        }
        return .str(input)
    },

    // MARK: - atom functions

    "atom": swiftClosure { args in
        guard case let .some(v) = args.asSingleton else {
            throw MalError.msg("Need one argument for atom")
        }
        return .atom(MalAtom(v))
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

    // MARK: - identity-testing functions

    "atom?": wrapMalBool("atom?") { if case .atom = $0 { return true } else { return false } },
    "false?": wrapMalBool("false?") { if case .bool(false) = $0 { return true } else { return false } },
    "fn?": wrapMalBool("fn?") {
        if case let .closure(c) = $0 { return !c.isMacro } else { return false }
    },
    "keyword?": wrapMalBool("keyword?") {
        if case let .str(s) = $0 { return s.first == Mal.keywordPrefix } else { return false }
    },
    "list?": wrapMalBool("list?") { if case .seq(true, _) = $0 { return true } else { return false } },
    "macro?": wrapMalBool("macro?") {
        if case let .closure(c) = $0 { return c.isMacro } else { return false }
    },
    "map?": wrapMalBool("map?") { if case .hashmap = $0 { return true } else { return false } },
    "nil?": wrapMalBool("nil?") { if case .null = $0 { return true } else { return false } },
    "number?": wrapMalBool("number?") { if case .int = $0 { return true } else { return false } },
    "sequential?": wrapMalBool("sequential?") { if case .seq(_, _) = $0 { return true } else { return false } },
    "string?": wrapMalBool("string?") {
        if case let .str(s) = $0 { return s.first != Mal.keywordPrefix } else { return false }
    },
    "symbol?": wrapMalBool("symbol?") { if case .sym = $0 { return true } else { return false } },
    "true?": wrapMalBool("true?") { if case .bool(true) = $0 { return true } else { return false } },
    "vector?": wrapMalBool("vector?") { if case .seq(false, _) = $0 { return true } else { return false } },

    // MARK: - Misc functions etc

    "*host-language*": .str("jw-swift"),
    "=": swiftClosure { xs in
        guard xs.count == 2 else {
            throw MalError.msg("Need two arguments for =")
        }
        return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
    },
    "throw": swiftClosure { xs in
        guard let val = xs.asSingleton else {
            throw MalError.msg("Need one arguments for throw")
        }
        throw MalError.val(val)
    },
    "apply": swiftClosure { xs in
        guard
            xs.count >= 2,
            case let .some(.closure(applyClosure)) = xs.first,
            let finalSeq = xs.last?.sequence
        else {
            throw MalError.msg("apply takes a function and at least one more argument, the last of which is a sequence")
        }
        let applyArgs = xs[xs.startIndex + 1..<xs.endIndex - 1] + finalSeq
        return try applyClosure.swift(applyArgs)
    },
    "map": swiftClosure { xs in
        guard case let .some((.closure(mapClosure), seqArg)) = xs.asPair, let seq = seqArg.sequence else {
            throw MalError.msg("map takes a function and sequence")
        }
        let mappedSeq = try seq.map { try mapClosure.swift(ArraySlice([$0])) }
        return .seq(true, ArraySlice(mappedSeq))
    },
    "symbol": swiftClosure { xs in
        guard case let .some(.str(s)) = xs.asSingleton else {
            throw MalError.msg("symbol takes a string argument")
        }
        return .sym(s)
    },
    "keyword": swiftClosure { xs in
        guard case let .some(.str(s)) = xs.asSingleton else {
            throw MalError.msg("symbol takes a string argument")
        }
        if s.first == Mal.keywordPrefix {
            return .str(s) // don't add prefix twice
        } else {
            return .str(String(Mal.keywordPrefix) + s)
        }
    },
    "time-ms": swiftClosure { xs in
        guard xs.isEmpty else {
            throw MalError.msg("time-ms takes no argument")
        }
        return .int(Int(Date().timeIntervalSince1970 * 1000))
    },
    "meta": swiftClosure { _ in throw MalError.msg("meta NYI") },
    "with-meta": swiftClosure { _ in throw MalError.msg("with-meta NYI") }
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

// Wrap an Mal -> Bool function in a Mal closure
fileprivate func wrapMalBool(_ name: String, _ f: @escaping (Mal) -> Bool) -> Mal {
    swiftClosure { (args: ArraySlice<Mal>) throws -> Mal in
        guard let x = args.asSingleton else {
            throw MalError.msg("Need one argument for \(name)")
        }
        return .bool(f(x))
    }
}
