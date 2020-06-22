// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// core - define the core environment

/// Starting environment for the repl
public let prelude = Env(
    outer: nil,
    data: [
        "+": wrapInt2Int("+", +),
        "-": wrapInt2Int("-", -),
        "*": wrapInt2Int("*", *),
        "/": wrapInt2Int("/", /),
        "<": wrapInt2Bool("/", <),
        ">": wrapInt2Bool("/", >),
        "<=": wrapInt2Bool("/", <=),
        ">=": wrapInt2Bool("/", >=),
        "=": .closure({ xs in
            if xs.count != 2 {
                throw MalError.msg("Need two arguments for =")
            }
            return .bool(xs[xs.startIndex] == xs[xs.startIndex + 1])
        }),
        "list": .closure({ xs in .list(xs)}),
        "list?": .closure({ xs in
            switch xs.first {
            case .list:
                return .bool(true)
            default:
                return .bool(false)
            }
        }),
        "empty?": .closure({ xs in
            switch xs.first {
            case .list(let ys):
                return .bool(ys.isEmpty)
            default:
                throw MalError.msg("Expected a list as the argument for empty?")
            }
        }),
        "count": .closure({ xs in
            switch xs.first {
            case .list(let ys):
                return .int(ys.count)
            case .null:
                return .int(0)
            default:
                throw MalError.msg("Expected a list as the argument for count")
            }
        }),
        "prn": .closure({ xs in
            if let v = xs.first {
                print(pr_str(v, readable: true))
            }
            return .null
        })

    ]
)

/// Wrap an (Int, Int) -> Int function in a closure
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

// Wrap an (Int, Int) -> Bool function in a closure
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
