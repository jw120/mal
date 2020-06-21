// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// core - define the core environment

/// Starting environment for the repl
public let prelude = Env(
    outer: nil,
    data: [
        "+": wrapIntBinary("+", +),
        "-": wrapIntBinary("-", -),
        "*": wrapIntBinary("*", *),
        "/": wrapIntBinary("/", /)
    ]
)

fileprivate func wrapIntBinary(_ name: String, _ f: @escaping (Int, Int) -> Int) -> Mal {
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
