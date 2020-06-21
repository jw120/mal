// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// core - define the core environment

/// Starting environment for the repl
public let prelude = Env(
    outer: nil,
    data: [
        "+": Mal.closure(malPlus)
    ]
)

fileprivate func malPlus(_ args: ArraySlice<Mal>) throws -> Mal {
    if args.count != 2 {
        throw MalError(.str("Need two argument"))
    }
    switch (args[args.startIndex], args[args.startIndex + 1]) {
    case (.int(let x), .int(let y)):
        return .int(x + y)
    default:
        throw MalError(.str("Arguments must be integers"))
    }
}
