// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// mal - executable for all steps >= 2

import malLib

/// Read a mal expression, evaluate it and convert to a String. If there is no mal expression in the string
/// (including one with just spaces and comments), return nil
internal func rep(_ s: String) throws -> String? {
    switch read_str(s) {
    case .value(let readValue):
        do {
            let evaluatedValue: Mal = try readValue.eval(prelude)
            return evaluatedValue.pr_str(readable: true)
        } catch MalError.val(let v) {
            return "Error: \(v.pr_str(readable: true))"
        } catch MalError.msg(let s) {
            return "Error: \(s)"
        }
    case .err(let msg):
        return msg
    case .nothing:
        return .none
    }
}

///Read-evaluate-print loop
internal func repl() throws {
    prelude.set("*ARGV*", .seq(true, []))
    while true {
        print("user> ", terminator: "")
        guard let input = readLine() else {
            break
        }
        if let output = try rep(input) {
            print(output)
        }
    }
}

/// Run the mal code in a file
internal func run(contentsOfFile: String, arguments: ArraySlice<String>) throws {
    prelude.set("*ARGV*", .seq(true, ArraySlice(arguments.map { .str($0) })))
    _ = try rep("(load-file \"" + contentsOfFile + "\")")
}

/// Starting environment for the repl
internal let prelude = Env(outer: nil, data: core)

// Run our mal start-up code
internal let startupCode: [String] = [
    "(def! not (fn* (a) (if a false true)))",
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))",
    "(defmacro! cond (fn* (& xs) (if (> (count xs) 0) (list 'if (first xs) " +
        "(if (> (count xs) 1) (nth xs 1) (throw \"odd number of forms to cond\")) (cons 'cond (rest (rest xs)))))))"
]

try startupCode.forEach { s in
    switch read_str(s) {
    case .value(let v):
        _ = try v.eval(prelude)
    default:
        throw MalError.msg("Failure in startup code: \(s)")
    }
}

prelude.set("eval", swiftClosure { args in
    guard args.count == 1, let ast = args.first else {
        throw MalError.msg("Need one string argument for slurp")
    }
    return try ast.eval(prelude)
}
)

if CommandLine.arguments.count > 1 {
    let fileName = CommandLine.arguments[1]
    let arguments = CommandLine.arguments.dropFirst(2)
    try run(contentsOfFile: fileName, arguments: arguments)
} else {
    try repl()
}
