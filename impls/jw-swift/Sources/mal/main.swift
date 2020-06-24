// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// mal - executable for all steps >= 2

import malLib

/// Starting environment for the repl
public let prelude = Env(outer: nil, data: core)

// Run our mal start-up code
fileprivate let startupCode: [String] = [
    "(def! not (fn* (a) (if a false true)))",
    "(def! load-file (fn* (f) (eval (read-string (str \"(do \" (slurp f) \"\nnil)\")))))"
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

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        switch read_str(s) {
        case .value(let readValue):
            do {
                let evaluatedValue: Mal = try readValue.eval(prelude)
                print(evaluatedValue.pr_str(readable: true))
            } catch MalError.val(let v) {
                print("Error: ", v.pr_str(readable: true))
            } catch MalError.msg(let s) {
                print("Error: \(s)")
            }
        case .err(let msg):
            print(msg)
        case .nothing:
            break
        }
    } else {
        break
    }
}
