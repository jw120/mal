// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// mal - executable for all steps >= 2

import malLib

internal func READ(_ input: String) -> ReadResult {
    read_str(input)
}

internal func EVAL(_ ast: Mal) -> Mal {
    ast
}

internal func PRINT(_ ast: Mal) -> String {
    pr_str(ast, readable: true)
}

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        switch READ(s) {
        case .value(let e):
            print(PRINT(EVAL(e)))
        case .err(let msg):
            print(msg)
        case .nothing:
            break
        }
    } else {
        break
    }
}
