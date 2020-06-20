// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// step 0 - basic repl

internal func READ(_ input: String) -> String {
    input
}

internal func EVAL(_ ast: String) -> String {
    ast
}

internal func PRINT(_ ast: String) -> String {
    ast
}

internal func rep(_ s: String) -> String {
    PRINT(EVAL(READ(s)))
}

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        print(rep(s))
    } else {
        break
    }
}
