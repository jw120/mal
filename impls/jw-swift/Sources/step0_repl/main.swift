// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// step 0 - basic repl

func READ(_ input: String) -> String {
    input
}

func EVAL(_ ast: String) -> String {
    ast
}

func PRINT(_ ast: String) -> String {
    ast
}

func rep(_ s: String) -> String {
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
