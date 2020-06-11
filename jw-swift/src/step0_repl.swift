// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11

func READ(_ input: String) -> String {
    return input
}

func EVAL(_ ast: String) -> String {
    return ast
}

func PRINT(_ ast: String) -> String {
    return ast
}

func rep(_ s: String) -> String {
    return PRINT(EVAL(READ(s)))
}

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        print(rep(s))
    } else {
        break
    }
}
