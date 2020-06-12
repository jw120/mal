// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// step 1 - read and print

func READ(_ input: String) -> ReadResult {
    read_str(input)
}

func EVAL(_ ast: Mal) -> Mal {
    return ast
}

func PRINT(_ _: Mal) -> String {
    return "PRINT NYI"
}

func rep(_ s: String) -> String {
    switch READ(s) {
        case .value(let e):
            return PRINT(EVAL(e))
        case .err(let msg):
            return msg
        case .nothing:
            return ""
    }
}

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        print(rep(s))
    } else {
        break
    }
}
