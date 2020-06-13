// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// step 1 - read and print

func READ(_ input: String) -> ReadResult {
    read_str(input)
}

func EVAL(_ ast: Mal) -> Mal {
    ast
}

func PRINT(_ ast: Mal) -> String {
    pr_str(ast)
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
