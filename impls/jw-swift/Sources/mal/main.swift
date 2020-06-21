// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// mal - executable for all steps >= 2

import malLib

while true {
    print("user> ", terminator: "")
    if let s = readLine() {
        switch read_str(s) {
        case .value(let readValue):
            let evaluatedValue: Mal = try eval(readValue, prelude)
            print(pr_str(evaluatedValue, readable: true))
        case .err(let msg):
            print(msg)
        case .nothing:
            break
        }
    } else {
        break
    }
}
