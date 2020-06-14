// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// reader - read an AST from a string

struct Reader {
    let tokens: [String]
    var index: Int = 0

    init(fromStringArray: [String]) {
        tokens = fromStringArray
    }

    mutating func next() -> String {
        let val = tokens[index]
        self.index += 1
        return val
    }

    func peek() -> String {
        tokens[index]
    }
}

func read_str(_ s: String) -> String {
    let r = Reader(fromStringArray: tokenize(s))
    return read_form(r)
}

func read_form(_ r: Reader) -> String {
    "NYI"
}

// [\s,]*(~@|[\[\]{}()'`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]*)
func tokenize(_ s: String) -> [String] {
    ["(", "+", "2", "2", ")"]
}
