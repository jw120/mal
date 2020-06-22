// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer - convert an AST to a string

/// Return a string representation of the given Mal value
public func pr_str(_ ast: Mal, readable: Bool = false) -> String {
    switch ast {
    case .int(let i):
        return String(i)
    case .list(let elements):
        return join_between(elements.map { pr_str($0, readable: readable) }, open: "(", close: ")")
    case .vec(let elements):
        return join_between(elements.map { pr_str($0, readable: readable) }, open: "[", close: "]")
    case .hashmap(let elements):
        let strElements = elements.flatMap { key, val in
            [showString(key, readable: readable), pr_str(val, readable: readable)]
        }
        return join_between(strElements, open: "{", close: "}")
    case .bool(let val):
        return val ? "true" : "false"
    case .null:
        return "nil"
    case .str(let val):
        return showString(val, readable: readable)
    case .sym(let val):
        return val
    case .closure:
        return "<function>"
    }
}

/// Join an array of strings with a space separator and given opening and closing strings
fileprivate func join_between(_ xs: [String], open: String, close: String) -> String {
    open + xs.joined(separator: " ") + close
}

/// Return the printable form of a string (including keywords)
fileprivate func showString(_ s: String, readable: Bool) {
    let readableS = readable ? escape(s) : s
    if let c = s.first {
        if c == Mal.keywordPrefix {
            return ":" + readableS
        }
    }
    return "\"" + readable "\""
}

/// Add escape sequences to newlines, double quotes and backslashes
fileprivate func escape(_ s: String) -> String {
    var newS: String = ""
    for c in s {
        if c == "\n" {
            newS.append("\\n")
        } else {
            if c == "\\"  || c == "\"" {
                newS.append("\\")
            }
            newS.append(c)
        }
    }
    return newS
}
