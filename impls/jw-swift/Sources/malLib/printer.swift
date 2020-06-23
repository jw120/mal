// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer - allow Mal type to be converted to a string

extension Mal {
    /// Return a string representation of the given Mal value
    public func pr_str(readable: Bool = false) -> String {
        switch self {
        case .int(let i):
            return String(i)
        case .list(let elements):
            return join_between(elements.map { $0.pr_str(readable: readable) }, open: "(", close: ")")
        case .vec(let elements):
            return join_between(elements.map { $0.pr_str(readable: readable) }, open: "[", close: "]")
        case .hashmap(let elements):
            let strElements: [String] = elements.flatMap { key, val in
                [showString(key, readable: readable), val.pr_str(readable: readable)]
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
}

/// Join an array of strings with a space separator and given opening and closing strings
fileprivate func join_between(_ xs: [String], open: String, close: String) -> String {
    open + xs.joined(separator: " ") + close
}

/// Return the printable form of a string (including keywords)
fileprivate func showString(_ s: String, readable: Bool) -> String {
    // keyword
    if s.first == .some(Mal.keywordPrefix) {
        var kw = s
        kw.remove(at: kw.startIndex)
        return ":" + kw
    }

    // string
    return readable ? "\"" + escape(s) + "\"" : s
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
