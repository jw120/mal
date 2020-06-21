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
        return join_between(elements.map { pr_str($0, readable: readable) }, open: "{", close: "}")
    case .bool(let val):
        return val ? "true" : "false"
    case .null:
        return "nil"
    case .str(let val):
        return "\"" + (readable ? escape(val) : val) + "\""
    case .sym(let val):
        return val
    case .closure:
        return "<function>"
    }
}

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

fileprivate func join_between(_ xs: [String], open: String, close: String) -> String {
    open + xs.joined(separator: " ") + close
}
