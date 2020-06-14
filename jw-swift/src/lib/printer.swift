// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// printer - convert an AST to a string

public func pr_str(_ ast: Mal, readable: Bool = false) -> String {
    switch ast {
    case .int(let i):
        return String(i)
    case .list(let elements):
        return join_between(elements.map { pr_str($0, readable: readable) }, open: "(", close: ")")
    case .bool(let val):
        return val ? "true" : "false"
    case .null:
        return "nil"
    case .sym(let val):
        return val
    }
}

private func join_between(_ xs: [String], open: String, close: String) -> String {
    open + xs.joined(separator: " ") + close
}
