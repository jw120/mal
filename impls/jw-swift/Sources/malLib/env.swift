// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// env - read an AST from a string

public class Env: Equatable {
    public let outer: Env?
    public var data: [String: Mal]

    public init(
        outer: Env? = nil,
        data: [String: Mal] = Dictionary(),
        binds: [String] = [],
        exprs: [Mal] = []) {
        self.outer = outer
        self.data = data
        for (symName, expr) in zip(binds, exprs) {
            set(symName, expr)
        }
    }

    public func set(_ s: String, _ v: Mal) {
        self.data[s] = v
    }

    public func find(_ s: String) -> Env? {
        if self.data[s] != nil {
            return self
        }
        return self.outer?.find(s)
    }

    public func get(_ s: String) throws -> Mal {
        if let val = self.find(s)?.data[s] {
            return val
        }
        throw MalError.msg("\(s) not found")
    }

    // We implement reference equality to conform to Equatable and allow testing
    public static func == (lhs: Env, rhs: Env) -> Bool {
        lhs === rhs
    }
}
