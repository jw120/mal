// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// Env - provide environment class

public class Env: Equatable {
    private let outer: Env?
    private var data: [String: Mal]

    public init(
        outer: Env? = nil,
        data: [String: Mal] = Dictionary(),
        binds: [String] = [],
        exprs: ArraySlice<Mal> = []
        ) {
        self.outer = outer
        self.data = data
        for bindsIndex in binds.startIndex..<binds.endIndex {
            let exprsIndex = exprs.startIndex + bindsIndex - binds.startIndex
            let symName = binds[bindsIndex]
            if symName == "&" {
                if bindsIndex + 1 < binds.endIndex {
                    set(binds[bindsIndex + 1], .seq(true, exprs.suffix(from: exprsIndex)))
                }
                break
            }
            set(symName, exprs[exprsIndex])
        }
    }

    public func set(_ s: String, _ v: Mal) {
        self.data[s] = v
    }

    public func find(_ s: String) -> Env? {
        self.data[s] != nil ? self : self.outer?.find(s)
    }

    public func get(_ s: String) throws -> Mal {
        guard let val = self.find(s)?.data[s] else {
            throw MalError.msg("'\(s)' not found")
        }
        return val
    }

    // We implement reference equality to conform to Equatable and allow testing
    public static func == (lhs: Env, rhs: Env) -> Bool {
        lhs === rhs
    }
}
