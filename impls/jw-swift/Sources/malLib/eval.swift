// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// eval - evaluate a Mal expression

/// Evaluate a mal value witin the given environment
public func eval(_ ast: Mal, _ env: Env) throws -> Mal {
    if ast.isEmptyList {
        return ast
    }
    let evaluatedAst = try eval_ast(ast, env)
    if let (head, tail) = evaluatedAst.headTail {
        switch head {
        case .closure(let c):
            return try c(tail)
        default:
            throw MalError.msg("Attempt to apply a non-function")
        }
    } else {
        return evaluatedAst
    }
}

fileprivate func eval_ast(_ ast: Mal, _ env: Env) throws -> Mal {
    switch ast {
    case .sym(let s):
        return try env.get(s)
    case .list(let xs):
        return .list(try xs.map({ x in try eval(x, env) }))
    case .vec(let xs):
        return .vec(try xs.map({ x in try eval(x, env) }))
    default:
        return ast
    }
}
