// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// eval - evaluate a Mal expression

/// Evaluate a mal value witin the given environment
public func eval(_ ast: Mal, _ env: Env) throws -> Mal {
    if let (head, tail) = ast.headTail {
        switch head {
        case .sym("def!"):
            return try defSpecialForm(tail, env)
        case .sym("let*"):
            return try letSpecialForm(tail, env)
        default:
            break
        }
    }
    let evaluatedAst = try evalAst(ast, env)
    if let (head, tail) = evaluatedAst.headTail {
        switch head {
        case .closure(let c):
            return try c(tail)
        default:
            throw MalError.msg("Attempt to apply a non-function")
        }
    }
    return evaluatedAst
}

/// Evaluate a mal ast without an apply phase
fileprivate func evalAst(_ ast: Mal, _ env: Env) throws -> Mal {
    switch ast {
    case .sym(let s):
        return try env.get(s)
    case .list(let xs):
        return .list(try xs.map({ x in try eval(x, env) }))
    case .vec(let xs):
        return .vec(try xs.map({ x in try eval(x, env) }))
    case .hashmap(let xs):
        return .hashmap(try xs.mapValues({ v in try eval(v, env) }))
    default:
        return ast
    }
}

/// Handle the def! special form
fileprivate func defSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    switch args.asPair {
    case .some((.sym(s), val)):
        evaluatedVal = eval(val, env)
        env.set(s, evaluatedVal)
        return evaluatedVal
    default:
        break
    }
}

fileprivate func letSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    if let (firstArg, secondArg) = args.asPair {
        if let bindinsgs = firstArg.sequence {
            let letEnv = Env(outer: env)
            try add(env: env, alternating: bindings)
            return try eval(secondArg, letEnv)
        }
    }
    throw MalError.msg("let* needs a sequence of bindings and a value")
}

fileprivate func add(env: Env, alternating: ArraySlice<T>) throws -> None {
    if !alternating.isMultiple(of: 2) {
        throw MalError.msg("Bindings must have an even number of elements")
    }
    for i in stride(from: alternating.startIndex, to: alternating.endIndex, by: 2) {
        switch alternating[i] {
        case .sym(s):
            env.add(s, try eval(alternating[i + 1], env))
        default:
            throw MalError.msg("Bindings must be symbols")
        }
    }
}
