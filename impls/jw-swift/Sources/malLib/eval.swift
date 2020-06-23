// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// eval - provide Mal type to provide evaluation

extension Mal {

    /// Evaluate this mal value witin the given environment
    public func eval(_ env: Env) throws -> Mal {
    if let (head, tail) = self.headTail {
        switch head {
        case .sym("def!"):
            return try defSpecialForm(tail, env)
        case .sym("do"):
            return try doSpecialForm(tail, env)
        case .sym("if"):
            return try ifSpecialForm(tail, env)
        case .sym("let*"):
            return try letSpecialForm(tail, env)
        default:
            break
        }
    }
    let evaluated = try self.evalAst(env)
    if let (head, tail) = evaluated.headTail {
        switch head {
        case .closure(let c):
            return try c(tail)
        default:
            throw MalError.msg("Attempt to apply a non-function")
        }
    }
    return evaluated
}

    /// Evaluate this mal value without an apply phase
    fileprivate func evalAst(_ env: Env) throws -> Mal {
    switch self {
    case .sym(let s):
        return try env.get(s)
    case .list(let xs):
        return .list(try ArraySlice(xs.map({ x in try x.eval(env) })))
    case .vec(let xs):
        return .vec(try ArraySlice(xs.map({ x in try x.eval(env) })))
    case .hashmap(let xs):
        return .hashmap(try xs.mapValues({ v in try v.eval(env) }))
    default:
        return ast
    }
}

/// Handle the def! special form
fileprivate func defSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    switch args.asPair {
    case .some((.sym(let s), let val)):
        let evaluatedVal = try val.eval(env)
        env.set(s, evaluatedVal)
        return evaluatedVal
    default:
        throw MalError.msg("def! needs a symbol and a value")
    }
}

/// Handle the def! special form
fileprivate func doSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    var lastValue: Mal? = nil
    for a in args {
        lastValue = try a.eval(env)
    }
    if let returnValue = lastValue {
        return returnValue
    } else {
        throw MalError.msg("do needs at least one argument")
    }
}

/// Handle the if special form
fileprivate func ifSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {

    func evalIf(_ c: Mal, _ t: Mal, _ f: Mal) throws -> Mal {
        switch try c.eval(env) {
        case .null, .bool(false):
            return try f.eval(env)
        default:
            return try t.eval(env)
        }
    }

    if let (condition, trueValue, falseValue) = args.asTriple {
        return try evalIf(condition, trueValue, falseValue)
    }
    if let (condition, trueValue) = args.asPair {
        return try evalIf(condition, trueValue, .null)
    }
    throw MalError.msg("if needs two or three arguments")
}

/// Handle the let* special form
fileprivate func letSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    if let (firstArg, secondArg) = args.asPair {
        if let bindings = firstArg.sequence {
            let letEnv = Env(outer: env)
            try add(env: letEnv, alternating: bindings)
            return try secondArg.eval(letEnv)
        }
    }
    throw MalError.msg("let* needs a sequence of bindings and a value")
}

fileprivate func add(env: Env, alternating: ArraySlice<Mal>) throws {
    if !alternating.count.isMultiple(of: 2) {
        throw MalError.msg("Bindings must have an even number of elements")
    }
    for i in stride(from: alternating.startIndex, to: alternating.endIndex, by: 2) {
        switch alternating[i] {
        case .sym(let s):
            env.set(s, try alternating[i + 1].eval(env))
        default:
            throw MalError.msg("Bindings must be symbols")
        }
    }
}
