// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// eval - provide Mal type to provide evaluation

extension Mal {
    /// Evaluate this mal value witin the given environment
    public func eval(_ env: Env) throws -> Mal {
        // We loop to avoid calling eval recursively when possible
        var ast: Mal = self
        var env: Env = env
        tcoLoop: while true {

            // if we are a non-empty list
            if let (head, tail) = self.headTail {
                switch head { // check for special forms
                case .sym("def!"):
                    return try defSpecialForm(tail, env)
                case .sym("do"): // last value of do returned for evaluation
                    ast =  try doSpecialForm(tail, env)
                    continue tcoLoop
                case .sym("fn*"):
                    return try fnSpecialForm(tail, env)
                case .sym("if"): // second or third argument returned for evaluation
                    ast = try ifSpecialForm(tail, env)
                    continue tcoLoop
                case .sym("let*"): // let body returned for evaluation
                    (ast, env) = try letSpecialForm(tail, env)
                    continue tcoLoop
                default:
                    break
                }
                // not a special form, so evaluate list and apply
                let evaluatedList = try self.evalAst(env)
                if let (evalHead, evalTail) = evaluatedList.headTail {
                    guard case let .closure(let c) = evalHead else {
                        throw MalError.msg("Attempt to apply a non-function")
                    }
                    // if not a mal closure, then call the swift function
                    guard let (ast, params, env) = c.mal else {
                        return try c.swift(evalTail)
                    }
                    // if a mal closure, then evaluate
                    env = Env(outer: env, binds: params, exprs: evalTail)
                }
            }
            ast = try ast.evalAst(env)
        }
        return ast
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
            return self
        }
    }
}

/// Handle the def! special form
fileprivate func defSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    guard case let (.sym(let s), let val) = args.asPair else {
        throw MalError.msg("def! needs a symbol and a value")
    }
    let evaluatedVal = try val.eval(env)
    env.set(s, evaluatedVal)
    return evaluatedVal
}

/// Handle the def! special form. Returns last value to be evaluated
fileprivate func doSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    guard let lastValue = args.last else {
        throw MalError.msg("do needs at least one argument")
    }
    for a in args.dropLast(1) {
        _ = try a.eval(env)
    }
    return lastValue
}

/// Handle the fn* special form
fileprivate func fnSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    guard let (bindMal, body) = args.asPair else {
        throw MalError.msg("fn* needs two arguments")
    }
    guard let bindSeq = bindMal.sequence else {
        throw MalError.msg("fn* needs a sequence and a body as arguments")
    }
    let bindStrings: [String] = try bindSeq.map { (m: Mal) throws -> String in
        guard case let .sym(name) = m {
            throw MalError.msg("fn* binding list must be made up of symbols")
        }
        return name
    }
    return .closure(MalClosure(
        mal: (body, bindStrings, env),
        swift: { (fnArgs: ArraySlice<Mal>) -> Mal in
            let fnEnv = Env(outer: env, binds: bindStrings, exprs: fnArgs)
            return try body.eval(fnEnv)
        }
    ))
}

/// Handle the if special form. Returns an expression to be evaluated
fileprivate func ifSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> Mal {
    func evalIf(_ c: Mal, _ t: Mal, _ f: Mal) throws -> Mal {
        switch try c.eval(env) {
        case .null, .bool(false):
            return f
        default:
            return t
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

/// Handle the let* special form. Returns an expression and an environment to be evaluated
fileprivate func letSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> (Mal, Env) {
    guard let (firstArg, secondArg) = args.asPair else {
            throw MalError.msg("let* needs two arguments")
    }
    guard let bindings = firstArg.sequence else {
        throw MalError.msg("let* needs a sequence of bindings and a value")
    }
    let letEnv = Env(outer: env)
    try add(env: letEnv, alternating: bindings)
    return (secondArg, letEnv)
}

fileprivate func add(env: Env, alternating: ArraySlice<Mal>) throws {
    if !alternating.count.isMultiple(of: 2) {
        throw MalError.msg("Bindings must have an even number of elements")
    }
    for i in stride(from: alternating.startIndex, to: alternating.endIndex, by: 2) {
        guard case let .sym(s) = alternating[i] else {
            throw MalError.msg("Bindings must be symbols")
        }
        env.set(s, try alternating[i + 1].eval(env))
    }
}
