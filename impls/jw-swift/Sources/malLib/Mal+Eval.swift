// jw-swift implementation of mal
//
// (C) Joe Watson 2020-06-11
//
// Eval - extend Mal type to provide evaluation

extension Mal {
    /// Evaluate this mal value witin the given environment
    public func eval(_ evalEnv: Env) throws -> Mal {
        // We loop to avoid calling eval recursively when possible
        var env: Env = evalEnv
        var ast: Mal = self
        tcoLoop: while true {
            ast = try ast.macroExpand(env)
            // If not a non-empty list, just evaluate without apply phase and done
            guard let (head, tail) = ast.listHeadTail else {
                return try ast.evalAst(env)
            }
            // If a non-empty list, first check for a special form
            switch try Mal.specialForm(head: head, arguments: tail, env: env) {
            case .Complete(let val):
                return val
            case .ToEvaluate(let specialAst, let specialEnv):
                ast = specialAst
                env = specialEnv
                continue tcoLoop
            case .NotSpecial:
                break
            }
            // If not a special form, evaluate list and apply
            guard let (evalHead, evalTail) = try ast.evalAst(env).listHeadTail else {
                throw MalError.msg("List became a non-list")
            }
            guard case let .closure(c, _) = evalHead else {
                throw MalError.msg("Attempt to apply a non-function")
            }
            // If no mal form available, use the swift form
            guard let (closureAst, params, closureEnv) = c.mal else {
                return try c.swift(evalTail)
            }
            // If a mal closure, then evaluate by looping
            ast = closureAst
            env = Env(outer: closureEnv, binds: params, exprs: evalTail)
        }
        return ast
    }

    /// Evaluate this mal value without an apply phase
    private func evalAst(_ env: Env) throws -> Mal {
        switch self {
        case .sym(let s):
            return try env.get(s)
        case .seq(let seqFlag, let xs, _):
            return .seq(seqFlag, try ArraySlice(xs.map({ x in try x.eval(env) })), nil)
        case .hashmap(let xs, _):
            return .hashmap(try xs.mapValues({ v in try v.eval(env) }), nil)
        default:
            return self
        }
    }

    /// Return a macro-expanded version 
    private func macroExpand(_ env: Env) throws -> Mal {
        var ast = self
        while try ast.isMacroCall(env: env) {
            guard
                let (head, tail) = ast.listHeadTail,
                case let .sym(s) = head,
                env.find(s) != nil, // avoid exception from get
                case let .closure(c, _) = try env.get(s),
                c.isMacro else {
                throw MalError.msg("Internal error non-macro in macro \(ast)")
            }
            ast = try c.swift(tail)
        }
        return ast
    }

    /// Possible results from trying to match against a special form
    private enum SpecialFormResult {
        case Complete(Mal) // matched a special form and evaluation complete
        case ToEvaluate(Mal, Env) // matched a special form and further evaluation needed via TCO
        case NotSpecial // did not match any special form
    }

    /// Given the head and tail of a mal list, apply any matching special form
    private static func specialForm(head: Mal, arguments: ArraySlice<Mal>, env: Env) throws -> SpecialFormResult {
        guard case let .sym(symbolName) = head else {
            return .NotSpecial
        }
        switch symbolName {
        case "def!":
            return try defSpecialForm(arguments, env)
        case "defmacro!":
            return try defMacroSpecialForm(arguments, env)
        case "do":
            return try doSpecialForm(arguments, env)
        case "fn*":
            return try fnSpecialForm(arguments, env)
        case "if":
            return try ifSpecialForm(arguments, env)
        case "let*":
            return try letSpecialForm(arguments, env)
        case "macroexpand":
            return try macroExpandSpecialForm(arguments, env)
        case "quasiquote":
            return try quasiquoteSpecialForm(arguments, env)
        case "quasiquoteexpand":
            return try quasiquoteexpandSpecialForm(arguments)
        case "quote":
            return try quoteSpecialForm(arguments, env)
        case "try*":
            return try trySpecialForm(arguments, env)
        default:
            return .NotSpecial
        }
    }

    private static func defSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard case let .some((.sym(s), val)) = args.asPair else {
            throw MalError.msg("def! needs a symbol and a value")
        }
        let evaluatedVal = try val.eval(env)
        env.set(s, evaluatedVal)
        return .Complete(evaluatedVal)
    }

    private static func defMacroSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard case let .some((.sym(s), val)) = args.asPair,
            case let .closure(c, _) = try val.eval(env)else {
            throw MalError.msg("defmacro! needs a symbol and a function")
        }
        let macro: Mal = .closure(MalClosure(mal: c.mal, swift: c.swift, isMacro: true), nil)
        env.set(s, macro)
        return .Complete(macro)
    }

    private static func doSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard let lastValue = args.last else {
            throw MalError.msg("do needs at least one argument")
        }
        for a in args.dropLast(1) {
            _ = try a.eval(env)
        }
        return .ToEvaluate(lastValue, env)
    }

    private static func fnSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard let (bindMal, body) = args.asPair else {
            throw MalError.msg("fn* needs two arguments")
        }
        guard let bindSeq = bindMal.sequence else {
            throw MalError.msg("fn* needs a sequence and a body as arguments")
        }
        let bindStrings: [String] = try bindSeq.map { (m: Mal) throws -> String in
            guard case let .sym(name) = m else {
                throw MalError.msg("fn* binding list must be made up of symbols")
            }
            return name
        }
        return .Complete(.closure(MalClosure(
            mal: (body, bindStrings, env),
            swift: { (fnArgs: ArraySlice<Mal>) -> Mal in
                let fnEnv = Env(outer: env, binds: bindStrings, exprs: fnArgs)
                return try body.eval(fnEnv)
            },
            isMacro: false
        ), nil))
    }

    private static func ifSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        func evalIf(_ c: Mal, _ t: Mal, _ f: Mal) throws -> Mal {
            switch try c.eval(env) {
            case .null, .bool(false):
                return f
            default:
                return t
            }
        }
        if let (condition, trueValue, falseValue) = args.asTriple {
            return try .ToEvaluate(evalIf(condition, trueValue, falseValue), env)
        }
        if let (condition, trueValue) = args.asPair {
            return try .ToEvaluate(evalIf(condition, trueValue, .null), env)
        }
        throw MalError.msg("if needs two or three arguments")
    }

    private static func letSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        func add(env: Env, alternating: ArraySlice<Mal>) throws {
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

        guard let (firstArg, secondArg) = args.asPair else {
            throw MalError.msg("let* needs two arguments")
        }
        guard let bindings = firstArg.sequence else {
            throw MalError.msg("let* needs a sequence of bindings and a value")
        }
        let letEnv = Env(outer: env)
        try add(env: letEnv, alternating: bindings)
        return .ToEvaluate(secondArg, letEnv)
    }

    private static func macroExpandSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard let val = args.asSingleton else {
            throw MalError.msg("macroexpand takes one argument")
        }
        return .Complete(try val.macroExpand(env))
    }

    private static func quasiquote(_ ast: Mal) throws -> Mal {
        switch ast {
        case .seq(false, let xs, _):
            return .seq(true, [.sym("vec"), try qq_list(.seq(true, xs, nil))], nil)
        case .seq(true, let xs, _):
            guard case let .some((.sym("unquote"), val)) = xs.asPair else {
                return try qq_list(ast)
            }
            return val
        case .hashmap:
            return .seq(true, [.sym("quote"), ast], nil)
        case .sym:
            return .seq(true, [.sym("quote"), ast], nil)
        default:
            return ast
        }
    }

    private static func qq_list(_ ast: Mal) throws -> Mal {
        guard let (astHead, astTail) = ast.seqHeadTail else {
            return ast
        }
        if let (headHead, headTail) = astHead.seqHeadTail,
            headHead == .sym("splice-unquote"),
            let headTailVal = headTail.asSingleton {
            return .seq(true, [.sym("concat"), headTailVal, try qq_list(.seq(true, astTail, nil))], nil)
        }
        return .seq(true, [.sym("cons"), try quasiquote(astHead), try qq_list(.seq(true, astTail, nil))], nil)
    }

    private static func quasiquoteSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard let val = args.asSingleton else {
            throw MalError.msg("quasiquote needs one arguments")
        }
        return .ToEvaluate(try quasiquote(val), env)
    }

    private static func quasiquoteexpandSpecialForm(_ args: ArraySlice<Mal>) throws -> SpecialFormResult {
        guard let val = args.asSingleton else {
            throw MalError.msg("quasiquoteexpand needs one arguments")
        }
        return .Complete(try quasiquote(val))
    }

    private static func quoteSpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        guard let val = args.asSingleton else {
            throw MalError.msg("Need one argument for quote")
        }
        return .Complete(val)
    }

    private static func trySpecialForm(_ args: ArraySlice<Mal>, _ env: Env) throws -> SpecialFormResult {
        if let value = args.asSingleton { // try* without catch*
            return .ToEvaluate(value, env)
        }
        guard
            let (value, catchList) = args.asPair,
            let (catchHead, catchTail) = catchList.listHeadTail,
            catchHead == .sym("catch*"),
            let (catchSym, catchForm) = catchTail.asPair,
            case let .sym(catchSymName) = catchSym
            else {
            throw MalError.msg("try* takes a value and a catch* clause")
        }
        var caughtValue: Mal = .null
        do {
            return try .Complete(value.eval(env))
        } catch MalError.val(let v) {
            caughtValue = v
        } catch MalError.msg(let s) {
            caughtValue = .str(s)
        } catch {
            caughtValue = .str("non-mal error")
        }
        let catchEnv = Env(outer: env)
        catchEnv.set(catchSymName, caughtValue)
        return .ToEvaluate(catchForm, catchEnv)
    }
}
