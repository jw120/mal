repl_env = Core.new_env
Repl.run(
  &Reader.read_str/1,
  &(Eval.eval(&1, repl_env)),
  &(Printer.pr_str(&1, true)))
