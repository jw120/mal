# Racket implementation of mal a la Beautiful Racket

* Tokenize and parse (a la BR)
* Convert to racket via BR expander
* Use racket evaluator
* Use customer printer

Implementation of mal as a translation to racket
- repl
- #lang
- Dr Racket repl (a la Basic in BR)


Implementation
- Types mapped direct from mal to racket: String, Number, List, Vector (immutable), Hash-map (immutable)
- Keywords held as prefixed strings (as racket keywords have special meaning in racket evaluation
- Exceptions
   - Empty list needs quoting in racket (not in mal)





Todo
- Stop using #lang br

raco pkg install --auto beautiful-racket

cd brmal
raco pkg install
