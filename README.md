# GABS

GABS is a pure functional language, featuring a powerful polymorphic and inferred type system based on the classic Hindley-Milner type theory. It includes a concrete parser and interactive read-eval-print loop (REPL).

# Building

This project requires the [Stack](https://docs.haskellstack.org/en/stable/README/) tool for Haskell. Build the project with `stack build`. 

# Running

After building, you may run GABS with the command `stack exec`. To pass arguments to GABS, you may instead using the command `stack exec -- <args>`, where `<args>` should be substituted for the particular flags. Use the command `stack exec -- --help` for information on the possible flags.

# REPL

You may enter the REPL via the command `stack exec -- --repl`. From here, you may enter an expression to evaluate it. If you prefix your query with `:t`, the REPL will instead display the inferred type of the expression. Type `:q` to quit, and `:help` for a complete listing of commands.

# Examples

Below are examples of well-formed and well-typed expressions, being evaluated and typed within the REPL:

```sh
> 1 + 7 * 3 - 2
20
> True of False and not False
True
> (λx. λy. x y) (λz. True) 3
True
> letrec fact = λx. if x > 1 then x * fact (x-1) else 1 in fact 4
24
> :t fix λapply f v t. if t = 0 then v else apply f (f v) (t-1)
(a -> a) -> a -> Int -> a
> :t λ f g x. f (g x)
(a -> b) -> (c -> a) -> c -> b
```
