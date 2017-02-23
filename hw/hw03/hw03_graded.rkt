;;> Student: abc (Daniel Hennessy <hennessy.da@husky.neu.edu>)
;;> Student: mspringer (Matthew Springer <springer.m@husky.neu.edu>)
;;> Maximum points for this assignment: <+100>
;;> Graded by Kaila Corrington
#|
    Project: Programming Languages HW #3
    Authors: Matthew Springer and Daniel Hennessy
    Date     January 31, 2017
 |#

#lang pl 03

(define minutes-spent 120)

;; ** The MUWAE interpreter

#| BNF for the MUWAE language:
     <MUWAE> ::= <num>
             | { + <MUWAE> <MUWAE> }
             | { - <MUWAE> <MUWAE> }
             | { * <MUWAE> <MUWAE> }
             | { / <MUWAE> <MUWAE> }
             | { with { <id> <MUWAE> } <MUWAE> }
             | <id>
             | { sqrt <MUWAE> }
|#

;; MUWAE abstract syntax trees
(define-type MUWAE
  [Num  (Listof Number)]
  [Add  MUWAE MUWAE]
  [Sub  MUWAE MUWAE]
  [Mul  MUWAE MUWAE]
  [Div  MUWAE MUWAE]
  [Id   Symbol]
  [With Symbol MUWAE MUWAE]
  [Sqrt MUWAE]
  )

(: parse-sexpr : Sexpr -> MUWAE)
;; parses s-expressions into MUWAEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num (list n))]
    [(symbol: name) (Id name)]
    [(cons 'with more)
     (match sexpr
       [(list 'with (list (symbol: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'sqrt expr) (Sqrt (parse-sexpr expr))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> MUWAE)
;; parses a string containing a MUWAE expression to a MUWAE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

#| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <MUWAE>s, `x' is some <id>,
   `y' is a *different* <id>)
      N[v/x]                = N
      {+ E1 E2}[v/x]        = {+ E1[v/x] E2[v/x]}
      {- E1 E2}[v/x]        = {- E1[v/x] E2[v/x]}
      {* E1 E2}[v/x]        = {* E1[v/x] E2[v/x]}
      {/ E1 E2}[v/x]        = {/ E1[v/x] E2[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
      {sqrt E1}[v/x]        = {sqrt E1[v/x]}
|#

(: subst : MUWAE Symbol MUWAE -> MUWAE)
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Sqrt s)  (Sqrt (subst s from to))]
    [(Add l r) (Add (subst l from to) (subst r from to))]
    [(Sub l r) (Sub (subst l from to) (subst r from to))]
    [(Mul l r) (Mul (subst l from to) (subst r from to))]
    [(Div l r) (Div (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(With bound-id named-expr bound-body)
     (With bound-id
           (subst named-expr from to)
           (if (eq? bound-id from)
             bound-body
             (subst bound-body from to)))]))

#| Formal specs for `eval':
     eval(N)         = N
     eval({+ E1 E2}) = eval(E1) + eval(E2)
     eval({- E1 E2}) = eval(E1) - eval(E2)
     eval({* E1 E2}) = eval(E1) * eval(E2)
     eval({/ E1 E2}) = eval(E1) / eval(E2)
     eval(id)        = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
     eval({sqrt E1}) = if eval(E1) >= 0: sqrt(eval(E1))
                     = else: error "sqrt requires a non-negative input"
|#
;;> Note: write as eval(x) = y if z
;;>                          w otherwise
;;> and you can simply use error!

(: eval : MUWAE -> (Listof Number))
;; evaluates MUWAE expressions by reducing them to numbers
(define (eval expr)
  (cases expr
    [(Num n) n]
    [(Add l r) (bin-op + (eval l) (eval r))]
    [(Sub l r) (bin-op - (eval l) (eval r))]
    [(Mul l r) (bin-op * (eval l) (eval r))]
    [(Div l r) (if (equal? (eval r) '(0))
                   (error 'eval "cannot divide by zero")
                   (bin-op / (eval l) (eval r)))]
    [(Sqrt e) (sqrt+ (eval e))]
    [(With bound-id named-expr bound-body)
     (eval (subst bound-body
                  bound-id
                  (Num (eval named-expr))))]
    [(Id name) (error 'eval "free identifier: ~s" name)]))

(: run : String -> (Listof Number))
;; evaluate a MUWAE program contained in a string
(define (run str)
  (eval (parse str)))

(: sqrt+ : (Listof Number) -> (Listof Number))
;; a version of `sqrt' that takes a list of numbers, and return a
;; list with twice the elements, holding the two roots of each of
;; the inputs; throws an error if any input is negative.
(define (sqrt+ ns)
  (cond [(null? ns) '()]
        [(< (first ns) 0) (error 'eval "`sqrt' requires a non-negative input")]
        [else (let [(sqr-first (sqrt (first ns)))]
           (cons sqr-first
                 (cons (- sqr-first) (sqrt+ (rest ns)))))]))
;;> <-2> Bad indentation after let, and round/square parens for let should be:
;;> (let ([x y]) (z))


;; tests for sqrt+
(test (sqrt+ '(1 4 9)) => '(1 -1 2 -2 3 -3))
(test (sqrt+ '()) => '())
(test (sqrt+ '(-9)) =error> "`sqrt' requires a non-negative input")

(: bin-op :
   (Number Number -> Number) (Listof Number) (Listof Number)
   -> (Listof Number))
;; applies a binary numeric function on all combinations
;; of numbers from the two input lists, and return the
;; list of all of the results
(define (bin-op op ls rs)
  (: helper : Number (Listof Number) -> (Listof Number))
  (define (helper l rs)
    (: f : Number -> Number)
    (define (f n)
      (op l n))
    (map f rs))
  (if (null? ls)
    null
    (append (helper (first ls) rs) (bin-op op (rest ls) rs))))

;; tests for bin-op
(test (bin-op + '() '(2)) => '())
(test (bin-op + '(1) '()) => '())
(test (bin-op + '(1) '(2)) => '(3))
(test (bin-op + '(1 3) '(2 4)) => '(3 5 5 7))

;; included tests
(test (run "5") => '(5))
(test (run "{+ 5 5}") => '(10))
(test (run "{with {x {+ 5 5}} {+ x x}}") => '(20))
(test (run "{with {x 5} {+ x x}}") => '(10))
(test (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}") => '(14))
(test (run "{with {x 5} {with {y {- x 3}} {+ y y}}}") => '(4))
(test (run "{with {x 5} {+ x {with {x 3} 10}}}") => '(15))
(test (run "{with {x 5} {+ x {with {x 3} x}}}") => '(8))
(test (run "{with {x 5} {+ x {with {y 3} x}}}") => '(10))
(test (run "{with {x 5} {with {y x} y}}") => '(5))
(test (run "{with {x 5} {with {x x} x}}") => '(5))
(test (run "{with {x 1} y}") =error> "free identifier")

;; additional tests added for 'complete coverage'
(test (run "{* 5 5}") => '(25))
(test (run "{/ 5 5}") => '(1))
(test (run "{with {x 5} {* x {with {y 3} x}}}") => '(25))
(test (run "{with {x 5} {/ x {with {y 3} x}}}") => '(1))
(test (run "{with {x 5} {/ x {with {y 0} y}}}")
      =error> "cannot divide by zero")
(test (run "{y}") =error> "bad syntax in (y)")
(test (run "{with {x 5} {/ x {with {y 3} }}}")
      =error> "bad `with' syntax in (with (y 3))")
(test (run "{+ {sqrt 1} 3}")
      => '(4 2))
(test (run "{+ {/ {+ {sqrt 1} 3} 2} {sqrt 100}}")
      => '(12 -8 11 -9))
(test (run "{sqrt {+ 16 {* {+ 1 {sqrt 1}} {/ 9 2}}}}")
      => '(5 -5 4 -4))
(test (run "{sqrt {with {x 9} x}}") => '(3 -3))
(test (run "{with {x 5} {* x {with {y 4} {sqrt 4}}}}") => '(10 -10))
(test (run "{with {x 5} {* x {with {y 4} {sqrt -4}}}}")
      =error> "`sqrt' requires a non-negative input")
(test (run "{sqrt 0}")
      => '(0 0))
;;> Note: these would all fit on one line, go ahead and keep them that way
