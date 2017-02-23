#lang pl

;;----------------------------------------------------------------------
;; Question 1

#| This is a solution that would get full credit:

  (: numeral : (All (A) Integer -> ((A -> A) A -> A)))
  (define (numeral n)
    (cond [(< n 0) (error 'numeral "expecting a non-negative integer")]
          [(= n 0) (lambda (f x) x)]
          [else    (lambda (f x) (f ((numeral (sub1 n)) f x)))]))

The problem with it is that it involves a polymorphic higher-order
function, which Typed Racket is not dealing with well enough.  You can
verify that this works by using `#lang pl untyped' -- and a proper typed
solution is below.  Note the many `inst's that are used to tell TR what
the `numeral' function should use as the `A' type.  (Again, there was no
need to do any of these in the exam -- the above definition is
sufficient.)
|#

(: numeral : (All (A) Integer -> ((A -> A) A -> A)))
;; Takes a non-negative number in and returns the corresponding encoded
;; church numeral, as a binary function (that takes in a unary function
;; and an argument for it).
(define (numeral n)
  (cond [(< n 0) (error 'numeral "expecting a non-negative integer")]
        [(= n 0) (lambda ([f : (A -> A)] [x : A]) x)]
        [else    (lambda ([f : (A -> A)] [x : A])
                   (f (((inst numeral A) (sub1 n)) f x)))]))

;; tests from the exam, with many of these `inst's
(test (((inst numeral Integer) 10) add1 0)           => 10)
(test (((inst numeral (Listof Number)) 3) rest '(1 2 3 4 5)) => '(4 5))
(test (((inst numeral Number) 0) sin 10)            => 10)
(: double : (All (A) (Listof A) -> (Listof A)))
(define (double l) (append l l))
(define triple ((inst numeral (Listof Number)) 3))
(test (triple double '(1 2)) => '(1 2 1 2 1 2 1 2 1 2 1 2 1 2 1 2))
(test (numeral -1) =error> "expecting a non-negative integer")

#|
  Note: some people did the recursive loop completely inside
  the (lambda (f x) ...) function, which requires a helper.  A good
  example would be

    (define (numeral n)
      (lambda (f x)
        (define (helper n x)
          (cond [(< n 0) (error 'numeral "...")]
                [(= n 0) x]
                [else (helper (sub1 n) (f x))]))
        (helper n x)))

  There are various possible problem with this kind of setup.  One
  example that can be seen here is that the error is signaled only when
  the resulting encoded numeral is used, so (numeral -1) is not an error
  by itself, only when the resulting function is called.  Another
  problem is that in pretty much all cases the helper was put higher
  up (mostly as a global helper definition), complicating it by
  requiring passing `f' as an argument.  Many cases made things even
  more complicated by doing some check (eg, for a negative input)
  outside of the (lambda (f x) ...) and some check inside.

|#


;;----------------------------------------------------------------------
;; Question 2

#|

Since we're dealing with a different *kind* of expressions in one case,
we need to make up a parallel rule to <SCHLAC-TEXPR>, and add `test' to
the toplevel expression:

<SCHLAC-TOP>  ::= <SCHLAC-EXPR>
                | (define <id> <SCHLAC-EXPR>)
                | (test <SCHLAC-TEXPR> => <SCHLAC-TEXPR>)

<SCHLAC-EXPR> ::= <id>
                | (lambda (<id> <id> ...) <SCHLAC-EXPR>)
                | (<SCHLAC-EXPR> <SCHLAC-EXPR> <SCHLAC-EXPR> ...)

<SCHLAC-TEXPR> ::= <id>
                 | (lambda (<id> <id> ...) <SCHLAC-TEXPR>)
                 | (<SCHLAC-TEXPR> <SCHLAC-TEXPR> <SCHLAC-TEXPR> ...)
                 | '<racket-literal>

Note that the last rule uses <racket-literal> to specify "any Racket
literal".  Also, the rule before that uses <SCHLAC-TEXPR> as the first
subexpression, which means that even something like ('1 '2) is valid in
a test expression, leaving it to be a runtime error when actually
running.  It could have used <SCHLAC-EXPR> there to avoid such broken
application expressions, but this would also disqualify using quotes
anywhere as a *sub*expression of the function expression, which is
useful.  For example,

    (test (->nat (if (bool-> '#t) 1 2)) => '1)

But since there was no explicit example with this, only one point was
taken of for this.
|#


;;----------------------------------------------------------------------
;; Question 3

#|

a. (((blank)))
   Simple three levels of nullary `lambda's
   => (define blank (lambda () (lambda () (lambda () 660))))

b. (blank if)
   This is impossible, since `if' cannot be used as a value in Racket,
   which means that this will always be a syntax error.

c. ((first (blank)) (blank))
   Again, simple going from the inside out
   => (define blank (lambda () (list (lambda (x) 660))))
   or (define (blank) (list (lambda (x) 660)))

d. ((and blank (blank)))
   Yet again:
   => (define blank (lambda () (lambda () 660)))

e. ((lambda (blank) (blank blank)) blank)
   The only `blank' which is relevant is the last one:
   => (define (blank x) 660)

|#


;;----------------------------------------------------------------------
;; Question 4

;; This is a simple solution.  To try it, add it to the bottom of the
;; FLANG implementation, including the utility definitions for
;; `fresh-name' below.

(: simplify-applications : FLANG -> FLANG)
;; Simplify applications so they always have an identifier for the
;; function expression, moving such computations to a `with'.
(define (simplify-applications expr)
  (: sub2 : (FLANG FLANG -> FLANG) FLANG FLANG -> FLANG)
  (define (sub2 make l r)
    (make (simplify-applications l) (simplify-applications r)))
  (cases expr
    [(Num n)   expr]
    [(Add l r) (sub2 Add l r)]
    [(Sub l r) (sub2 Sub l r)]
    [(Mul l r) (sub2 Mul l r)]
    [(Div l r) (sub2 Div l r)]
    [(Id name) expr]
    [(Fun id body) (Fun id (simplify-applications body))]
    [(With id named body)
     (With id
           (simplify-applications named)
           (simplify-applications body))]
    [(Call (Id id) r) (Call (Id id) (simplify-applications r))]
    [(Call l r)
     ;; Note that it's sufficient to choose a name that does not appear
     ;; in the RHS expression.  It's of course also possible to use
     ;; (fresh-name expr) though that's unnecessarily more conservative.
     ;; Some people have kept the initial expression and used it -- that
     ;; works too, except even more conservative, but a more serious
     ;; problem is that it complicates the code unnecessarily.
     (let ([id (fresh-name r)])
       ;; Simple version:
       ;;   (With id
       ;;         (simplify-applications l)
       ;;         (Call (Id id) (simplify-applications r)))
       ;; A simpler alternative, using one recursive call -- works
       ;; because the recursive call will have an identifier for the
       ;; application, so there's no infinite loop
       (simplify-applications (With id l (Call (Id id) r))))]))

;; To get it to run, we need an actual working implementation of the
;; `fresh-name'.  Here's a quick hack that works for a few names.
(: fresh-name : FLANG -> Symbol)
;; Given a FLANG expression, return a symbol that is never mentioned
;; anywhere in it.  The return symbol is the first of `f', `f1',
;; `f2', `f3', ... that is not mentioned in the input expression.
(define (fresh-name expr)
  (define names (all-names expr))
  (or (ormap (lambda ([sym : Symbol]) (and (not (memq sym names)) sym))
             '(f f0 f1 f2 f3 f4 f5 f6 f7 f8 f9))
      'f999))
(: all-names : FLANG -> (Listof Symbol))
;; Return all identifiers that are mentioned in the input expression.
(define (all-names expr)
  (: all2 : FLANG FLANG -> (Listof Symbol))
  (define (all2 E1 E2) (append (all-names E1) (all-names E2)))
  (cases expr
    [(Num  n) '()]
    [(Add  l r)  (all2 l r)]
    [(Sub  l r)  (all2 l r)]
    [(Mul  l r)  (all2 l r)]
    [(Div  l r)  (all2 l r)]
    [(Id   name) (list name)]
    [(Call l r) (all2 l r)]
    [(With id named body) (cons id (all2 named body))]
    [(Fun  id body) (cons id (all-names body))]))

;; Tests from the exam:

(: sim-app : String -> FLANG)
(define (sim-app expr) (simplify-applications (parse expr)))

;; call with identifier is unchanged
(test (sim-app "{call x y}") => (parse "{call x y}"))

;; other calls are transformed
(test (sim-app "{call {call x y} z}")
      => (parse "{with {f {call x y}} {call f z}}"))

;; and they're transformed blindly, even if it's nonsensical
(test (sim-app "{call {+ x y} z}")
      => (parse "{with {f {+ x y}} {call f z}}"))

;; conversion happens anywhere
(test (sim-app "{* {call {call x y} z}
                   {call {with {a b} c} 3}}")
      => (parse "{* {with {f {call x y}} {call f z}}
                    {with {f {with {a b} c}} {call f 3}}}"))

;; test that the code avoids capturing `f'
(test (sim-app "{with {f whatever}
                  {call {call a b} f}}")
      => (parse "{with {f whatever}
                   {with {f0 {call a b}}
                     {call f0 f}}}"))


;;----------------------------------------------------------------------
;; Question 5

#|
The complete running code is at the end of this fine.  Look for a
"[*]" to see all of the changes -- the ones that you had assume were
done, and the extended evaluation code, and even the tests.  What you
needed to write is just the listed three new/extended cases:

    ;; Same as the unary case: just pair the information with the
    ;; environment.
    [(Fun2 id1 id2 default2-expr bound-body)
     (Fun2V id1 id2 default2-expr bound-body env)]

    ;; The two-argument application case is a minor extension of the
    ;; regular call: we just add the evaluation of the additional
    ;; expression and chain the two extensions to bind both.  (The
    ;; default expression is ignored.)
    [(Call2 fun-expr arg1-expr arg2-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(Fun2V id1 id2 default2-expr bound-body f-env)
          (eval bound-body
                (Extend id2 (eval arg2-expr env)
                        (Extend id1 (eval arg1-expr env)
                                f-env)))]
         [else (error 'eval "`call' expects a binary function, got: ~s"
                            fval)]))]

    [(Call fun-expr arg-expr)
     ;; Unary call...
     (let ([fval (eval fun-expr env)])
       (cases fval
         ;; With a unary function: same as the original `Call' case
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         ;; And this is the core of the extension: the important point
         ;; here is that we evaluate `default2-expr' in an extension of
         ;; the closure's environment.
         [(Fun2V id1 id2 default2-expr bound-body f-env)
          (let* ([fenv1 (Extend id1 (eval arg-expr env) f-env)]
                 [fenv2 (Extend id2 (eval default2-expr fenv1) fenv1)])
            (eval bound-body fenv2))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]

An alternative for the core case can avoid using a `let': since the
default expression is evaluated in the function's scope, we can just
translate it into a `with' expression:

          (eval (With id2 default2-expr bound-body)
                (Extend id1 (eval arg-expr env) f-env))

You can change the extended evaluator below to use this to see that it
works.

|#


;;----------------------------------------------------------------------
;; Question 6

;; You can run this code with "#lang pl schlac+" which has all of our
;; definitions already baked into the language (so this code shadows
;; these bindings).

(define #f 0)
(define #t 1)
;; You can try this code with different non-0 values to see that the
;; tests work

;; Q6a. The implementation of `if' is essentially the same as the
;; `zero?' that we've seen in class -- only using the two `then'/`else'
;; inputs instead of #t and #f.  This works fine since it depends on how
;; we encode natural numbers, and that didn't change.
(define if
  (lambda (c t e)
    (c (lambda (x) t) e)))

(test (->nat (if #t 1 2)) => '1)
(test (->nat (if #t (+ 4 5) (+ '1 '2))) => '9)

;; These were defined in the text.
(define and *)
(define or +)

;; Q6b. We need a `not' implementation: simple to do since we can just
;; use the (new) `if':
(define not (lambda (a) (if a #f #t)))
;; (Note that both versions we've seen in class actually depend on the
;; particular boolean encoding we had in class: flipping the arguments
;; in one case, omitting `if' which worked by relying on the boolean
;; encoding which is no longer used here.)

;; Test these boolean functions -- avoid `->bool' (which relies on the
;; previous boolean encoding) so we can test that it spits the correct
;; result.
(test (->nat (if (and #f #f) 2 3)) => '3)
(test (->nat (if (and #t #f) 2 3)) => '3)
(test (->nat (if (and #f #t) 2 3)) => '3)
(test (->nat (if (and #t #t) 2 3)) => '2)
(test (->nat (if (or  #f #f) 2 3)) => '3)
(test (->nat (if (or  #t #f) 2 3)) => '2)
(test (->nat (if (or  #f #t) 2 3)) => '2)
(test (->nat (if (or  #t #t) 2 3)) => '2)
(test (->nat (if (not #f) 2 3)) => '2)
(test (->nat (if (not #t) 2 3)) => '3)

;; Q6c. Finally, this was extremely simple: naturals are booleans, and
;; `0' is the only false, so `not' is essentially a `zero?' predicate.
(define zero? not)
(test (->nat (if (and (zero? 0) (not (zero? 3))) 2 3)) => '2)


;;----------------------------------------------------------------------
;; Question 7

;; Q7a: 5
;; 1. This basically says just "No" -- since the "In fact" part is true
;;    if the order doesn't matter.  By popular request, half of the
;;    credit was given to this choice.
;; 2. This is incorrect: the order makes no *semantical* difference.
;; 4. This is incorrect, easy to see with the second sentence, which
;;    false similarly to the above, except more stressed and therefore
;;    more easy to see through...
;; 3. This was supposed to have only the first sentence of #4 which is
;;    more subtly bogus, and instead I made a mistake and left the
;;    second one, making it incomprehensible and so even more obviously
;;    bogus...
;; 5. This is correct: something that is easy to see considering what we
;;    did in the BRANG homework, and the fact that all compilers do
;;    that, resulting in no names in the environment implementation.

;; Q7b: 2,6
;; 2. This is true as was clearly seen in the case of the `and' and `or'
;;    implementations.
;; 1. Wrong, due to #2 being correct.
;; 3. Nonsensical.
;; 6. This is true in all cases where 0 is treated as false -- but much
;;    more stressed in schlac since there are no errors.
;; 4. Wrong, due to #6 being correct.
;; 5. Nonsensical.

;; Q7c:
;; What we need here is a way to execute the same piece of code twice,
;; with some binding that will be wrongly used in the second time.  This
;; is possible with functions, leading to this simple test:
;;
;;     {with {id {fun {x} x}}
;;       {+ {call id 1}
;;          {call id 2}}}
;;
;; The broken implementation would use {fun {x} 1} in the second call,
;; leading to a bogus 2 result.

;; Q7d: 1
;; 2. This is wrong: compilation is irrelevant, and in fact the question
;;    says that this is a possible strategy for *compiling* code, so
;;    that happens always.
;; 3. Wrong for the same reason.
;; 4. It's true that you can do this when V is a numeric expression, but
;;    it can be done in many other cases too.  (Immediate function
;;    applications on numeric expressions are probably as rare as on
;;    constants.)
;; 5. Same as the above.  (In fact, I didn't even notice that strictly
;;    speaking, "numeric expression" is actually a numeric expression; I
;;    intended to say "numeric literal"...)
;; 1. Yes, this is the correct one, and it's something that we've talked
;;    about in class, saying that beta-reduction is sometimes not
;;    actually reducing the expression.


;;----------------------------------------------------------------------
;; The full code for Q5

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  ;; [*]
  [Fun2  Symbol Symbol FLANG FLANG] ; id1, id2, default2, body
  [Call  FLANG FLANG]               ; 1-arg call, uses default
  [Call2 FLANG FLANG FLANG])        ; simple 2-arg call

(: parse-sexpr : Sexpr -> FLANG)
;; parses s-expressions into FLANGs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n)    (Num n)]
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
    ;; [*]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        ;; a plain unary function (same as original version)
        (Fun name (parse-sexpr body))]
       [(list 'fun (list (symbol: name1) (symbol: name2) default) body)
        ;; new 2-argument function with a default expression
        (Fun2 name1 name2 (parse-sexpr default) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list 'call fun arg)
     ;; plain unary calls (same as original version)
     (Call (parse-sexpr fun) (parse-sexpr arg))]
    [(list 'call fun arg1 arg2)
     ;; new two-argument calls
     (Call2 (parse-sexpr fun) (parse-sexpr arg1) (parse-sexpr arg2))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Symbol FLANG ENV]
  [Fun2V Symbol Symbol FLANG FLANG ENV]) ; [*] Fun2V = Fun2 + ENV

(: lookup : Symbol ENV -> VAL)
;; lookup a symbol in an environment, return its value or throw an
;; error if it isn't bound
(define (lookup name env)
  (cases env
    [(EmptyEnv) (error 'lookup "no binding for ~s" name)]
    [(Extend id val rest-env)
     (if (eq? id name) val (lookup name rest-env))]))

(: NumV->number : VAL -> Number)
;; convert a FLANG runtime numeric value to a Racket one
(define (NumV->number val)
  (cases val
    [(NumV n) n]
    [else (error 'arith-op "expected a number, got: ~s" val)]))

(: arith-op : (Number Number -> Number) VAL VAL -> VAL)
;; gets a Racket numeric binary operator, and uses it within a NumV
;; wrapper
(define (arith-op op val1 val2)
  (NumV (op (NumV->number val1) (NumV->number val2))))

(: eval : FLANG ENV -> VAL)
;; evaluates FLANG expressions by reducing them to values
(define (eval expr env)
  (cases expr
    [(Num n) (NumV n)]
    [(Add l r) (arith-op + (eval l env) (eval r env))]
    [(Sub l r) (arith-op - (eval l env) (eval r env))]
    [(Mul l r) (arith-op * (eval l env) (eval r env))]
    [(Div l r) (arith-op / (eval l env) (eval r env))]
    [(With bound-id named-expr bound-body)
     (eval bound-body
           (Extend bound-id (eval named-expr env) env))]
    [(Id name) (lookup name env)]
    [(Fun bound-id bound-body)
     (FunV bound-id bound-body env)]
    ;; [*] new closure type
    ;; Same as the unary case: just pair the information with the
    ;; environment.
    [(Fun2 id1 id2 default2-expr bound-body)
     (Fun2V id1 id2 default2-expr bound-body env)]
    ;; The two-argument application case is a minor extension of the
    ;; regular call: we just add the evaluation of the additional
    ;; expression and chain the two extensions to bind both.  (The
    ;; default expression is ignored.)
    [(Call2 fun-expr arg1-expr arg2-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(Fun2V id1 id2 default2-expr bound-body f-env)
          (eval bound-body
                (Extend id2 (eval arg2-expr env)
                        (Extend id1 (eval arg1-expr env)
                                f-env)))]
         [else (error 'eval "`call' expects a binary function, got: ~s"
                            fval)]))]
    [(Call fun-expr arg-expr)
     ;; Unary call...
     (let ([fval (eval fun-expr env)])
       (cases fval
         ;; With a unary function: same as the original `Call' case
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
         ;; And this is the core of the extension: the important point
         ;; here is that we evaluate `default2-expr' in an extension of
         ;; the closure's environment.
         [(Fun2V id1 id2 default2-expr bound-body f-env)
          (let* ([fenv1 (Extend id1 (eval arg-expr env) f-env)]
                 [fenv2 (Extend id2 (eval default2-expr fenv1) fenv1)])
            (eval bound-body fenv2))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]))

(: run : String -> Number)
;; evaluate a FLANG program contained in a string
(define (run str)
  (let ([result (eval (parse str) (EmptyEnv))])
    (cases result
      [(NumV n) n]
      [else (error 'run "evaluation returned a non-number: ~s"
                   result)])))

;; existing tests run fine, using unary functions and applications
(test (run "{call {fun {x} {+ x 1}} 4}")
      => 5)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
      => 4)
(test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
      => 7)
(test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
      => 124)
(test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
      => 7)
(test (run "{call {with {x 3}
                    {fun {y} {+ x y}}}
                  4}")
      => 7)
(test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
      => 124)

;; [*] tests for default expressions from the exam:

;; y defaults to 20, but it is not used
(test (run "{with {foo {fun {x y 20} {+ x y}}}
              {call foo 1 2}}")
      => 3)
;; y defaults to 20, and it is used
(test (run "{with {foo {fun {x y 20} {+ x y}}}
              {call foo 1}}")
      => 21)
;; y defaults to 2*10
(test (run "{with {foo {fun {x y {* 2 10}} {+ x y}}}
              {call foo 1}}")
      => 21)
;; y defaults to 9/0, but it's not used => no error
(test (run "{with {foo {fun {x y {/ 9 0}} {+ x y}}}
              {call foo 1 2}}")
      => 3)
;; test that the default is evaluated in the right scope
(test (run "{with {a 10}
              {with {foo {fun {x y {* a 2}} {+ x y}}}
                {with {a 3}
                  {call foo 1}}}}")
      => 21)
;; including a binding for the the first argument
(test (run "{with {foo {fun {x y {* x 2}} {+ x y}}}
              {call foo 1}}")
      => 3)
;; and make sure that it is the argument and no other x binding
(test (run "{with {x 10}
              {with {foo {fun {x y {* x 2}} {+ x y}}}
                {with {x 20}
                  {call foo 1}}}} ")
      => 3)
