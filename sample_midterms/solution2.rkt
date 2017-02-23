#lang pl

;;----------------------------------------------------------------------
;; Question 1

;; The solutions for this requires `interleave` to return a closure over
;; internal state that should be represented as a mutable box:

(: interleave : (All (A) (-> A) (-> A) -> (-> A)))
;; Takes two nullary producer functions and returns a producer function
;; that interleaves them.
(define (interleave f g)
  (let ([b : (Boxof Boolean) (box #f)])
    (lambda ()
      (set-box! b (not (unbox b)))
      (if (unbox b) (f) (g)))))

;; tests from the text
(define (tick) 'tick)
(define (tock) 'tock)
(define clock (interleave tick tock))
(test (list (clock) (clock) (clock) (clock))
      => '(tick tock tick tock))
(define foo (interleave clock (interleave clock random)))
(list (foo) (foo) (foo) (foo) (foo) (foo) (foo) (foo))

;; Note that it is possible to make the box hold an integer and toggle
;; it between two numbers, or increment it and check whether it is even
;; -- but doing that implied some (small) penalty since it's an indirect
;; and inefficient way to implement what is essentially a boolean flag.
;; Also note that giving a type to the box as done above is needed,
;; since otherwise Typed Racket runs into inference problems -- but that
;; was not something that you're expected to know (and with TR's
;; inference it's more guessing than knowing anyway), so there was no
;; need for that.

;;----------------------------------------------------------------------
;; Question 2

#|

a. (blank (sqrt -1))
   Since Racket has imaginary numbers, the sqrt operation doesn't throw
   an error, and `blank` should be a simple function that takes an
   argument, ignore it, and return 660:
   => (define blank (lambda (x) 660))

   If Racket didn't have them and would throw an error, there would be
   no answer for this question, partial credit was given to people who
   said this (and justified it) -- and full credit was given to answers
   that said that it'll be an error but still described a solution in
   case it's not.

b. (blank 'blank)
   => (define blank (lambda (x) 660))

c. (second (map (lambda (x) (blank x blank)) '(1 2 3)))
   => (define blank (lambda (x y) 660))

d. ((and (blank) blank))
   This looked trickier than it is...  First, we need (blank) to be a
   non-#f value, then `and` would result in `blank` and that's applied
   again on no arguments, so the solution is very simple:
   => (define blank (lambda () 660))

e. (let ([blink blank]) (blank blink))
   This is essentially applying `blank` with itself as an argument, so:
   => (define (blank x) 660)

|#


;;----------------------------------------------------------------------
;; Question 3

#|

There are two ways that we can go through to arrive at a solution.  The
first, is to remove the `fun` form from the language, then make the
named-expression part of a `with` expression be a new non-terminal:

  <FLANGLITE> ::= <num>
                | { + <FLANGLITE> <FLANGLITE> }
                | { - <FLANGLITE> <FLANGLITE> }
                | { * <FLANGLITE> <FLANGLITE> }
                | { / <FLANGLITE> <FLANGLITE> }
                | { with { <id> <FLANGLITE/FUN> } <FLANGLITE> }
                | <id>
                | { call <FLANGLITE> <FLANGLITE> }

This non-terminal would be either a plain <FLANGLITE> expression, or a
`fun` expression, where in the latter case we should use <FLANGLITE/FUN>
for the recursion to allow nested `fun`s:

  <FLANGLITE/FUN> ::= <FLANGLITE>
                    | { fun { <id> } <FLANGLITE/FUN> }

The second way starts by splitting the BNF into a two non-terminals, one
without the `fun` form to be used as the main FLANGLITE entry point, and
one with it:

  <FLANGLITE> ::= ... same as the <FLANG> definition, without `fun` ...
  <FLANGLITE/FUN> ::= ... same, but *with* `fun` expressions ...

In the <FLANGLITE> case, expressions should be <FLANGLITE>, except for
the named expression which should be a <FLANGLITE/FUN>.  Now, since
<FLANGLITE/FUN> is almost identical to <FLANGLITE>, we can express it in
a short way as "same as <FLANGLITE>, with `fun`s added":

  <FLANGLITE/FUN> ::= <FLANGLITE>
                    | { fun { <id> } ??? }

And finally, the "???" expression should allow nested (curried) `fun`
expressions, which gets us to the same rule as above.

|#


;;----------------------------------------------------------------------
;; Question 4

;; As usual with syntax code, this closely follows the BNF.  We need two
;; scanning functions, one that mirrors FLANGLITE and one for
;; FLANGLITE/FUN.  Having a full specification for all FLANG cases in
;; each one would be as bad as repeating the most of the BNF cases in
;; the two non-terminals.  Instead, we can follow the way that the BNF
;; is organized, and have an `is-lite/fun?` predicate that will add one
;; check to `is-lite?`.  (This additional predicate could be made into a
;; local helper, of course.)

(: is-lite? : FLANG -> Boolean)
;; A predicate for FLANG expressions that are valid in the FLANGLITE
;; subset.
(define (is-lite? expr)
  (cases expr
    ;; simple leaf cases first
    [(Num n) #t]
    [(Id name) #t]
    [(Fun bound-id bound-body) #f]
    ;; simple recursive cases next
    [(Add  l r) (and (is-lite? l) (is-lite? r))]
    [(Sub  l r) (and (is-lite? l) (is-lite? r))]
    [(Mul  l r) (and (is-lite? l) (is-lite? r))]
    [(Div  l r) (and (is-lite? l) (is-lite? r))]
    [(Call l r) (and (is-lite? l) (is-lite? r))]
    ;; the main interesting case
    [(With id named body) (and (is-lite/fun? named) (is-lite? body))]))

(: is-lite/fun? : FLANG -> Boolean)
;; Helper for `is-lite?`, corresponding to the FLANGLITE/FUN part of the
;; language: allowing top-level function expressions.
(define (is-lite/fun? expr)
  (cases expr
    ;; allow immediate functions as well as curried functions
    [(Fun id body) (is-lite/fun? body)]
    ;; otherwise it's the same as `is-lite?`
    [else (is-lite? expr)]))

;; tests
(: lite? : String -> Boolean)
(define (lite? str) (is-lite? (parse str)))
(test (lite? "1"))
(test (lite? "{with {x {+ 1 2}} {- x 3}}"))
(test (not (lite? "{fun {x} x}")))
(test (lite? "{with {f {fun {x} {fun {y} {+ x y}}}} {call f f}}"))


;;----------------------------------------------------------------------
;; Question 5

;; The two cases that use `FunV` are `Fun` and `Call`, both changes are
;; extremely simple:

     [(Fun bound-id bound-body)
      (FunV bound-id bound-body (box env))]

     [(Call fun-expr arg-expr)
      (let ([fval (eval fun-expr env)])
        (cases fval
          [(FunV bound-id bound-body boxed-f-env)
           (eval bound-body
                 (Extend bound-id (eval arg-expr env) (unbox boxed-f-env)))]
          [else (error 'eval "`call' expects a function, got: ~s"
                             fval)]))]

;; For the main part, we need to evaluate the expression first, then use
;; `cases` on the result and either change the box that we find or throw
;; an error.  (This means that it's similar to the `Call` case which
;; could have been used as a template.)  Once that's done, we still need
;; some proper return value, and something like the bogus (NumV 42) will
;; do fine.
;;
;; Here's an evaluation fragment that does that:

     [(Close fun-expr)
      (let ([fval (eval fun-expr env)])
        (cases fval
          [(FunV bound-id bound-body boxed-f-env)
           (set-box! boxed-f-env env)
           (NumV 42)]
          [else (error 'eval "`close!' expects a function, got: ~s"
                             fval)]))]


;;----------------------------------------------------------------------
;; Question 6

;; Remember that we're using the two given definitions for `not1` and
;; `not2` as well as the definition that we've discussed in class for
;; the two booleans and for `null`:
;;
;; - (define #t (lambda (x y) x))
;;
;; - (define #f (lambda (x y) y))   ; actually, this is never used below
;;
;; - (define null (lambda (s) #t))

;; First, here is the short version, where we skip some steps where
;; - the only change is expanding implicitly curried functions or
;;   function calls,
;; - we replace a binding name with its definition, meaning that we just
;;   go from an application of a known function to what it reduces to,
;;   like going from (#f X Y) directly to Y.

;; Using `not1`:
((not1 null) A B C)
;; use definition of `not1`, and reduce its application
((null #f #t) A B C)
;; use definition of `null`, and reduce its application
((#t #t) A B C)
;; remove the inner parens since that's the same as implicit curried call
(#t #t A B C)
;; use definition of `#t`, and reduce its application on first two args
(#t B C)
;; again
B

;; Using `not2`:
((not2 null) A B C)
;; remove the inner parens since that's the same as implicit curried call
(not2 null A B C)
;; use definition of `not2`, and reduce its application on first 3 args
(null B A C)
;; use definition of `null`, and reduce its application
(#t A C)
;; use definition of `#t`, and reduce its application
A

;; The fully detailed sequences are obviously more verbose (note that
;; the above was sufficient for full credit):

;; Using `not1`:
((not1 null) A B C)
;; use definition of `not1`
(((lambda (a) (a #f #t)) null) A B C)
;; reduce its application
((null #f #t) A B C)
;; use definition of `null`
(((lambda (s) #t) #f #t) A B C)
;; expand curried application
((((lambda (s) #t) #f) #t) A B C)
;; reduce its application
((#t #t) A B C)
;; use definition of `#t`
(((lambda (x y) x) #t) A B C)
;; expand curried function
(((lambda (x) (lambda (y) x)) #t) A B C)
;; reduce application
((lambda (y) #t) A B C)
;; expand curried application
(((lambda (y) #t) A) B C)
;; reduce application
(#t B C)
;; use definition of `#t`
((lambda (x y) x) B C)
;; expand curried function
((lambda (x) (lambda (y) x)) B C)
;; expand curried application
(((lambda (x) (lambda (y) x)) B) C)
;; reduce application
((lambda (y) B) C)
;; reduce application
B

;; Using `not2`:
((not2 null) A B C)
;; use definition of `not2`
(((lambda (a x y) (a y x)) null) A B C)
;; expand curried function
(((lambda (a) (lambda (x y) (a y x))) null) A B C)
;; reduce application
((lambda (x y) (null y x)) A B C)
;; expand curried function
((lambda (x) (lambda (y) (null y x))) A B C)
;; expand curried application
(((lambda (x) (lambda (y) (null y x))) A) B C)
;; reduce application
((lambda (y) (null y A)) B C)
;; expand curried application
(((lambda (y) (null y A)) B) C)
;; reduce application
((null B A) C)
;; use definition of `null`
(((lambda (s) #t) B A) C)
;; expand curried application
((((lambda (s) #t) B) A) C)
;; reduce application
((#t A) C)
;; use definition of `#t`
(((lambda (x y) x) A) C)
;; expand curried function
(((lambda (x) (lambda (y) x)) A) C)
;; reduce application
((lambda (y) A) C)
;; reduce application
A


;;----------------------------------------------------------------------
;; Question 7

;; Q7a: 3
;; 1,2. Both of these are wrong, because #3 explains why there is *no*
;;    restriction...
;; 3. This is the correct answer.  Translating our discussion with
;;    Racket to FlangLite: all we need is to convert an arbitrary
;;    forbidden `fun` expression {fun {x} E} into
;;        {with {f {fun {x} E}} f}
;;    which has the same semantics and is not forbidden.
;; 4. This is obviously bogus: yes, syntax and semantics live at
;;    different levels, but restricting the syntax can very easily
;;    restrict the semantics.  As a trivial example: what if we forbid
;;    identifiers as expressions?  Or maybe if we kick out `call`
;;    expressions?

;; Q7b: 2,3,4
;; 1. It is often a bad idea to use mutation in code mostly because many
;;    people rush to mutation even when it isn't necessary.  But
;;    mutation in itself is certainly a useful feature to have!  -- And
;;    one that all languages do have.
;; 2,3. This is a major problem: it's the fact that we can change the
;;    *scope* of code that is particularly disastrous.  (See for example
;;    the deprecation of the Javascript `with` operator for a relevant
;;    example which is much tamer than out `close!`.)
;; 4. This is correct too: we will not be able to compile code with
;;    efficient varibale lookup (using indexes) since the meaning of
;;    names can change as a result of a `close!`.
;; 5. This is all true, but it's not a problem since you can already
;;    achieve the same effect of consuming memory in many other ways.

;; Q7c: 3
;; 1. Is silly.
;; 2. Is kind of hinting at a problem that we might still have: our
;;    ability to close over the same code in different places.  It's not
;;    nearly as bad, but it's still a problem.  (But it's not easy to
;;    see it, you need something like passing a raw function as an
;;    argument to a function and close over it there -- which means that
;;    you can pass different raw functions in a similar way too.)
;;    Very partial credit was given.
;; 3. This is the main problem: it doesn't get us recursion.  When we do
;;    a {close f} we want to call it, but we want this call to have this
;;    {close f} value as the binding of `f`, which is not the case.  We
;;    could try that with:
;;        {with {f {fun ...}}
;;          {with {f {close f}}
;;            ...}}
;;    but the function body would not see the closed-f value!
;; 4. Nonsense.  It's true that you can get a kind of a dynamic-scope-
;;    like effect, but that's in the nature of a `close` being separated
;;    from the function.  The "shadowing" part of the answer disclose it
;;    as completely bogus, but because of that confusion I gave (very)
;;    partial credit here.

;; Q7d: 4
;; 1. Wrong, for all the reasons discussed in later items.
;; 2. This points at another problem: if `return` is a function, then we
;;    can use it anywhere, unlike a real `return` which is a statement.
;;    So it's better, but still wrong since it misses the real problem.
;;    Also, it's a syntactical issue: we can claim that a Racket version
;;    of `return` *should* look like a regular function.
;; 3. This is close: it hints at the problem that when you use this kind
;;    of a `return` function, you're not getting out of the current
;;    function.  (Partial credit given)
;; 4. This is an explicit explanation of the problem that we're having,
;;    and therefore it is the correct answer.
;; 5. This is nonsense.
