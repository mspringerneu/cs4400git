#lang pl

;;----------------------------------------------------------------------
;; Question 1

(: curried-nth : (All (A) (Integer -> (Rec this (A -> (U A this))))))
;; Returns a curried function that takes in n arguments one by one, and
;; returns the last one.
(define (curried-nth n)
  (lambda (x)
    (if (zero? n) x (curried-nth (sub1 n)))))

#; ; an alternative form that many people used
(define (curried-nth n)
  (if (zero? n)
    (lambda (x) x)
    (lambda (x) (curried-nth (sub1 n)))))

(test ((curried-nth 0) 'x)               => 'x)
(test ((((curried-nth 2) 1) 2) 3)        => 3)
(test ((((curried-nth 1) add1) sub1) 10) => 9)


;;----------------------------------------------------------------------
;; Question 2

(require racket/list) ; this gives us the assumed `shuffle` function

(: random-bag : (All (A) (Listof A) -> (-> A)))
;;   same type but more confusing: (All (A) (Listof A) -> -> A)
;; Get a list of things and return a function that returns random
;; elements from the list without repetitions until all are returned and
;; then cycles again.
(define (random-bag things)
  (: state : (Boxof (Listof A)))
  (define state (box '()))
  (lambda ()
    (when (null? (unbox state))
      (set-box! state (shuffle things)))
    (let ([l (unbox state)])
      (set-box! state (rest l))
      (first l))))

;; (missing tests!)


;;----------------------------------------------------------------------
;; Question 3

;; 3a. (blank blank)
;;
;; (define blank (lambda (x) 660))

;; 3b. (Y blank)
;;
;; All we need to know here is that (Y blank) is an infinite sequence of
;; blank applications, so all we need to do is ignore the argument and
;; return 660.
;; (define blank (lambda (x) 660))

;; 3c. (blank blank (foldl blank 0 '(1 2 3)))
;;
;; (define blank (lambda (x y) 660))

;; 3d. (or blank (map blank blank))
;;
;; Might seem impossible since blank must be a 660 for the first part
;; and a function for the second, but the second doesn't get evaluated
;; if the first is true -- so the simple thing works.
;; (define blank 660)

;; 3e. (let ([blank +]) (blank (blank)))
;;
;; No solution to this one, since blank is not free here, so this
;; expression never uses the global value of blank.


;;----------------------------------------------------------------------
;; Question 4

#|

Note that this grammar is pretty simple, and straightforwardly follows
the specifications in the question.

<DECLARATION> ::= (: <identifier> : <TYPE>)

<TYPE>        ::= Number
                | String
                | Any
                | ( <TYPE> ... -> <TYPE> )
                | ( U <TYPE> ... )
|#


;;----------------------------------------------------------------------
;; Question 5

;; First, there is, in fact, such a magical "fresh name" function in
;; Racket, and we'll learn about it later in the class.  It's called
;; `gensym`, so we can do this to get it:
(require (only-in racket/base gensym))
(define fresh-name gensym)

;; Second, to try this code out, copy "flang.rkt" and add this test in
;; the end.  It will obviously fail now.
(test (run "{with {f {fun {y} {+ x y}}}
              {with {x 7}
                {call f 1}}}")
      =error> "free identifier")

#|
;; There are several ways to write a solution.  The conceptually simpler
;; but more verbose way that I hinted at in the exam will use the naive
;; `subst` as a utility function.  For example, we can use it to
;; implement a utility that makes all names fresh by scanning the
;; expression first and replacing all binding instances with fresh
;; names.  This isn't great since it's pretty inefficent -- not only
;; does it do two scans where the whole expression is reconstructed each
;; time, it also makes sure that there are no name collisions between
;; any two scopes and therefore the usual `subst` that we do in the
;; second pass is guaranteed to be slow as it never stops descending,
;; and finally, this freshening step could be done once instead of
;; repeating it in each and every subst.  Having said all that, here's
;; how this looks like:
;;
(: freshen-names : FLANG -> FLANG)
(define (freshen-names expr)
  (cases expr
    [(Num n)   expr]
    [(Id name) expr]
    [(Add  l r) (Add  (freshen-names l) (freshen-names r))]
    [(Sub  l r) (Sub  (freshen-names l) (freshen-names r))]
    [(Mul  l r) (Mul  (freshen-names l) (freshen-names r))]
    [(Div  l r) (Div  (freshen-names l) (freshen-names r))]
    [(Call l r) (Call (freshen-names l) (freshen-names r))]
    [(Fun bound-id bound-body)
     (let ([new-id (fresh-name)])
       (Fun new-id (freshen-names
                    (subst bound-body bound-id (Id new-id)))))]
    [(With bound-id named-expr bound-body)
     (let ([new-id (fresh-name)])
       (With new-id
             named-expr
             (freshen-names
              (subst bound-body bound-id (Id new-id)))))]))
(define (safe-subst expr from to)
  (subst (freshen-names expr) from to))
|#

#|
;; Another way is to make safe-subst be similar to subst -- copy the
;; whole function and rename it `safe-subst` (including recursive calls
;; that it does).  Note the two uses of `subst` (the original, naive
;; version) aas utilities here.  Note also the longer `With` case, since
;; we're doing the new name thing only when needed.
;;
(: safe-subst : FLANG Symbol FLANG -> FLANG)
(define (safe-subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add  l r) (Add  (safe-subst l from to) (safe-subst r from to))]
    [(Sub  l r) (Sub  (safe-subst l from to) (safe-subst r from to))]
    [(Mul  l r) (Mul  (safe-subst l from to) (safe-subst r from to))]
    [(Div  l r) (Div  (safe-subst l from to) (safe-subst r from to))]
    [(Call l r) (Call (safe-subst l from to) (safe-subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
       expr
       (let* ([new-id   (fresh-name)]
              [new-body (subst bound-body bound-id (Id new-id))])
         (Fun new-id (safe-subst new-body from to))))]
    [(With bound-id named-expr bound-body)
     (if (eq? bound-id from)
       (With bound-id
             (safe-subst named-expr from to)
             bound-body)
       (let* ([new-id   (fresh-name)]
              [new-body (subst bound-body bound-id (Id new-id))])
         (With new-id
               (safe-subst named-expr from to)
               (safe-subst new-body from to))))]))
|#

;; Finally, the thing that is slightly difficult to see is that in the
;; above code `safe-subst` could have called *itself* as a utility.
;; It's slightly difficult because it might look like this can get stuck
;; in an infinite loop -- but if you look at it carefully, you'll see
;; that it's always called over smaller expressions and therefore it's
;; fine even with the double recursive call.  We can further simplify
;; this by removing the `new-body` binding (since it's used only once),
;; and the result is a `subst` implementation that works fine.  (This
;; answer is actually very short since this version is identical to the
;; original one except for the two cases for `Fun` and `With`):

(define (subst expr from to)
  (cases expr
    [(Num n) expr]
    [(Add  l r) (Add  (subst l from to) (subst r from to))]
    [(Sub  l r) (Sub  (subst l from to) (subst r from to))]
    [(Mul  l r) (Mul  (subst l from to) (subst r from to))]
    [(Div  l r) (Div  (subst l from to) (subst r from to))]
    [(Call l r) (Call (subst l from to) (subst r from to))]
    [(Id name) (if (eq? name from) to expr)]
    [(Fun bound-id bound-body)
     (if (eq? bound-id from)
       expr
       (let ([new-id (fresh-name)])
         (Fun new-id (subst (subst bound-body bound-id (Id new-id))
                            from to))))]
    [(With bound-id named-expr bound-body)
     (if (eq? bound-id from)
       (With bound-id
             (subst named-expr from to)
             bound-body)
       (let ([new-id (fresh-name)])
         (With new-id
               (subst named-expr from to)
               (subst (subst bound-body bound-id (Id new-id))
                      from to))))
     ;; A variant of the above that keeps a single `With` construct, at
     ;; the cosdt of sometimes allocatiung a new symbolk when one is not
     ;; needed, and doing the decision inside twice (once for the
     ;; binding and once for the body).
     #;
     (let ([new-id (fresh-name)])
       (With (if (eq? bound-id from) bound-id new-id)
             (subst named-expr from to)
             (if (eq? bound-id from)
               bound-body
               (subst (subst bound-body bound-id (Id new-id))
                      from to))))]))


;;----------------------------------------------------------------------
;; Question 6

;; First, the change to `eval` is simple: instead of blindly evaluating
;; `Num`s into `NumV`s, we do a lookup:
;;
;;     [(Num n) (lookup n env)]
;;
;; For this to work, we should obviously change the first input type for
;; `lookup`, which is also a good hint that lookup is the second thing
;; to change (since its type was not mentioned earlier in the question).
;; The change itself is simple: in case of an environment we do the
;; same, but when we reach an `EmptyEnv` we throw an error only of the
;; name that we looked for was a symbol -- if it's a number, we just
;; return a `NumV`:
;;
;;     (: lookup : Identifier ENV -> VAL)
;;     (define (lookup name env)
;;       (cases env
;;         [(EmptyEnv)
;;          (if (number? name)
;;            (NumV name)
;;            (error 'lookup "no binding for ~s" name))]
;;         ...same...))
;;
;; Note that strictly speaking, the `eq?` in the other branch should
;; have been changed to `equal?`, but `eq?` works fine for "small"
;; integers, and I intentionally didn't complicate the question with it.
;;
;; A note about catching errors: some people tried to somehow catch the
;; lookup error in `eval` and return a `NumV` in that case.  As said in
;; the exam, this is wrong -- it is not needed and also can't be done.
;; In addition, if this could have worked, then there wouldn't be any
;; need for any other changes except for the change in type.

;; See full code at the bottom with these changes incorporated.


;;----------------------------------------------------------------------
;; Question 7

;; 1. (not1 #f)
;;    ((lambda (a) (a #f #t)) #f)              ; definition of not1
;;    (#f #f #t)                               ; reduce the call
;;    ((lambda (x y) y) #f #t)                 ; definition of #f
;;    #t                                       ; reduce the call

;; 2. (not1 #t)                                ; same, just with #t
;;    ((lambda (a) (a #f #t)) #t)
;;    (#t #f #t)
;;    ((lambda (x y) x) #f #t)
;;    #f

;; 3. (not2 #f)
;;    ((lambda (a x y) (a y x)) #f)            ; definition of not2
;;    ((lambda (a) (lambda (x y) (a y x))) #f) ; expand currying
;;    (lambda (x y) (#f y x))                  ; reduce the call
;;    (lambda (x y) ((lambda (x y) y) y x))    ; definition of #f
;;    (lambda (x y) x)                         ; reduce the inner call
;;    #t                                       ; same as defn. of #t

;; 4. (not2 #t)                                ; same with #t
;;    ((lambda (a x y) (a y x)) #t)
;;    ((lambda (a) (lambda (x y) (a y x))) #t)
;;    (lambda (x y) (#t y x))
;;    (lambda (x y) ((lambda (x y) x) y x))
;;    (lambda (x y) y)
;;    #f

;; Note that the above lists all steps -- if you do definition expansion
;; with reducing calls you get a much shorter sequences:

;; 1. (not1 #f) -> (#f #f #t) -> #t
;; 2. (not1 #t) -> (#t #f #t) -> #f
;; 3. (not2 #f) -> (lambda (x y) (#f y x)) -> (lambda (x y) x) -> #t
;; 4. (not2 #t) -> (lambda (x y) (#t y x)) -> (lambda (x y) y) -> #f

;;----------------------------------------------------------------------
;; Question 8

;; Q8a: 5
;; To clarify a bit more: in order to make the second and third tests
;; typecheck, we'll need to make sure that the intermediate results are
;; indeed functions.  That would make the code much more verbose...

;; Q8b: 2
;; Note that it is a bit tricky, but most certainly possible to test
;; random functionality!

;; Q8c: 1
;; Auto-currying means that this expression is read as
;;     (((lambda (x) (lambda (x) x)) 1) 2)
;;
;; - Note that this *could* also be seen as Schlac.  The problem with
;;   that answer is that Schlac has a whole pile of additional
;;   differences from Racket, so #1 is a better answer.  But there's
;;   also another, more technical point: in Schlac, all values are
;;   printed as #<procedure...> since they're all functions.  (Partial
;;   credit was given for this answer.)
;; - Also, some people chose "dynamically scoped" -- this is wrong,
;;   since a dynamically scoped language will usually still restrict the
;;   syntax of functions to have unique argument names.


;; Q8d: 3
;; Converting code into de-bruijn form would eliminate names but
;; maintain the semantics, and therefore it would be possible to compare
;; code regardless of the names used.


;;======================================================================
;; Full code of the modified Flang in question 6

(define-type Identifier = (U Symbol Number))

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [Call FLANG FLANG]
  [With Identifier FLANG FLANG]
  [Fun  Identifier FLANG])

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
       [(list 'with (list (number: name) named) body)
        (With name (parse-sexpr named) (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `with' syntax in ~s" sexpr)])]
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [(list 'fun (list (number: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg)
                       (Call (parse-sexpr fun) (parse-sexpr arg))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Identifier VAL ENV])

(define-type VAL
  [NumV Number]
  [FunV Identifier FLANG ENV])

(: lookup : Identifier ENV -> VAL)
;; lookup a symbol in an environment, return its value or throw an
;; error if it isn't bound
(define (lookup name env)
  (cases env
    [(EmptyEnv)
     (if (number? name)
       (NumV name)
       (error 'lookup "no binding for ~s" name))]
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
    [(Num n) (lookup n env)]
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
    [(Call fun-expr arg-expr)
     (let ([fval (eval fun-expr env)])
       (cases fval
         [(FunV bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env) f-env))]
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

;; tests
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

(test (run "{with {1 2} {+ 1 1}}") => 4)
(test (run "{with {2 1} {+ 1 1}}") => 2)
(test (run "{call {fun {1} {with {2 1} {+ 1 2}}}
                  {+ 1 2}}")
      => 6)
(test (run "{call {fun {1} {with {2 {+ 1 2}} {+ 1 2}}}
                  {+ 1 2}}")
      => 8)
