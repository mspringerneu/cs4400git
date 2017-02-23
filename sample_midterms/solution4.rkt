#lang pl

;;----------------------------------------------------------------------
;; Question 1

(: encode : (All (A) (Listof A) -> (Listof (List Integer A))))
;; take in a list of things and return a run-length encoded list
(define (encode things)
  (: helper : (All (A) A (Listof A) Integer -> (Listof (List Integer A))))
  (define (helper x things n)
    (cond [(null? things)
           (list (list n x))]
          [(equal? (first things) x)
           (helper x (rest things) (add1 n))]
          [else (cons (list n x)
                      (helper (first things) (rest things) 1))]))
  (helper (first things) (rest things) 1))

;; tests from the exam
(test (encode '(1 1 1 0 1 0 0 0 1))
      => '((3 1) (1 0) (1 1) (3 0) (1 1)))
(test (encode '(1 1 1 1 2 2 3 4 4 4))
      => '((4 1) (2 2) (1 3) (3 4)))
(test (encode '(A B B C))
      => '((1 A) (2 B) (1 C)))


;;----------------------------------------------------------------------
;; Question 2

;; This question was basically free points, all you needed to do was to
;; take the `currify` function we've seen in class and flip its
;; arguments, and remember to do the same with the types too.

(: r-curry : (All (A B C) (A B -> C) -> (B -> (A -> C))))
;; convert a double-argument function to a revered-curried one
(define (r-curry f)
  (lambda (x) (lambda (y) (f y x))))

;; test from the exam
(define str1 (r-curry string-append))
(test ((str1 "one") "two") => "twoone")


;;----------------------------------------------------------------------
;; Question 3

;; 3a. (map blank '())
;;
;; Since the list input to `map' is empty, it will not even call `blank'
;; and just return an empty list, so there is no solution here.

;; 3b. (first (map blank (blank blank)))
;;
;; Starting from the inside, `blank' is applied on itself and the result
;; is supposed to be a list, so: (define (blank _) (list _)); next, the
;; first element in this list is fed to `blank' (by `map') and is the
;; ultimate result because of the `first' so we also need `blank` to be
;; a single-argument function that returns 660.  This could mean that
;; there is no solution to this, but we can test the input argument and
;; return either a one-item list (if the input is `blank' itself), or
;; the actual value (otherwise).
;; (define (blank x) (if (equal? x blank) (list 0) 660))


;; 3c. ((compose blank blank) 0)
;;
;; This one is easy:
;;   (define (blank x) 660)

;; 3d. ((or blank (blank 3)))
;;
;; If `blank' is any value which is not #f it will be considered truthy,
;; and therefore it will be returned, the result needs to be a nullary
;; function (a thunk), which is is not #f and therefore this is
;; possible:
;;   (define blank (lambda () 660))

;; 3e. (let* ([blonk (blank)]
;;            [blink (blonk)]
;;            [blank (blink)])
;;       (blink 'bling))
;;
;; The thing to note here is that we're using the second binding, which
;; is bound to the result of (blonk) which is in turn ((blank)), so:
;;    (define blank (lambda () (lambda () (lambda (x) 660))))
;; *However* the same function is called in the third binding, so it
;; also has to be a function of no arguments, and therefore there is no
;; solution for this.  [Note that it full Racket, we could write a
;; function with a default argument:
;;   (define blank (lambda () (lambda () (lambda ([x 0]) 660))))
;; but you shouldn't know about that...]


;;----------------------------------------------------------------------
;; Question 4

#|

The first extension is easy, take the original BNF:

    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <FLANG> }
              | { call <FLANG> <FLANG> }

and we add just one simple rule:

              | { if0 <FLANG> <FLANG> <FLANG> }

The second extension is the more interesting one, and we can solve that,
as hinted, with a new non-terminal, <TAIL>, which includes the new
`return` expression.  First, in the usual FLANG language there is just
one tail position -- the body of a function:

    <FLANG> ::= <num>
              | { + <FLANG> <FLANG> }
              | { - <FLANG> <FLANG> }
              | { * <FLANG> <FLANG> }
              | { / <FLANG> <FLANG> }
              | { with { <id> <FLANG> } <FLANG> }
              | <id>
              | { fun { <id> } <TAIL> }
              | { call <FLANG> <FLANG> }
              | { if0 <FLANG> <FLANG> <FLANG> }

Now, for the <TAIL> -- we have four rules: (1) allow a plain <FLANG>
since `return`s are optional, (2) add the new `return`, and (3,4)
specify how "tailness" is preserved in a `with` or `if0`:

    <TAIL>  ::= <FLANG>
              | { return <FLANG> }
              | { with { <id> <FLANG> } <TAIL> }
              | { if0 <FLANG> <TAIL> <TAIL> }

Note that if we make the `return` expression be <TAIL>, we'd be able to
write things like {fun {x} {return {return x}}}:

    <TAIL>  ::= <FLANG>
              | { return <TAIL> }
              | { with { <id> <FLANG> } <TAIL> }
              | { if0 <FLANG> <TAIL> <TAIL> }

This is not common, but it's the part that wasn't specified completely
in the question.

|#


;;----------------------------------------------------------------------
;; Question 5

;; (The full code for the solution of Q6 at the bottom of this file also
;; implements enough to test this code, which is included there too.)

;; the first part is the main code:
(: bad-returns? : FLANG Boolean -> Boolean)
(define (bad-returns? expr tail?)
  (cases expr
    [(Num n) #f]
    [(Id name) #f]
    [(Add l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
    [(Sub l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
    [(Mul l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
    [(Div l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
    [(Call l r) (or (bad-returns? l #f) (bad-returns? r #f))]
    [(With bound-name named-expr bound-body)
     (or (bad-returns? named-expr #f)
         (bad-returns? bound-body tail?))]
    [(Fun bound-name bound-body)
     (bad-returns? bound-body #t)]
    ;; The extensions
    [(If0 c t e) (or (bad-returns? c #f)
                     (bad-returns? t tail?)
                     (bad-returns? e tail?))]
    ;; The unspecified case: use #t to allow nested `return`s
    [(Ret v) (or (not tail?) (bad-returns? v #f))]))

;; the second part is the toplevel function:
(: has-bad-returns? : FLANG -> Boolean)
;; detect whether there are any bad (non-tail-position) uses of `return`
(define (has-bad-returns? expr)
  (bad-returns? expr #f))


;;----------------------------------------------------------------------
;; Question 6

;; See the bottom of this file for the complete implementation, the four
;; bits of code that you needed to write are:

;; Q6a
;; [(list 'if0 val then else)
;;  (If0 (parse-sexpr val) (parse-sexpr then) (parse-sexpr else))]

;; Q6b
;; [(If0 val-expr then-expr else-expr)
;;  (eval (if (equal? (eval val-expr env) (NumV 0)) then-expr else-expr)
;;        env)]

;; Q6c
;; [(With bound-id (Fun f-id f-body) bound-body)
;;  (eval bound-body
;;        (Extend bound-id (NamedFunV bound-id f-id f-body env) env))]

;; Q6d
;; [(NamedFunV fun-id bound-id bound-body f-env)
;;  (eval bound-body
;;        (Extend bound-id (eval arg-expr env)
;;                (Extend fun-id fval f-env)))]


;;----------------------------------------------------------------------
;; Question 7

;;   (void? the-pair)
;; expand the definition of `void?' and apply it
;;   (zero? (the-pair 1 1))
;; expand the definition of `the-pair'
;;   (zero? ((cons A B) 1 1))
;; expand the definition of `cons' and apply it
;;   (zero? ((lambda (s) (s A B)) 1 1))
;; reduce applying the selector function
;;   (zero? ((1 A B) 1))
;; reduce applying the first 1 (which is also identity)
;;   (zero? ((A B) 1))
;; remove the implicit parentheses for the (A B) application
;;   (zero? (A B 1))
;; expand the definition of `zero?' from class
;;   ((lambda (n) (n (lambda (x) #f) #t)) (A B 1))
;; reduce its application on the argument of (A B 1)
;;   ((A B 1) (lambda (x) #f) #t)
;; again, remove parens that are implicit anyway
;;   (A B 1 (lambda (x) #f) #t)


;;----------------------------------------------------------------------
;; Question 8

;; Q8a: 3
;; The resulting language is indeed different in the sense demonstrated
;; by #3, which explains why #1 is wrong; #2 claims that some code works
;; when it doesn't, and 4 and 5 are nonsense answers.

;; Q8b: 4

;; Q8c: 3
;; You can actually try running these things and see the different
;; results, and you can trace how the execution goes to see the
;; different environment that you end up using for the named
;; expressions.


;;======================================================================
;; Full code of the modified Flang in question 6, and the AST code in
;; question 5

(define-type FLANG
  [Num  Number]
  [Add  FLANG FLANG]
  [Sub  FLANG FLANG]
  [Mul  FLANG FLANG]
  [Div  FLANG FLANG]
  [Id   Symbol]
  [With Symbol FLANG FLANG]
  [Fun  Symbol FLANG]
  [Call FLANG FLANG]
  [If0  FLANG FLANG FLANG]
  [Ret  FLANG]) ; added for Q5

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
    [(cons 'fun more)
     (match sexpr
       [(list 'fun (list (symbol: name)) body)
        (Fun name (parse-sexpr body))]
       [else (error 'parse-sexpr "bad `fun' syntax in ~s" sexpr)])]
    [(list '+ lhs rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '- lhs rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '* lhs rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list '/ lhs rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list 'call fun arg)
                       (Call (parse-sexpr fun) (parse-sexpr arg))]
    ;; Question 6a
    [(list 'if0 val then else)
     (If0 (parse-sexpr val) (parse-sexpr then) (parse-sexpr else))]
    ;; Added to test the code from question 5
    [(list 'return val) (Ret (parse-sexpr val))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> FLANG)
;; parses a string containing a FLANG expression to a FLANG AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: has-bad-returns? : FLANG -> Boolean)
;; detect whether there are any bad (non-tail-position) uses of `return`
(define (has-bad-returns? expr)
  (: bad-returns? : FLANG Boolean -> Boolean)
  (define (bad-returns? expr tail?)
    (cases expr
      [(Num n) #f]
      [(Id name) #f]
      [(Add l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
      [(Sub l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
      [(Mul l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
      [(Div l r)  (or (bad-returns? l #f) (bad-returns? r #f))]
      [(Call l r) (or (bad-returns? l #f) (bad-returns? r #f))]
      [(With bound-name named-expr bound-body)
       (or (bad-returns? named-expr #f)
           (bad-returns? bound-body tail?))]
      [(Fun bound-name bound-body)
       (bad-returns? bound-body #t)]
      ;; The extensions
      [(If0 c t e) (or (bad-returns? c #f)
                       (bad-returns? t tail?)
                       (bad-returns? e tail?))]
      ;; The unspecified case: use #t to allow nested `return's
      [(Ret v) (or (not tail?) (bad-returns? v #f))]))
  (bad-returns? expr #f))

;; testing utilities
(: invalid? : String -> Boolean)
(define (invalid? str) (has-bad-returns? (parse str)))
(: valid? : String -> Boolean)
(define (valid? str) (not (invalid? str)))
;; tests from question 4
(test (invalid? "{return 3}"))
(test (valid?   "{fun {x} {return {+ x 1}}}"))
(test (valid?   "{fun {x} {return {call foo {* 2 x}}}}"))
(test (valid?   "{fun {x} {call foo {* 2 x}}}"))
(test (invalid? "{fun {x} {call foo {return {* 2 x}}}}"))
(test (valid?   "{fun {x} {with {y 2} {return {* x y}}}}"))
(test (invalid? "{fun {x} {with {y {return 2}} {return {* x y}}}}"))
(test (invalid? "{fun {x} {+ 10 {with {y 2} {return {* x y}}}}}"))
(test (invalid? "{fun {x} {+ 10 {return {if0 x y z}}}}"))
(test (valid?   "{fun {x} {return {if0 x y z}}}"))
(test (valid?   "{fun {x} {if0 x {return y} {return z}}}"))
(test (invalid? "{fun {x} {if0 {return x} {return y} {return z}}}"))
(test (valid?   "{fun {x} {return {if0 x y z}}}"))
;; the following test needs to be flipped to `valid?' if we do the
;; change that is mentioned at the end of the function
(test (invalid? "{fun {x} {return {if0 x y {return z}}}}"))

;; Types for environments, values, and a lookup function

(define-type ENV
  [EmptyEnv]
  [Extend Symbol VAL ENV])

(define-type VAL
  [NumV      Number]
  [FunV      Symbol FLANG ENV]
  [NamedFunV Symbol Symbol FLANG ENV])

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
    ;; Question 6c
    [(With bound-id (Fun f-id f-body) bound-body)
     (eval bound-body
           (Extend bound-id (NamedFunV bound-id f-id f-body env) env))]
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
         ;; Question 6d
         [(NamedFunV fun-id bound-id bound-body f-env)
          (eval bound-body
                (Extend bound-id (eval arg-expr env)
                        (Extend fun-id fval f-env)))]
         [else (error 'eval "`call' expects a function, got: ~s"
                            fval)]))]
    ;; Question 6b
    [(If0 val-expr then-expr else-expr)
     (eval (if (equal? (eval val-expr env) (NumV 0))
             then-expr
             else-expr)
           env)]))

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

;; Tests from the exam (demonstrating the binding order too)
(test (run "{with {sum {fun {n} {if0 n 0 {+ n {call sum {- n 1}}}}}}
              {call sum 10}}")
      => 55)
(test (run "{with {foo {fun {foo} {+ foo 1}}} {call foo 10}}")
      => 11)
(test (run "{with {foo {fun {foo} {call foo 1}}} {call foo 10}}")
      =error> "expects a function")
(test
 (run "{with {even_and_odd
              {fun {n}
                {if0 n
                  {fun {n} {if0 n 0 {call {call even_and_odd 1} {- n 1}}}}
                  {fun {n} {if0 n 1 {call {call even_and_odd 0} {- n 1}}}}}}}
         {with {is_even {call even_and_odd 0}}
           {with {sum_evens
                  {fun {n}
                    {if0 n 0 {+ {call sum_evens {- n 1}}
                                {if0 {call is_even n} n 0}}}}}
             {call sum_evens 10}}}}")
 => 30)
