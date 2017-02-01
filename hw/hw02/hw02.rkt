#lang pl 02

#|
    Project:      cs4400 HW #2
    Author:       Matthew Springer
    Date Created: Jan 19, 2017
 |#

;; Question 1a:

#|  BNF for the SE language:
    <SE> ::= <num> | '<sym>
           | {cons <SE> <list>}
           | {list <SE> ...}
           | {append <list> ...}
           | null

    <LIST> ::= <elem>
             | {cons <elem> <list>}
             | {list 
             |
 |#

;; Question 1b:

#| BNF for the AE language:
   <AE> ::= <num>
          | { <AE> + <AE> }
          | { <AE> - <AE> }
          | { <AE> * <AE> }
          | { <AE> / <AE> }
|#

;; AE abstract syntax trees
(define-type AE
  [Num Number]
  [Add AE AE]
  [Sub AE AE]
  [Mul AE AE]
  [Div AE AE])

(: parse-sexpr : Sexpr -> AE)
;; to convert s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
    ;; code edited to support infixing
    [(list lhs '+ rhs) (Add (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '- rhs) (Sub (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '* rhs) (Mul (parse-sexpr lhs) (parse-sexpr rhs))]
    [(list lhs '/ rhs) (Div (parse-sexpr lhs) (parse-sexpr rhs))]
    [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))

(: parse : String -> AE)
;; parses a string containing an AE expression to an AE AST
(define (parse str)
  (parse-sexpr (string->sexpr str)))

(: eval : AE -> Number)
;; consumes an AE and computes the corresponding number
(define (eval expr)
  (cases expr
    [(Num n)   n]
    [(Add l r) (+ (eval l) (eval r))]
    [(Sub l r) (- (eval l) (eval r))]
    [(Mul l r) (* (eval l) (eval r))]
    [(Div l r) (if (= (eval r) 0) 999 (/ (eval l) (eval r)))]))

(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests
(test (run "3") => 3)
(test (run "{3 + 4}") => 7)
(test (run "{3 - 4}") => -1)
(test (run "{3 * 4}") => 12)
(test (run "{5 / 0}") => 999)
(test (run "{10 / 2}") => 5)
(test (run "{{3 - 4} + 7}") => 6)
(test (run "{+ 7 11}") =error> "bad syntax in (+ 7 11)")

;; Question 1c:

   ;; 1c.1
      #|
          
       |#



;; Question 2:



;; Question 3a:



;; Question 3b:



;; Question 3c:



;; Question 3d:



;; Question 4:

;; as of 1/19/17 3:45pm
(define minutes-spent 30)