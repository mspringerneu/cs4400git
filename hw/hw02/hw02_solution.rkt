#lang pl 02

;;; ------------------------------------------------------------------
;;; Question 1a

#|

  <SE>   ::= <ATOM> | <LIST>

  <LIST> ::= null
           | (cons <SE> <LIST>)
           | (list <SE> ...)
           | (append <LIST> ...)

  <ATOM> ::= <num> | '<sym>

|#


;;; ------------------------------------------------------------------
;;; Question 1b

#| BNF for the AE language, infix version:
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
;; parses s-expressions into AEs
(define (parse-sexpr sexpr)
  (match sexpr
    [(number: n) (Num n)]
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
    [(Div l r) (let ([r (eval r)])
                 ;; if we try to divide by zero, return `infinity'
                 (if (zero? r)
                   999
                   (/ (eval l) r)))]))

(: run : String -> Number)
;; evaluate an AE program contained in a string
(define (run str)
  (eval (parse str)))

;; tests (modified for infix, extended for complete coverage):
(test (run "3") => 3)
(test (run "{3 + 4}") => 7)
(test (run "{{3 - 4} + 7}") => 6)
(test (run "{8 * 9}") => 72)
(test (run "{8 / 2}") => 4)
(test (run "{8 / 0}") => 999)
(test (run "{8 / {5 - 5}}") => 999)
(test (run "{1}") =error> "bad syntax in")


;;; ------------------------------------------------------------------
;;; Question 1c

#|

1. The problem is that the syntax is ambiguous, unless we fix the
   semantics to specify some fixed order of evaluation for
   subexpressions.  Without this, the given expression:

      {* {+ {set 1} {set 2}} get}

   can evaluate to 6 (if we always evaluate left-to-right), it can be
   unspecified or an error (right-to-left), and it can even evaluate
   to 3 (evaluating the addition right-to-left and the multiplication
   left-to-right).

2. The following grammar specifies a MAE program as a non-empty
   sequence of subcomputations, all except the last must `set' the
   result into memory, and the first is not allowed to use `get'.  It
   has three non-terminals, <MAE> is for whole programs, <AE> is for
   simple arithmetic expressions, and <AE/g> is for arithmetic
   expressions that can contain `get'.

     <MAE>  ::= {seq <AE>}
              | {seq {set <AE>}
                     {set <AE/g>}
                     ...
                     <AE/g>}

     <AE/g> ::= <num>
              | { + <AE/g> <AE/g> }
              | { - <AE/g> <AE/g> }
              | { * <AE/g> <AE/g> }
              | { / <AE/g> <AE/g> }
              | get

     <AE>   ::= <num>
              | { + <AE> <AE> }
              | { - <AE> <AE> }
              | { * <AE> <AE> }
              | { / <AE> <AE> }

|#


;;; ------------------------------------------------------------------
;;; Question 2

(: square : Number -> Number)
;; Computes the square of a given number.
(define (square n)
  (* n n))

;; tests
(test (square  0) => 0)
(test (square  1) => 1)
(test (square -1) => 1)
(test (square 10) => 100)

(: sum-of-squares : (Listof Number) -> Number)
;; Computes the sum of squares of numbers in the input list.
(define (sum-of-squares numbers)
  (foldl + 0 (map square numbers)))

;; tests
(test (sum-of-squares '()) => 0)
(test (sum-of-squares '(1)) => 1)
(test (sum-of-squares '(0 0)) => 0)
(test (sum-of-squares '(1 2 4)) => 21)


;;; ------------------------------------------------------------------
;;; Question 3a

(define-type BINTREE
  [Leaf Number]
  [Node BINTREE BINTREE])

;; used for tests below
(define 1234-tree
  (Node (Node (Leaf 1) (Leaf 2))
        (Node (Leaf 3) (Leaf 4))))
(define 528-tree (Node (Leaf 5) (Node (Leaf 2) (Leaf 8))))


;;; ------------------------------------------------------------------
;;; Question 3b

(: tree-map : (Number -> Number) BINTREE -> BINTREE)
;; Maps the given function recursively over the given tree, returning
;; a tree of the results with the same shape.  Note the higher-order
;; type.
(define (tree-map f tree)
  (cases tree
    [(Leaf n) (Leaf (f n))]
    [(Node l r) (Node (tree-map f l) (tree-map f r))]))

;; tests
(test (tree-map add1 (Node (Leaf 1) (Node (Leaf 2) (Leaf 3))))
      => (Node (Leaf 2) (Node (Leaf 3) (Leaf 4))))
(test (tree-map add1 1234-tree)
      => (Node (Node (Leaf 2) (Leaf 3)) (Node (Leaf 4) (Leaf 5))))
(test (tree-map add1 (Leaf 1))
      => (Leaf 2))


;;; ------------------------------------------------------------------
;;; Question 3c

(: tree-fold : (All (A) (A A -> A) (Number -> A) BINTREE -> A))
;; `folds' a tree using a combiner function for combining the two
;; sub-results in case of a node, and a leaf function for a leaf case.
(define (tree-fold combiner leafer tree)
  (cases tree
    [(Leaf n)   (leafer n)]
    [(Node l r) (combiner (tree-fold combiner leafer l)
                          (tree-fold combiner leafer r))]))

;; Used for tests:
(: tree-flatten : BINTREE -> (Listof Number))
;; flattens a binary tree to a list of its values in left-to-right
;; order
(define (tree-flatten tree)
  (tree-fold (inst append Number) (inst list Number) tree))

;; tests:
(test (tree-flatten 1234-tree) => '(1 2 3 4))
(test (tree-flatten 528-tree) => '(5 2 8))
(test (tree-flatten (Node 1234-tree 528-tree)) => '(1 2 3 4 5 2 8))
(test (tree-flatten (Leaf 0)) => '(0))


;;; ------------------------------------------------------------------
;;; Question 3d

(: NodeRev : BINTREE BINTREE -> BINTREE)
;; Consumes two trees and makes a tree out of them in reverse order.
;; This is a helper for `tree-reverse' below.
(define (NodeRev r l)
  (Node l r))

(: tree-reverse : BINTREE -> BINTREE)
;; returns a "mirror-image" copy of the given BINTREE
(define (tree-reverse tree)
  (tree-fold NodeRev Leaf tree))

;; tests:
(test (equal? (reverse (tree-flatten 1234-tree))
              (tree-flatten (tree-reverse 1234-tree))))
(test (equal? (reverse (tree-flatten 528-tree))
              (tree-flatten (tree-reverse 528-tree))))
(test (equal? (reverse (tree-flatten (Leaf 0)))
              (tree-flatten (tree-reverse (Leaf 0)))))
