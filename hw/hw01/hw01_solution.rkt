;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw01_solution) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;; -------------------------------------------------------------------
;; 2

;; near? : Integer Integer Integer -> Boolean
;; Determines if all three inputs are in an interval of at most 2.
(define (near? a b c)
  (<= (- (max a b c) (min a b c)) 2))

;; tests:
(near? 0 0 0)
(near? 1 2 3)
(not (near? 0 2 3))
(near? 2 1 3)
(near? 3 2 1)
(near? 3 3 3)
(near? -1 -3 -2)
(near? 1 -1 1)
(not (near? 2 -1 1))


;; -------------------------------------------------------------------
;; 3

;; count-xs : (Listof Symbol) -> Integer
;; Counts the number of times the symbol `x' appears in the input list.
(define (count-xs list)
  (cond [(null? list)        0]
        [(eq? (car list) 'x) (+ 1 (count-xs (rest list)))]
        [else                (count-xs (rest list))]))

;; tests
(equal? 0 (count-xs '()))
(equal? 1 (count-xs '(x)))
(equal? 3 (count-xs '(x x x)))
(equal? 0 (count-xs '(y y y)))
(equal? 3 (count-xs '(y x x y x)))
(equal? 3 (count-xs '(x x y y y x)))

;; Alternative version: this one avoids repeating the recursive call
(define (count-xs list)
  (if (null? list)
    0
    (let ([n (count-xs (rest list))])
      (if (eq? (car list) 'x) (+ 1 n) n))))

;; Another alternative: this one is also tail recursive
(define (helper list n)
  (if (null? list)
    n
    (helper (rest list) (if (eq? (car list) 'x) (+ n 1) n))))
(define (count-xs list)
  (helper list 0))


;; -------------------------------------------------------------------
;; 4

;; ascending? : (Listof Number) -> Boolean
;; Determines if the input list of numbers is ascending.  This code is
;; not great.
(define (ascending? list)
  (cond [(null? list)                    #t]
        [(null? (rest list))             #t]
        [(<= (first list) (second list)) (ascending? (rest list))]
        [else                            #f]))

;; This version is much better: it's more succinct, using `or' and
;; `and'.
(define (ascending? list)
  (or (null? list)
      (null? (rest list))
      (and (<= (first list) (second list))
           (ascending? (rest list)))))

;; tests
(ascending? '())
(ascending? '(0))
(ascending? '(0 0))
(ascending? '(1 2))
(ascending? '(0 0 0 0))
(ascending? '(0 1 2 3))
(ascending? '(0 1 1 1))
(not (ascending? '(1 0)))
(not (ascending? '(-1 -2)))


;; -------------------------------------------------------------------
;; 5

;; zip2: (Listof A) (Listof B) -> (Listof (List A B))
;; Consumes two lists of equal length, returns a list of two-item
;; lists from both.
(define (zip2 l1 l2)
  (if (null? l1) ; assume equal length: no need to check l2!
    null
    (cons (list (car l1) (car l2)) (zip2 (cdr l1) (cdr l2)))))
;; tests:
(equal? (list (list 1 'a) (list 2 'b) (list 3 'c))
        (zip2 (list 1 2 3) (list 'a 'b 'c)))
(equal? null (zip2 null null))
