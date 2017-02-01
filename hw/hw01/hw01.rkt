;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname hw01) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;;; Author - Matthew Springer
;;; Date - 1/16/17

;;; Academic integrity binding
(define I-will-behave #t)

;;; This function determines if three integers are "near" each other, meaning
;;; that the difference between the largest integer and the smallest integer
;;; is no larger than 2.
;;;
;;; Near?: Integer Integer Integer -> Boolean
(define (near? a b c)
  (if (< (- (max a (max b c)) (min a (min b c))) 3) #t #f))

;;; Test functions for near?
(equal? (near? -7 -8 -6) #t)
(equal? (near? -7 -9 -6) #f)
(equal? (near? 0 -1 1) #t)
(equal? (near? -2 1 0) #f)
(equal? (near? 7 8 6) #t)
(equal? (near? 7 5 8) #f)

;;; This function consumes a list of symbols and returns how many of those
;;; symbols are 'x'
;;;
;;; Count-xs: ListOfSymbol -> Integer
(define (count-xs l)
  (cond [(list? l)
         (cond [(empty? l) 0]
               [(equal? (first l) 'x) (+ 1 (count-xs (rest l)))]
               [else (+ 0 (count-xs (rest l)))])]
        [else 0]))

;;; Test functions for count-xs
(define list1 (list 'x 'y 's 'x 'h 'j 'x))
(define list2 (list 'x 'y 'a 'b 'x 'w 'k))
(define list3 (list 'r 'q 'v 'b 'e 'j 'b))
(equal? (count-xs list1) 3)
(equal? (count-xs list2) 2)
(equal? (count-xs list3) 0)
(equal? (count-xs null) 0)
(equal? (count-xs 6) 0)

;;; This function consumes a list of numbers and returns true if the list
;;; is sorted in ascending order (lowest -> highest), and false otherwise
;;;
;;; Ascending?: ListOfNumber -> Boolean
(define (ascending? l)
  (cond [(list? l)
         (cond [(empty? l) #t]
               [(empty? (rest l)) #t]
               [(<= (first l) (first (rest l))) (and #t (ascending? (rest l)))]
               [else #f])]
        [else #f]))

;;; Test functions for ascending?
(define list4 (list 1 2 3 4 5 6 7))
(define list5 (list -2 -1 0 1 2 3 4))
(define list6 (list 4 7 2 6 3 9 6))
(equal? (ascending? list4) #t)
(equal? (ascending? list5) #t)
(equal? (ascending? list6) #f)
(equal? (ascending? null) #t)
(equal? (ascending? 6) #f)

;;; This function consumes two lists (x and y) of equal length (n) and returns
;;; a list of tuples in the form (x1 y1), (x2 y2), ... , (xn yn)
;;; Zip2: ListOfX ListofY -> ListOf(ListOfTuple-XY)
(define (zip2 list1 list2)
  (cond [(empty? list1) null]
        [else (cons (list (first list1) (first list2))
                    (zip2 (rest list1) (rest list2)))]))

;;; Test functions for zip2
(define list7 (list (list 'x 1)
                    (list 'y 2)
                    (list 's 3)
                    (list 'x 4)
                    (list 'h 5)
                    (list 'j 6)
                    (list 'x 7)))
(define list8 (list (list 'x -2)
                    (list 'y -1)
                    (list 's 0)
                    (list 'x 1)
                    (list 'h 2)
                    (list 'j 3)
                    (list 'x 4)))
(define list9 (list (list 'x 4)
                    (list 'y 7)
                    (list 's 2)
                    (list 'x 6)
                    (list 'h 3)
                    (list 'j 9)
                    (list 'x 6)))
(equal? (zip2 list1 list4) list7)
(equal? (zip2 list1 list5) list8)
(equal? (zip2 list1 list6) list9)
(equal? (zip2 null null) null)

;;; Binds to the photo number with my picture in it.
(define my-picture 46)

;;; Binds to a list of other photo numbers with my picture in it (none)
(define my-other-pictures null)

;;; Binds to the numbers of minutes I spent on this assignment
(define minutes-spent 120)
               