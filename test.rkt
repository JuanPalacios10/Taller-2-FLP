#lang racket/base

(require rackunit "interpretadorClase.rkt")

;; Test 1:
(define exp1 (scan&parse "cons(1 cons(2 empty))"))
(define expected-exp1
  '(1 2)
)
(check-equal? (evaluar-programa exp1) expected-exp1 )

;; Test 2: Expresión mínima usando empty
(define exp2 (scan&parse "empty"))
(define expected-exp2
  '()
)
(check-equal? (evaluar-programa exp2) expected-exp2 )

;; Test 3: Expresión que construye una lista con un valor y el empty
(define exp3 (scan&parse "cons (1 empty)"))
(define expected-exp3
  '(1)
)
(check-equal? (evaluar-programa exp3) expected-exp3)

;; Test 4: Expresión más compleja con cons anidado
(define exp5 (scan&parse "cons (2 cons(5 cons(6 cons(7 empty))))"))
(define expected-exp5
  '(2 5 6 7)
)
(check-equal? (evaluar-programa exp5) expected-exp5)

;; Test 5: Expresión de let con cons anidado
(define exp6 (scan&parse "cons(1 let l = cons(2 cons(3 empty)) in l)"))
(define expected-exp6
  '(1 2 3)
)
(check-equal? (evaluar-programa exp6) expected-exp6)

;; Test 6: Expresión de con lets anidados
(define exp8 (scan&parse "let l = cons(1 cons(2 empty)) in let l2 = cons(3 cons(4 empty)) in cons(l l2)"))
(define expected-exp8
  '((1 2) 3 4)
)
(check-equal? (evaluar-programa exp8) expected-exp8)

;;-------------------------------- Test de length --------------------------------
(define length1 (scan&parse "length(cons(1 cons(2 empty)))"))
(check-equal?  (evaluar-programa length1)  (length expected-exp1))


(define length2 (scan&parse "length(empty)"))
(check-equal?  (evaluar-programa length2) (length expected-exp2))

(define length3 (scan&parse "length(cons (1 empty))"))
(check-equal?  (evaluar-programa length3) (length expected-exp3))

(define length4 (scan&parse "length(cons (2 cons(5 cons(6 cons(7 empty)))))"))
(check-equal?  (evaluar-programa length4) (length expected-exp5))

(define length5 (scan&parse "length(cons(1 let l = cons(2 cons(3 empty)) in l))"))
(check-equal?  (evaluar-programa length5) (length expected-exp6))

(define length6 (scan&parse "length(let l = cons(1 cons(2 empty)) in let l2 = cons(3 cons(4 empty)) in cons(l l2))"))
(check-equal?  (evaluar-programa length6) (length expected-exp8))

;;-------------------------------- Test de length --------------------------------

; --------------------------------- Test de first ---------------------------------

(define first1 (scan&parse "first(cons(1 cons(2 empty)))"))
(check-equal?  (evaluar-programa first1) (car expected-exp1))

(define first3 (scan&parse "first(cons (1 empty))"))
(check-equal?  (evaluar-programa first3) (car expected-exp3))

(define first4 (scan&parse "first(cons (2 cons(5 cons(6 cons(7 empty)))))"))
(check-equal?  (evaluar-programa first4) (car expected-exp5))

(define first5 (scan&parse "first(cons(1 let l = cons(2 cons(3 empty)) in l))"))
(check-equal?  (evaluar-programa first5) (car expected-exp6))

(define first6 (scan&parse "first(let l = cons(1 cons(2 empty)) in let l2 = cons(3 cons(4 empty)) in cons(l l2))"))
(check-equal?  (evaluar-programa first6) (car expected-exp8))

; --------------------------------- Test de first ---------------------------------

; --------------------------------- Test de rest ---------------------------------
(define rest1 (scan&parse "rest(cons(1 cons(2 empty)))"))
(check-equal?  (evaluar-programa rest1) (cdr expected-exp1))


(define rest4 (scan&parse "rest(cons (2 cons(5 cons(6 cons(7 empty)))))"))
(check-equal?  (evaluar-programa rest4) (cdr expected-exp5))

(define rest5 (scan&parse "rest(cons(1 let l = cons(2 cons(3 empty)) in l))"))
(check-equal?  (evaluar-programa rest5) (cdr expected-exp6))

(define rest6 (scan&parse "rest(let l = cons(1 cons(2 empty)) in let l2 = cons(3 cons(4 empty)) in cons(l l2))"))
(check-equal?  (evaluar-programa rest6) (cdr expected-exp8))

; --------------------------------- Test de rest ---------------------------------

; --------------------------------- Test de nth ---------------------------------
(define nth1 (scan&parse "nth(0 cons(1 cons(2 empty)))"))
(check-equal?  (evaluar-programa nth1) (list-ref expected-exp1 0))

(define nth2 (scan&parse "nth(1 cons(1 cons(2 empty)))"))
(check-equal?  (evaluar-programa nth2) (list-ref expected-exp1 1))

(define nth3 (scan&parse "nth(0 cons (1 empty))"))
(check-equal?  (evaluar-programa nth3) (list-ref expected-exp3 0))

(define nth4 (scan&parse "nth(1 cons (2 cons(5 cons(6 cons(7 empty)))))"))
(check-equal?  (evaluar-programa nth4) (list-ref expected-exp5 1))

#|
;Test 1: 
(define exp1 
  (scan&parse
    "let
      x = {a:1,b:2,c:3,d:4,e:5}
      in
        display-dict(x)
    "
  )
)
(define expected-exp1
  "{a:1, b:2, c:3, d:4, e:5}"
)

(check-equal?  (eval-program exp1) expected-exp1)

;Test 2: 
(define exp2 
  (scan&parse
    "let
      x = {a:1,b:2,c:3,d:4,e:5,a:6,b:7,c:8,d:9,e:10}
      in
        display-dict(x)
    "
  )
)

(define expected-exp2
  "{a:6, b:7, c:8, d:9, e:10}"
)

(check-equal?  (eval-program exp2) expected-exp2)

; Test 3:

(define exp3 
  (scan&parse
    "let
      x = {a:1,a:2,a:3,a:4,a:5,b:6,c:7,z:8,w:9,w:10,l:11}
      in
        display-dict(x)
    "
  )
)


(define expected-exp3
  "{a:5, b:6, c:7, l:11, w:10, z:8}"
)

(check-equal?  (eval-program exp3) expected-exp3)
|#