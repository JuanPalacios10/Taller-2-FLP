#lang racket/base

(require rackunit "interpretadorClase.rkt")

(define exp1 (scan&parse "cons(1 cons(2 empty))"))

;; 1. Expresión mínima usando empty
(define exp2 (scan&parse "empty"))

;; 2. Expresión que construye una lista con un valor y el empty
(define exp3 (scan&parse "cons (1 empty)"))

;; 4. Expresión con cons y list-exp
(define exp4 (scan&parse "cons( 2 cons(3 cons (4 empty)))"))

;; 5. Expresión más compleja con cons anidado
(define exp5 (scan&parse "cons (2 cons(5 cons(6 cons(7 empty))))"))

;; 6. Expresión con una lista vacía dentro
(define exp6 (scan&parse "cons ( cons (7 empty) cons ( 1 empty))"))

;; 7. Expresión usando un list-exp más grande
(define exp7 (scan&parse "cons ( cons (2 empty) cons ( 5 empty))"))


;; 8. Expresión con múltiples niveles de cons anidados
(define exp8 (scan&parse "cons (6 8)"))

#|
;; 9. Expresión con listas vacías combinadas
(define exp10 (scan&parse "cons empty (cons empty empty)"))

;; 10. Expresión que combina valores, listas y empty
(define exp11 (scan&parse "cons (cons (list 15 16) empty) (cons (list 17 18 19) empty)"))
|#
(check-equal? (evaluar-programa exp1) '(1 2))

(check-equal? (evaluar-programa exp2) '())

(check-equal? (evaluar-programa exp3) '(1))

(check-equal? (evaluar-programa exp4) '(2 3 4))

(check-equal? (evaluar-programa exp5) '(2 5 6 7))

(check-equal? (evaluar-programa exp6) '((7) 1))

(check-equal? (evaluar-programa exp7) '((2) 5))

(check-equal? (evaluar-programa exp8) '(6 8))
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