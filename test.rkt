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

; (check-equal? (evaluar-programa exp8) '(6 8))

;; Tests de los condicionales

;; 1. Expresión con un condicional verdadero
(define expc1 (scan&parse "cond +(1,1) ==> 2 else ==> 0 end"))

(check-equal? (evaluar-programa expc1) 2)

;; 2. Expresión con un condicional falso
(define expc2 (scan&parse "cond 0 ==> 1 else ==> 9 end"))

(check-equal? (evaluar-programa expc2) 9)

;; 3. Expresión con un condicional vacio
(define expc3 (scan&parse "cond else ==> 0 end"))

(check-equal? (evaluar-programa expc3) 0)

;; 4. Expresión con multiples condiciones
(define expc4 (scan&parse "cond +(1,1) ==> 2 -(1,1) ==> 0 else ==> 0 end"))
(define expc5 (scan&parse "cond -(1,1) ==> 2 -(3,3) ==> 0 +(1,2) ==> 3  else ==> 10 end"))

(check-equal? (evaluar-programa expc4) 2)
(check-equal? (evaluar-programa expc5) 3)

;; 5. Expresión con una condicion booleana
(define expc6 (scan&parse "cond true ==> 1 else ==> 0 end"))
(define expc7 (scan&parse "cond false ==> 1 else ==> 0 end"))

(check-equal? (evaluar-programa expc6) 1)
(check-equal? (evaluar-programa expc7) 0)

;; 6. Expresión con una condicion que no sea aritmetica
(define expc8 (scan&parse "cond >(1, 2) ==> 1 <(1, 2) ==> 5 else ==> 9 end"))
(define expc9 (scan&parse "cond ==(5, 4) ==> 1 let l = cons(1 cons(2 empty)) in ==(length(l), 2) ==> 3 else ==> 9 end"))

(check-equal? (evaluar-programa expc8) 5)
(check-equal? (evaluar-programa expc9) 3)

;; 7. Expresión con condicionales anidados
(define expc10 (scan&parse "cond >(3, 1) ==> cond -(3, 3) ==> true ==(1, 2) ==> 3 else ==> 0 end else ==> 9 end"))
(define expc11 (scan&parse "cond >(5, 10) ==> true ==(1, 2) ==> 3 else ==> cond let x = 5 in -(x, 3) ==> true else ==> 0 end end"))

(check-equal? (evaluar-programa expc10) 0)
(check-equal? (evaluar-programa expc11) #t)

;; 8. Expresión con condicionales de varios casos
(define expc12 (scan&parse "cond let x = cond >(5, 6) ==> 1 <(5, 6) ==> 2 else ==> 3 end l = cons(1 cons(2 cons(3 empty))) in nth(l, x) ==> cond let y = cons(1 cons(2 empty)) in ==(first(y), 1) ==> 1 else ==> 0 end else ==> 9 end"))
(define expc13 (scan&parse "cond let x = 5 y = 1 in cond <(x, y) ==> let z = cond ==(-(x, 3), y) ==> 3 ==(x, y) ==> 2 else ==> 10 end in >(x, -(z, y)) else ==> cond let l = cons(1 cons(5 cons(3 empty))) in ==(nth(l, 0), 1) ==> 30 else ==> false end end ==> +(77, 33) else ==> 15 end"))

(check-equal? (evaluar-programa expc12) 1)
(check-equal? (evaluar-programa expc13) 110)