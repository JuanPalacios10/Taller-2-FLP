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
(define expc5 (scan&parse "cond -(1,1) ==> 2 -(3,3) ==> 0 +(1,2) ==> 3 else ==> 10 end"))

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