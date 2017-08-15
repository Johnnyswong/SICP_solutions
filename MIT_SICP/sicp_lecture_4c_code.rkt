#lang scheme
(define driv
  (lambda (exp var)
    (match exp
      [(? number? x) 0]
      [(? variable? x)
       (if (same-variable? x var)
           1
           0)]
      [`(,op ,u ,v)
       (let ([du (driv u var)])
         (match op
           ['+ (make-sum du (driv v var))]
           ['* (make-sum (make-product u (driv v var)) (make-product v du))]
           ['exp (make-product (make-product v (make-exponentiation u (- v 1))) du)]))]
      [else error "Unknown type" exp]
      )))

(define same-variable?
  (lambda (x y)
    (and (variable? x) (variable? y) (eq? x y))))

(define variable?
  (lambda (x)
    (symbol? x)))

(define =number?
  (lambda (x num)
    (and (number? x) (= x num))))

(define make-sum
  (lambda (a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list '+ a1 a2)))))

(define make-product
  (lambda (m1 m2)
    (cond ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((or (=number? m1 0) (=number? m2 0)) 0)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list '* m1 m2)))))

(define make-exponentiation
  (lambda (base exponent)
    (cond ((=number? base 1) 1)
          ((=number? exponent 0) 1)
          ((and (number? base) (number? exponent)) (expt base exponent))
          (else (list 'exp base exponent)))))

