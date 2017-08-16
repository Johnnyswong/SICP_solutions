#lang planet neil/sicp
;1.3
(define max-two
  (lambda (x y)
    (if (>= x y)
        x
        y)))

(define max-three
  (lambda (x y z)
    (let ([max (max-two x y)])
      (max-two max z))))

;1.11
(define (f n)
  (define (f-iter a b c k)
    (cond ((< n 3) n)
          ((>= k n) c)
          (else (f-iter b c (+ c (* 2 b) (* 3 a)) (+ k 1)))))
  (f-iter 0 1 2 2))

;1.16
(define square
  (lambda (x)
    (* x x)))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (fast-expt-iter b n)
  (define (iter prod k)
    (cond ((= n 0) 1)
          ((>= k n) prod)
          (else
           (if (< (* 2 k) n)
               (iter (square prod) (* 2 k))
               (iter (* b prod) (+ k 1))))))
  (iter b 1))

;version 2
(define (fast-exp b n)
  (define (iter b k a)
    (cond ((= n 0) a)
          ((= k 0) a)
          ((even? k)
           (iter (square b) (/ k 2) a))
          ((odd? k)
           (iter b (- k 1) (* a b)))))
  (iter b n 1))

;1.17
(define (mul a b)
  (if (= b 0)
      0
      (+ a (mul a (- b 1)))))

(define halve
  (lambda (x)
    (/ x 2)))

(define double
  (lambda (x)
    (+ x x)))

(define (prod a b)
  (define (iter a k result)
    (cond ((= b 0) 0)
          ((= k 0) result)
          ((even? k) (iter (double a) (halve k) result))
          ((odd? k) (iter a (- k 1) (+ result a)))))
  (iter a b 0))

;1.19
(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        b
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 1 0 n))

(define (pp p q)
  (+ (square p) (square q)))

(define (qq p q)
  (+ (* p q) (* (+ p q) q)))

(define (a-next a b p q)
  (+ (* b q) (* a q) (* a p)))

(define (b-next a b p q)
  (+ (* b p) (* a q)))

(define (fib-log n)
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a b (pp p q) (qq p q) (/ count 2)))
          ((odd? count)
           (fib-iter (a-next a b p q) (b-next a b p q) p q (- count 1)))))
  (fib-iter 1 0 0 1 n))

;1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;1.21
(define (smallest-divisor n)
  (find-divisor n 2))

(define (smallest-fast-divisor n)
  (find-divisor-new n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (find-divisor-new n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? n test-divisor) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (prime-fast? n)
  (= n (smallest-fast-divisor n)))

;1.22-pre
;(xy)%n = (x%n*y%n)%n
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* (remainder base m) (expmod base (- exp 1) m)) m))))

(define (expmod-why? base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m)) m))
        (else
         (remainder (* base (expmod base (- exp 1) m)) m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod-why? a n n) (remainder a n)))
  (try-it (+ 1 (random (- n 1))))
  )

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((not (fermat-test n)) #f)
        (else
         (fast-prime? n (- times 1)))))

;1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes n)
  (if (odd? n)
      (start-prime-test n (runtime))
      (search-for-primes (+ n 1))))

;1.23
(define (next input)
  (cond ((= input 2) 3)
        (else (+ input 2))))

;chapter1.3
(define (cube x)
  (* x x x))

(define (sum-intergers a b)
  (if (> a b)
      0
      (+ a (sum-intergers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;1.29
(define (inc x)
  (+ x 1))

(define (identity x)
  x)

(define (simpson f a b n)
   (define (term k)
     (let ((h (/ (- b a) n)))
       (let ((yi (f (+ a (* k h)))))
         (cond ((= k 0) yi)
               ((= k n) yi)
               ((even? k) (* 2.0 yi))
               ((odd? k) (* 4.0 yi))))))
  (define (iter k result)
    (if (> k n)
        (let ((h (/ (- b a) n)))
          (/ (* h result) 3))
        (iter (inc k) (+ (term k) result))))
  (iter 0 0.0))

;1.30
(define (sums term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))

;1.31
(define (product a term next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* (term a) result))))
  (iter a 1))

(define (factorial n)
  (if (= n 0)
      1
      (product 1 identity inc n)))

(define (productr a term next b)
  (if (> a b)
      1
      (* (term a) (productr (next a) term next b))))

(define (factorialr n)
  (if (= n 0)
      1
      (productr 1 identity inc n)))

(define (factor x)
  (let ((f1 (/ (* 2.0 x) (+ (* 2.0 x) 1.0)))
        (f2 (/ (+ (* 2.0 x) 2.0) (+ (* 2.0 x) 1.0))))
    (* f1 f2)))

(define pi (lambda (n) (* (product 1 factor inc n) 4)))

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result))))
  (iter a null-value))

(define add (lambda (a b) (+ a b)))
(define prodd (lambda (a b) (* a b)))

(define sum-new (lambda (term a next b) (accumulate add 0 term a next b)))
(define product-new (lambda (term a next b) (accumulate prodd 1 term a next b)))

;1.35
(define average (lambda (a b) (/ (+ a b) 2.0)))

(define close-enough? (lambda (a b) (< (abs (- b a)) 1e-6)))

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
        mid-point
        (let ((test-point (f mid-point)))
          (cond ((positive? test-point)
                 (search f neg-point mid-point))
                ((negative? test-point)
                 (search f mid-point pos-point))
                (else
                 mid-point))))))

(define tolerance 1e-4)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess time)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          (display (cons next time))
          (try next (+ time 1)))))
  (try first-guess 0))

(define gold-func (lambda (x) (+ 1.0 (/ 1.0 x))))
(define log-func (lambda (x) (/ (log 1000) (log x))))
(define log-func-z (lambda (x) (average x (/ (log 1000) (log x)))))

;1.37
(define (cont-frac N D k)
    (define (cf i)
        (if (= k i)
            (/ (N k) (D k))
            (/ (N i)
               (+ (D i) (cf (+ i 1))))))
    (cf 1))

(define (cont-frac-iter N D k)
  (define (iter i result)
    (if (= i 0)
        result
        (iter (- i 1)
              (/ (N i)
                 (+ (D i) result)))))
  (iter (- k 1)
        (/ (N k) (D k))))

(define cont-test (lambda (k)
                    (cont-frac (lambda (i) 1.0)
                               (lambda (i) 1.0)
                               k)))
; 1.38
(define di (lambda (x)
             (cond ((= x 1) 1)
                   ((= x 2) 2)
                   ((= (remainder x 3) 0) 1)
                   ((= (remainder x 3) 1) 1)
                   ((= (remainder x 3) 2) (* 2 (+ 1 (/ (- x 2) 3)))))))


(define e-test (lambda (k)
                 (+ (cont-frac (lambda (i) 1.0)
                            di
                            k) 2)))

; 1.39
(define (tan-cf x k)
  (define (tan-cf-iter x k)
    (define (iter i)
      (if (= i k)
          (/ (square x) (+ (* 2 i) 1))
          (- 1 (/ (square x) (iter (+ i 1))))))
    (iter 0))
  (/ x (tan-cf-iter x k)))

;1.40
(define dx 1e-6)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (fixed-point-iter f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (newtons-method g guess)
  (fixed-point-iter (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x)
    (+ (cube x) (* a (square x)) (* b x) c)))

;1.41
(define (doub precedure)
  (lambda (x) (precedure (precedure x))))

(define (compose f g)
  (lambda (x) (f (g x))))
;1.43
(define (repeated f n)
  (cond ((= n 1) (lambda (x) (f x)))
        ((even? n) (doub (repeated f (/ n 2))))
        ((odd? n) (compose f (repeated f (- n 1))))))
;1.44
(define average-three
  (lambda (x y z)
    (/ (+ x y z) 3)))

(define d-x 1e-6)

(define (smooth f)
  (lambda (x)
    (average-three (f (- x d-x)) (f x) (f (+ x d-x)))))

(define (smooth-n f n)
  (repeated f n))

;1.45
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (times n)
  (if (odd? n)
      (+ (/ (- n 1) 2) 1)
      (/ n 2)))

(define (n-root x n)
  (let ((count (times n)))
    (fixed-point-iter (repeated (average-damp (lambda (y) (/ x (fast-exp y (- n 1))))) count) 1.0)))

;1.46
(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (iter (improve guess))))
  (iter 1.0))

(define sqrt (lambda (x) (iterative-improve close-enough? (lambda (y) (average y (/ x y))))))

(define fixed-point-itera (lambda (f) (iterative-improve close-enough? f)))







