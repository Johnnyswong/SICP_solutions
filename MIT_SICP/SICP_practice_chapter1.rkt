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












