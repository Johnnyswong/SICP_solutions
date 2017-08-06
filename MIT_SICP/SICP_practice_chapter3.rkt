#lang planet neil/sicp
; exercises for chapter 3
; 3.1
(define (make-accumulator balance)
  (lambda (amount)
    (begin (set! balance (+ balance amount))
           balance)))

; 3.2
(define (make-monitored f)
  (define times 0)
  (define (monitor mf)
      (cond ((number? mf) (begin (set! times (+ times 1))
                         (f mf)))
            ((eq? mf 'how-many-calls?) times)
            ((eq? mf 'reset-count) (begin (set! times 0) times))))
  monitor)

; 3.3
; pass
; 3.4
; pass

; 3.5
(define square (lambda (x) (* x x)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

(define (area x1 x2 y1 y2)
  (* (abs (- x1 x2)) (abs (- y1 y2))))

(define (monte-carlo p? x1 x2 y1 y2 times)
  (define (iter expers in_area)
    (let ((x (random-in-range x1 x2))
          (y (random-in-range y1 y2)))
      (cond ((> expers times) (* (/ in_area expers) (area x1 x2 y1 y2)))
            ((p? x y) (iter (+ expers 1.0) (+ in_area 1.0)))
            (else (iter (+ expers 1.0) in_area)))))
  (iter 0.0 0.0))

(define (p? x y)
  (if (<= (+ (square (- x 5)) (square (- y 7))) (square 3))
      #t
      #f))

; 3.6
(define random-init 1008611)

(define rand 
  (let ((state random-init))
    (lambda (mode)
      (cond ((eq? mode 'generate)
             (random state))
            ((eq? mode 'reset)
             (lambda (new-value)
               (begin (set! state new-value)
               state)))
            (else
             (error "Unknow mode -- " mode))))))

;3.7
;pass

;3.8
(define f
  (lambda (first-value)
   (begin (set! f (lambda (second-value) 0))
    first-value)))

;3.9
;pass

;3.10