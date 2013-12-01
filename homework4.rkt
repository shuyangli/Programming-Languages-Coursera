
#lang racket

(provide (all-defined-out)) ;; so we can put tests in a second file

;; put your code below

; 1.
(define (sequence low high stride)
  (if (> low high) null
      (cons low (sequence (+ low stride) high stride))))

; 2.
(define (string-append-map xs suffix)
  (map (lambda (str) (string-append str suffix)) xs))

; 3.
(define (list-nth-mod xs n)
  (if (< n 0) (error "list-nth-mod: empty list")
      (if (null? xs) (error "list-nth-mod: empty list")
          (car (list-tail xs (remainder n (length xs)))))))

; 4.
(define (stream-for-n-steps s n)
  (if (= n 0) null
      (cons (car (s))
            (stream-for-n-steps (cdr (s)) (- n 1)))))

; 5.
(define funny-number-stream
  (letrec ([f (lambda (x)
                (if (= 0 (remainder x 5))
                    (cons (- x) (lambda () (f (+ 1 x))))
                    (cons x (lambda () (f (+ 1 x))))))])
    (lambda () (f 1))))

; 6.
(define dan-then-dog
  (letrec ([f (lambda (x)
                (if (string=? x "dog.jpg")
                    (cons "dan.jpg" (lambda () (f "dan.jpg")))
                    (cons "dog.jpg" (lambda () (f "dog.jpg")))))])
    (lambda () (f "dog.jpg"))))

; 7.
(define (stream-add-zero s)
  (lambda () (cons (cons 0 (car (s))) (stream-add-zero (cdr (s))))))

; 8.
(define (cycle-lists xs ys)
  (letrec ([f (lambda (n)
                (cons (cons (list-nth-mod xs n) (list-nth-mod ys n)) 
                      (lambda () (f (+ 1 n)))))])
    (lambda () (f 0))))

; 9.
(define (vector-assoc v vec)
  (letrec ([f (lambda (n)
                (if (> n (- (vector-length vec) 1))
                    #f
                    (if (pair? (vector-ref vec n))
                        (if (equal? (car (vector-ref vec n)) v)
                            (vector-ref vec n)
                            (f (+ n 1)))
                        (f (+ n 1)))))])
    (f 0)))

; 10.
(define (cached-assoc xs n)
  (lambda (v)
    (let ([cache (make-vector n #f)]
          [index 0])
      (if (vector-assoc v cache)
          (vector-assoc v cache)
          (let ([cur-val (assoc v xs)])
            (begin
              (vector-set! cache index cur-val)
              (set! index
                    (if (> index (- n 1))
                        0
                        (+ index 1)))
              (vector-assoc v cache)))))))