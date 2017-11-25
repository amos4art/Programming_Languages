#lang racket

(provide (all-defined-out))

(define (sequence low high stride)
  (cond [(> low high) null]
        [#t (cons low (sequence (+ low stride) high stride))]))
        
(define (string-append-map xs suffix )
   (map (lambda (arg) (string-append arg suffix))  xs))

(define (list-nth-mod xs n)
  (cond [ (> 0 n) (raise (error 'list-nth-mod "negative number"))]
        [ (null? xs) (raise (error 'list-nth-mod "empty list")) ]
        [#t (letrec ([ i (remainder (length xs) n) ]
                     [ getith (lambda(arglist counter) (if (= counter i) (car arglist) (getith (cdr arglist) (+ counter 1))))])
              (getith xs 0))]))

(define (stream-for-n-steps s n)
  (letrec ([f (lambda (fstream fnum)
                (if (= fnum n )
                    (cons (car (fstream)) null)
                    (cons (car (fstream)) (f (cdr (fstream)) (+ fnum 1)))))])
    (f s 1)))


(define funny-number-stream
   (letrec ([convert (lambda (y) (if (= 0 (remainder y 5)) (- 0 y) y))]
            [f (lambda (x) (cons (convert x) (lambda () (f (+ x 1)))))])
     (lambda () (f 1)))) 

(define dan-then-dog
   (letrec ([f (lambda (x) (if (= 0 (remainder x 2))
                               (cons "dog.jpg" (lambda () (f (+ x 1))))
                               (cons "dan.jpg" (lambda () (f (+ x 1))))))])
     (lambda () (f 1))))


(define (stream-add-zero s)
  (letrec ([f (lambda (fstream)
                    (cons (cons 0 (car (fstream))) (stream-add-zero (cdr (fstream)))))])
    (lambda () (f s))))


(define (cycle-lists-2 xs ys)
  (letrec ([f (lambda (xlist ylist)
                  (cons (cons (car xlist) (car ylist))
                        (f (cdr xlist) (cdr ylist))))])
    (lambda () (f xs ys))))


(define (cycle-lists xs ys)
  (letrec ([f (lambda (xlist ylist)
                  (cons (cons (if (null? xlist) (car xs) (car xlist))
                              (if (null? ylist) (car ys) (car ylist)))
                        (lambda () (f (if (null? xlist) (cdr xs) (cdr xlist))
                                      (if (null? ylist) (cdr ys) (cdr ylist))))))])
    (lambda () (f xs ys))))




(define ones (lambda () (cons 1 ones)))

(define nats
  (letrec ([f (lambda (x) (cons x (lambda () (f (+ x 1)))))])
    (lambda () (f 1)))) 