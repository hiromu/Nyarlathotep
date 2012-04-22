#!/usr/bin/env gosh

(define (usage)
    (format (current-error-port)
        "Usage: ~a [source file]\n" *program-name*)
    (exit 2))

(define true
    (lambda (x) (lambda (y) x)))

(define false
    (lambda (x) (lambda (y) y)))

(define empty-list
    (lambda (x) true))

(define cons-list
    (lambda (x) (lambda (y) (lambda (z) ((z x) y)))))

(define car-list
    (lambda (x) (x true)))

(define cdr-list
    (lambda (x) (x false)))

(define null-list
    (lambda (x) (x (lambda (y) (lambda (z) false)))))

(define s
    (lambda (x) (lambda (y) (lambda (z) ((x z) (y z))))))

(define k
    (lambda (x) (lambda (y) x)))

(define i
    (lambda (x) x))

(define u
    (lambda (x) ((x s) k)))

(define zero
    (lambda (x) (lambda (y) y)))

(define succ
    (lambda (x) (lambda (y) (lambda (z) (y ((x y) z))))))

(define church-num
    (lambda (n)
        (if (= n 0) zero
            (succ (church-num (- n 1))))))

(define (cons-string input church-list)
    (if (null? input) church-list
        (cons-string (cdr input) ((cons-list (church-num (car input))) church-list))))

(define (church input)
    (cons-string (reverse (map char->integer (string->list input))) empty-list))

(define (unchurch output)
    (list->string
        (map integer->char
            (reverse
                (do ((church-list output (cdr-list church-list))
                        (string-list () (cons (((car-list church-list) (lambda (x) (+ x 1))) 0) string-list)))
                    ((eq? (((null-list church-list) 1) 0) 1) string-list)
                    ())))))

(define (iota source)
    (let ((char (read-char source)))
        (if (eq? #\う char)
            (let ((f (iota source))) (f (iota source)))
            (if (eq? #\に char) u
                (if (not (eof-object? char)) (iota source))))))

(define (main args)
    (if (null? (cdr args))
        (usage)
        (let ()
            (display "> ") (flush)
            (port-for-each
                (lambda (input)
                    (let ()
                        (print (unchurch ((iota (open-input-file (car (cdr args)))) (church input))))
                        (display "> ") (flush)))
                read-line))))
