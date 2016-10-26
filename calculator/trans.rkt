#lang racket
;(define a "1+2*3")
;(define b '())
;(define (trans)
;  (for ([i a][j (in-range (string-length a))])
;    (if (not (char-numeric? i))
;        (begin
;          (cond [(eq? i #\+)(set! b (cons '+ b))]
;                [(eq? i #\-)(set! b (cons '- b))]
;                [(eq? i #\*)(set! b (cons '* b))]
;                [(eq? i #\/)(set! b (cons '/ b))])
;          (set! a (string-replace a (substring a j (+ j 1)) " "))
;          (set! b (reverse b)))
;        (void))))
;(trans)
;(set! a (for/list ([i (string-split a)])
;  (string->number i)))
;(for ([i b][j (in-range (length b))])
;  (cond [(or (eq? i '*) (eq? i '/))
;         (set! a ())]))

(define (trans a)
  (define b '())
  (cond [(eq? (string-ref a 1) #\+)(set! b (append b '(+)))]
        [(eq? (string-ref a 1) #\-)(set! b (append b '(-)))]
        [(eq? (string-ref a 1) #\*)(set! b (append b '(*)))]
        [(eq? (string-ref a 1) #\/)(set! b (append b '(/)))])
  (set! b (append b (list (- (char->integer (string-ref a 0)) 48))))
  (append b (list (- (char->integer (string-ref a 2)) 48))))