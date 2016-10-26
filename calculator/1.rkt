#lang racket/gui



(define (trans a)
  (define b '())
  (cond [(eq? (string-ref a 1) #\+)(set! b (append b '(+)))]
        [(eq? (string-ref a 1) #\-)(set! b (append b '(-)))]
        [(eq? (string-ref a 1) #\*)(set! b (append b '(*)))]
        [(eq? (string-ref a 1) #\/)(set! b (append b '(/)))])
  (set! b (append b (list (- (char->integer (string-ref a 0)) 48))))
  (append b (list (- (char->integer (string-ref a 2)) 48))))



(define frame (new frame%[label "计算器"]))
(define msg (new text-field%
                 [label ""]
                 [parent frame]
                 [stretchable-height 1]
                 [stretchable-width 1]
                 ))
(define msg2 (new text-field%
                 [label ""]
                 [parent frame]
                 [stretchable-height 1]
                 [stretchable-width 1]
                 ))
(define pan0 (new horizontal-panel%[parent frame]))
(define pan1 (new horizontal-panel%[parent frame]))
(define pan2 (new horizontal-panel%[parent frame]))
(define pan3 (new horizontal-panel%[parent frame]))
(define pan4 (new horizontal-panel%[parent frame]))

(for ([i (in-range 1 10)][j (list pan1 pan1 pan1 pan2 pan2 pan2 pan3 pan3 pan3)])
  (new button%
       [parent j]
       [label (number->string i)]
       [stretchable-width 1]
       [stretchable-height 1]
       [callback (lambda (button event)(send (send msg get-editor) insert (number->string i)))]))


(define b-> (new button%[label "<-"]
     [parent pan0]
     [stretchable-width 1]
     [stretchable-height 1]
     [callback (lambda (button event)
                 (define a (send msg get-value))
                 (cond [(not (eq? a ""))
                     (send msg set-value (substring a 0 (- (string-length a) 1)))]))]))

(define bc (new button%[label "c"]
     [parent pan0]
     [stretchable-width 1]
     [stretchable-height 1]
     [callback (lambda (button event)
                 (send msg set-value ""))]))

(define b. (new button%[label "."]
     [parent pan4]
     [stretchable-width 1]
     [stretchable-height 1]
     [callback (lambda (button event)
                 (let ([a (send msg get-value)])
                     (if (or (eq? (string-length a) 0) (not (char-numeric? (first (reverse (string->list a))))))
                         (send msg set-value (string-append a "0."))
                         (send msg set-value (string-append a ".")))))]))

(define b0 (new button%[label "0"]
     [parent pan4]
     [stretchable-width 1]
     [stretchable-height 1]
     [callback (lambda (button event)
                 (send (send msg get-editor) insert "0"))]))

(define b= (new button%[label "="]
     [parent pan4]
     [stretchable-width 1]
     [stretchable-height 1]
     [callback (lambda (button event)
                 (send msg2 set-value (number->string (eval (trans (send msg get-value))))))]))

(for ([i '("/" "*" "-" "+")][j (list pan1 pan2 pan3 pan4)])
  (new button%
       [parent j]
       [label i]
       [stretchable-width 1]
       [stretchable-height 1]
       [callback (lambda (button event)
                   (let ([a (send msg get-value)])
                     (cond [(not (eq? (string-length a) 0))
                         (begin
                           (cond [(not (char-numeric? (first (reverse (string->list a)))))(set! a (substring a 0 (- (string-length a) 1)))])
                           (send msg set-value (string-append a i)))])))]))
(send frame show #t)