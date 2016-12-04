
(define (caar x) (car (car x)))
(define (cadr x) (car (cdr x)))
(define (cdar x) (cdr (car x)))
(define (cddr x) (cdr (cdr x)))

; Some utility functions that you may find useful to implement.
(define (map proc items)
    (if (null? items) nil
        (cons (proc (car items)) (map proc (cdr items)) )
    )
)

(define (cons-all first rests)
  'replace-this-line)

(define (zip pairs)
  'replace-this-line)

(define (counter x)
  (+ x 1)
)
;; Problem 17
;; Returns a list of two-element lists
(define (enumerate s)
  ; BEGIN PROBLEM 17
  (define (enumerate_helper s index)
      (cond ((null? s) s)
            ( (null? (cdr s))
              (list(append (list index) (cons (car s) nil) ) )
            )
            (else (append (list(append (list index) (list (car s)) ))  (enumerate_helper (cdr s) (+ index 1)) )
            )
      )
  )
  (enumerate_helper s 0)
)
  ; END PROBLEM 17

;; Problem 18
;; List all ways to make change for TOTAL with DENOMS
(define (list-change total denoms)
  ; BEGIN PROBLEM 18

  (cond ((= total 0) (cons nil nil))
        ((< total 0) nil)
        ((null? denoms) nil)
        (else (define used_denom (list-change (- total (car denoms)) denoms))
              (define add_denom (lambda (s) (cons (car denoms) s) ) )
              (define with_denom (map add_denom used_denom) )
              (define without_denom (list-change total (cdr denoms)))
              (append with_denom without_denom)
        )
  )
)
(define (cons-all first rests)
    (if (equal?  rests nil) nil
       (cons (cons first (car rests))
       (cons-all first (cdr rests)) )
    )
)
  
  ; END PROBLEM 18

;; Problem 19
;; Returns a function that checks if an expression is the special form FORM
(define (check-special form)
  (lambda (expr) (equal? form (car expr))))

(define lambda? (check-special 'lambda))
(define define? (check-special 'define))
(define quoted? (check-special 'quote))
(define let?    (check-special 'let))

;; Converts all let special forms in EXPR into equivalent forms using lambda
(define (let-to-lambda expr)
  (cond ((atom? expr) expr)
        ((quoted? expr) expr)
        ((or (lambda? expr) (define? expr))
             (let ((form   (car expr))
                  (params (cadr expr))
                  (body   (cddr expr)))
                  (cons form (cons params (map let-to-lambda body)))
             )
        )
        ((let? expr)
         (let ((values (cadr expr))
               (body   (cddr expr)))           
               (define form (map (lambda (x) (car x)) values))
               (define values (map (lambda (x) (let-to-lambda (cadr x))) values))
               (cons (cons 'lambda (cons form (map let-to-lambda body)) ) values)
          )
         )
        (else (map let-to-lambda expr) )
  )

)
