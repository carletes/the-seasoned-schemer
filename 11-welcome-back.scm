;; My initial take on `two-in-a-row?`:

(define two-in-a-row?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (or (is-first? (car lat) (cdr lat))
	  (two-in-a-row? (cdr lat)))))))

(define is-first?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (eq? a (car lat))))))

;; The revised version of the text couples together `is-first?` and
;; `two-in-a-row?`:

(define two-in-a-row-revised?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (is-first-b? (car lat) (cdr lat))))))

(define is-first-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? a (car lat))
	  (two-in-a-row-revised? lat))))))

;; Next suggested step: Expand `two-in-a-row-revised?` inside
;; `is-first-b?`, and call the resultimg function `two-in-a-row-b?`

(define two-in-a-row-b?
  (lambda (a lat)
    (cond
     ((null? lat) #f)
     (else
      (or (eq? a (car lat))
	  (two-in-a-row-b? (car lat) (cdr lat)))))))

;; We use it to build the final `two-in-a-row?`:

(define two-in-a-row-final?
  (lambda (lat)
    (cond
     ((null? lat) #f)
     (else
      (two-in-a-row-b? (car lat)
		       (cdr lat))))))

;; Next exercise: `sum-of-prefixes`. My first cut, inspired by
;; `two-in-a-row?` and `two-in-a-row-b?`

(define sum-of-prefixes
  (lambda (tup)
    (cond
     ((null? tup) '())
     (else
      (sum-of-prefixes-b (car tup) (cdr tup))))))

(define sum-of-prefixes-b
  (lambda (acc tup)
    (cond
     ((null? tup) (cons acc '()))
     (else
      (cons acc
	    (sum-of-prefixes-b (+ acc (car tup)) (cdr tup)))))))

;; This situation is summarised in the _Eleventh Commandment_: Use
;; additional arguments when a function needs to know what other
;; arguments to the function have been so far.
