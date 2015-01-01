(define-module gpsystem.util
  (export
    init
    take
    shuffle
    random-find
    replace
    clone
    )
  )
(select-module gpsystem.util)

(use srfi-27)
(use srfi-43)

(define (init lst)
  (if (null? (cdr lst))
    '()
    (cons (car lst) (init (cdr lst)))))

(define (take n lst)
  (define (take-aux n lst result)
    (if (or (< n 1) (null? lst))
      (reverse result)
      (take-aux (- n 1) (cdr lst) (cons (car lst) result))))
  (take-aux n lst '()))

(define (shuffle lst)
  (let1 vec (list->vector lst)
    (do ((i (- (vector-length vec) 1) (- i 1)))
      ((< i 1) (vector->list vec))
      (vector-swap! vec i (random-integer (+ i 1))))))

; (a -> Boolean) -> List<a> -> Maybe<a>
(define (random-find pred lst)
  (let1 results (filter pred lst)
    (if (null? results)
      #f
      (list-ref results (random-integer (length results))))))

(define (replace from to lst)
  (if (null? lst)
    lst
    (if (equal? (car lst) from)
      (cons to (replace from to (cdr lst)))
      (cons (car lst) (replace from to (cdr lst))))))

(define (clone obj)
  (let* ((klass (class-of obj))
         (new-obj (make klass))
         (slot-names (map slot-definition-name (class-slots klass))))
    (for-each
      (lambda (slot)
        (when (slot-bound? obj slot)
          (slot-set! new-obj slot (slot-ref obj slot))))
      slot-names)
    new-obj))

(provide "gpsystem/util")
