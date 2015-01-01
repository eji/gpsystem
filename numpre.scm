#!/usr/bin/env gosh
;;;
;;; 遺伝的プログラミングを使ってナンプレを解くプログラム
;;;

(use gpsystem)

(use srfi-27)
(use gauche.collection)
(use gauche.sequence)
(use util.combinations)

(define *board-block-size* 3)

;;; =================================================================
;;; Util functions
;;; =================================================================

(define (uniq lst)
  (fold (lambda (x y)
          (if (member x y)
            y
            (cons x y)))
        '()
        lst))

(define (duplicate-exists? lst)
  (< (length (uniq lst)) (length lst)))
(define (repeat n init-thunk)
  (if (zero? n)
    '()
    (cons (init-thunk) (repeat (- n 1) init-thunk))))

(define (make-square-vector size :key init-thunk)
  (let1 rows (make-vector size)
    (do ((i 0 (+ i 1)))
      [(>= i size) rows]
      (vector-set! rows i (if (undefined? init-thunk)
                            (make-vector size)
                            (list->vector (repeat size init-thunk)))))))

(define (make-coordinates size)
  (cartesian-product (list (iota size) (iota size))))

(define first-block-coordinates (make-coordinates *board-block-size*))

(define (make-block-coordinates block-coordinate)
  (let ((block-row-idx (car block-coordinate))
        (block-col-idx (cadr block-coordinate)))
    (define (shift-coordinate coordinate)
      (let* ((row-idx (car coordinate))
             (col-idx (cadr coordinate))
             (shifted-row-idx (+ row-idx (* *board-block-size* block-row-idx)))
             (shifted-col-idx (+ col-idx (* *board-block-size* block-col-idx))))
        (list shifted-row-idx shifted-col-idx)))
    (map shift-coordinate first-block-coordinates)))

(define (make-all-block-coordinates board-size)
  (let* ((block-count (div board-size *board-block-size*))
         (block-coordinates (make-coordinates block-count)))
    (map make-block-coordinates block-coordinates)))

;;; =================================================================
;;; gene definition
;;; =================================================================

[@gene identity :: Board -> Board]
;[@gene identity :: Input -> Input]
[@gene #t :: Boolean]
[@gene #f :: Boolean]
[@gene if :: Boolean -> x -> x -> x]
[@gene and :: Boolean -> Boolean -> Boolean]
[@gene or :: Boolean -> Boolean -> Boolean]
[@gene not :: Boolean -> Boolean]
[@gene begin :: Undefined -> Board -> Board]
[@gene equal? :: x -> x -> Boolean]

;; Board value

(define-class <board-value> ()
  ((raw-value :init-keyword :raw-value :accessor raw-value-of)))

(define-method write-object ((v <board-value>) out)
  (format out "~s" (raw-value-of v)))

(define-method object-equal? ((a <board-value>) (b <board-value>))
  (equal? (raw-value-of a) (raw-value-of b)))

(define (make-board-value raw-value)
  (make <board-value> :raw-value raw-value))

(define (make-empty-board-value)
  (make <board-value> :raw-value empty-value))

(define empty-value (make-board-value 0))

[@gene empty-value? :: BoardValue -> Boolean]
(define-method empty-value? ((bv <board-value>))
  (equal? bv empty-value))

(define-method set-value! ((self <board-value>) (new-val <integer>))
  (set! (ref self 'raw-value) new-val))

[@gene set-value! :: BoardValue -> BoardValue -> Undefined]
(define-method set-value! ((self <board-value>) (new-val <board-value>))
  (set-value! self (raw-value-of new-val)))

[@gene *value-range* :: List<BoardValue>]
(define *value-range* (map make-board-value (iota 9 1)))

;; Board

(define-class <base-board> ()
  ((rows :init-keyword :rows :accessor rows-of)
   (cols :init-keyword :cols :accessor cols-of)
   (height :init-keyword :height :accessor height-of)
   (width :init-keyword :width :accessor width-of)
   (right-down-diagonal :init-keyword :right-down-diagonal :accessor right-down-diagonal-of)
   (right-up-diagonal :init-keyword :right-up-diagonal :accessor right-up-diagonal-of)))

(define-class <board> (<base-board>)
  ((blocks :init-keyword :blocks :accessor blocks-of)))

(define-class <board-block> (<base-board>) ())

(define-method sync-fields! ((board <base-board>))
  (for-each-with-index
    (lambda (row-idx row)
      (for-each-with-index
        (lambda (col-idx v)
            (vector-set! (col-of board col-idx) row-idx v)
            (when (= row-idx col-idx)
              (vector-set! (right-down-diagonal-of board) col-idx v))
            (when (= (- (- (height-of board) 1) row-idx) col-idx)
              (vector-set! (right-up-diagonal-of board) col-idx v)))
        row))
    (rows-of board)))

(define (make-empty-board klass size :key (sync? #f))
  (let1 board (make klass
                :width size
                :height size
                :rows (make-square-vector size :init-thunk make-empty-board-value)
                :cols (make-square-vector size)
                :right-down-diagonal (make-vector size)
                :right-up-diagonal (make-vector size))
    (when sync? (sync-fields! board))
    board))

[@gene make-board :: Input -> Board]
(define (make-board lst-tbl)
  (let* ((board-size (length lst-tbl))
         (board (make-empty-board <board> board-size :sync? #t))
         (all-block-coordinates (make-all-block-coordinates board-size)))

    (for-each-with-index
      (lambda (row-idx row)
        (for-each-with-index
          (lambda (col-idx v)
            (let1 bv (value-of board row-idx col-idx)
              (set! (ref bv 'raw-value) v)))
          row)
          (rows-of board))
      lst-tbl)

    (set! (ref board 'blocks)
      (map (lambda (block-coordinate)
             (let1 block (make-empty-board <board-block> *board-block-size*)
               (for-each (lambda (coordinate)
                           (let* ((row-idx (car coordinate))
                                  (col-idx (cadr coordinate))
                                  (val (value-of board row-idx col-idx))
                                  (block-row-idx (mod row-idx *board-block-size*))
                                  (block-col-idx (mod col-idx *board-block-size*)))
                             (vector-set! (row-of block block-row-idx) block-col-idx val)))
                         block-coordinate)
               (sync-fields! block)
               block))
           all-block-coordinates))
    board))

[@gene row-of :: Board -> Vector<BoardValue>]
[@gene row-of :: BoardBlock -> Vector<BoardValue>]
(define-method row-of ((board <base-board>) row-idx)
  (vector-ref (rows-of board) row-idx))

[@gene col-of :: Board -> Vector<BoardValue>]
[@gene col-of :: BoardBlock -> Vector<BoardValue>]
(define-method col-of ((board <base-board>) col-idx)
  (vector-ref (cols-of board) col-idx))

(define-method value-of ((board <base-board>) row-idx col-idx)
  (vector-ref (row-of board row-idx) col-idx))

[@gene all-values :: Board -> List<BoardValue>]
[@gene all-values :: BoardBlock -> List<BoardValue>]
(define-method all-values ((board <base-board>))
  (apply append (map vector->list (vector->list (rows-of board)))))

(for-each
  (lambda (block-coordinate)
    (for-each
      (lambda (coordinate)
        (let* ((row-idx (car coordinate))
               (col-idx (cadr coordinate)))
          (for-each
            (lambda (val)
              (eval `[@gene (lambda (b) (set-value! b ,row-idx ,col-idx ,val)) :: Board -> Undefined] (current-module)))
            (iota 10 1))))
      block-coordinate))
  (make-all-block-coordinates 9))
(define-method set-value! ((board <base-board>) row-idx col-idx new-val)
  (set-value! (value-of board row-idx col-idx) new-val))

[@gene selected-values :: BoardBlock -> List<BoardValue>]
(define-method selected-values ((bb <board-block>))
  (filter (lambda (v) (not (equal? empty-value v)))
          (apply append (map (cut vector->list <>) (rows-of bb)))))

[@gene unselected-values :: BoardBlock -> List<BoardValue>]
(define-method unselected-values ((bb <board-block>))
  (let1 selected (selected-values bb)
    (filter (lambda (v) (not (member v selected))) *value-range*)))

[@gene completed-board? :: Board -> Boolean]
(define (completed-board? board)
  (and 
    (every completed-vline? (vector->list (rows-of board)))
    (every completed-vline? (vector->list (cols-of board)))))
;    (completed-vline? (right-down-diagonal-of board))
;    (completed-vline? (right-up-diagonal-of board))))

[@gene completed-board-block? :: BoardBlock -> Boolean]
(define (completed-board-block? bb)
  (completed-line? (selected-values bb)))

[@gene completed-vline? :: Vector<BoardValue> -> Boolean]
(define (completed-vline? vline)
  (completed-line? (vector->list vline)))

[@gene completed-line? :: List<BoardValue> -> Boolean]
(define (completed-line? line)
  (= (length (uniq line)) (lenght *value-range*)))

[@gene valid-line? :: List<BoardValue> -> Boolean]
(define (valid-line? line)
  (let1 non-empty-vals (filter (.$ not empty-value?) line)
    (not (duplicate-exists? non-empty-vals))))

[@gene valid-vline? :: Vector<BoardValue> -> Boolean]
(define (valid-vline? vline)
  (valid-line? (vector->list vline)))

[@gene valid-board? :: Board -> Boolean]
(define-method valid-board? ((board <board>))
  (and 
    (every valid-vline? (vector->list (rows-of board)))
    (every valid-vline? (vector->list (cols-of board)))))
;    (valid-vline? (right-down-diagonal-of board))
;    (valid-vline? (right-up-diagonal-of board))))

;;; =================================================================
;;; gene evaluator
;;; =================================================================

(define *max-evaluate-value* 100)

(define (calc-evaluate-value board)
  (let* ((all-vals (all-values board))
         (empty-vals (filter empty-value? all-vals)))
    (floor (* (- 1 (/ (length empty-vals)
                      (length all-vals)))
              100))))

(define (gene-result-evaluator gp-env gene result-board)
  (let ((eval-val 0)
        (expect-type (expect-type-of gp-env)))
    (cond
      ((equal? expect-type (gene-type-of gene))
       (inc! eval-val 100)
       (when (valid-board? result-board)
         (inc! eval-val 20)
         (inc! eval-val (calc-evaluate-value result-board))))
      ((equal? (input-type-of (gene-type-of gene)) (input-type-of expect-type))
       (inc! eval-val 40))
      ((equal? (output-type-of (gene-type-of gene)) (output-type-of expect-type))))
    eval-val))

; Gene -> List<Board> -> EvalValue
(define (gene-results-evaluator gp-env gene result-boards)
  (let* ((results (map (cut gene-result-evaluator gp-env gene <>) result-boards))
         (sum-results (fold + 0 results))
         (results-count (length results)))
    (if (zero? results-count)
      0
      (floor (/ sum-results (length results))))))


; EvalValue -> Boolean
(define (can-pass? evaluate-value)
  (let1 rval (+ (random-integer *max-evaluate-value*) 1)
    (<= rval evaluate-value)))

;;; =================================================================
;;; Entry point
;;; =================================================================
(define *default-max-generation* 1000)

(define (main args)
  (init-gp!)
  (let1 gp-env (make-gp-env
                 :expect-gene-type [@gene-type Input -> Board]
                 :input-list '(((0 0 0  0 2 6  0 0 0)
                                (0 0 0  0 4 8  0 0 0)
                                (0 0 7  5 3 9  1 4 0)

                                (0 0 0  0 9 1  0 0 0)
                                (9 1 4  2 8 7  6 5 3)
                                (0 0 0  0 5 3  0 0 0)

                                (0 0 0  9 6 4  8 0 0)
                                (0 0 3  0 7 2  0 1 0)
                                (0 6 0  0 1 5  0 0 2)))
                 :gene-eval-func gene-results-evaluator
                 :gene-select-pred can-pass?
                 :eval-module (current-module))
    (let1 genes (exec-natural-selection-loop gp-env 10)
       (for-each
         (^ (gene)
            (display (class-name (class-of gene)))
            (display ": ")
            (display gene)
            (newline))
         (reverse (sort genes))))))

