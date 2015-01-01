(define-module gpsystem
  (export-all)) ;;FIXME 
;  (export
;    @gene
;    @gene-type
;    make-gp-env
;    exec-natural-selection-loop
;    ))
(select-module gpsystem)

(use gpsystem.util) ; FIXME define-moduleの中に書きたい

(use srfi-27)   ; random-integer

; init random source
(define (init-gp!)
  (random-source-randomize! default-random-source))

(define *max-num-of-genes* 1000)

(define (make-empty-gene-table)
  (make-hash-table 'equal?))

(define *base-gene-table* (make-empty-gene-table))
(define *gene-type-variable-regexp* #/^[a-z]*$/)
(define *gene-type-value-regexp* #/^[A-Z][a-zA-Z<>]*$/)

(define (get-all-base-genes)
  (apply append (hash-table-values *base-gene-table*)))

(define (make-gp-env :key expect-gene-type gene-eval-func gene-select-pred input-list eval-module)
  (make <gp-env>
    :expect-type expect-gene-type
    :input-list input-list
    :eval-func  gene-eval-func
    :filter-pred gene-select-pred
    :eval-module eval-module
    ))

(define (gene-type-variable-sym? gtype)
  (*gene-type-variable-regexp* (symbol->string gtype)))

(define (gene-type-value-sym? gtype)
  (*gene-type-value-regexp* (symbol->string gtype)))

(define (gene-type-sym? gtype)
  (or (gene-type-value-sym? gtype) (gene-type-variable-sym? gtype)))

;;; =================================================================
;;; Gene type
;;; =================================================================

(define (func-gene-type? gtype)
  (and (pair? gtype)
       (pair? (cdr gtype))))

(define (applicable-gene-type? gt1 gt2)
  (and (func-gene-type? gt1)
       (not (func-gene-type? gt2))
       (pair? gt2) ; FIXME 一時しのぎ
       (let1 fst-type (car gt1)
         (or (equal? fst-type (car gt2))
             (gene-type-variable-sym? fst-type)))))

(define (apply-gene-type gt1 gt2)
  (let1 gt1-first-type (car gt1)
    (replace gt1-first-type (car gt2) (cdr gt1))))

(define (composable-gene-type? gt1 gt2)
  (and (func-gene-type? gt1)
       (func-gene-type? gt2)
       (= (length gt1) 2)
       (or (equal? (last gt2) (car gt1))
           (gene-type-variable-sym? (car gt1)))))

(define (compose-gene-type gt1 gt2)
  (append (init gt2)
          (replace (car gt1) (last gt2) (cdr gt1))))

;;; =================================================================
;;; Gene
;;; =================================================================

(define-class <gene> ()
  ((gene-type :init-keyword :gene-type :accessor gene-type-of)
   (eval-value :init-keyword :eval-value :accessor eval-value-of :init-value 0)))

(define-class <primitive-gene> (<gene>)
  ((code :init-keyword :code :accessor code-of)))

(define-class <composited-gene> (<gene>) ())

(define-class <leaf-gene> (<primitive-gene>) ())

(define-class <function-gene> (<primitive-gene>) ())

(define-class <applied-gene> (<composited-gene>)
  ((node-gene :init-keyword :node-gene :accessor node-gene-of)
   (arg :init-keyword :arg :accessor arg-of)))

(define-class <composed-gene> (<composited-gene>)
  ((left-gene :init-keyword :left-gene :accessor left-gene-of)
   (right-gene :init-keyword :right-gene :accessor right-gene-of)))

(define-method write-object ((gene <gene>) out)
  (let ((type-string (string-join (map symbol->string (gene-type-of gene)) " -> "))
        (code (gen-code gene))
        (eval-value (eval-value-of gene)))
    (write #"[~eval-value] ~code :: ~type-string" out)))

(define (make-leaf-gene gene-type code)
  (make <leaf-gene>
    :gene-type gene-type
    :code code))

(define (make-function-gene gene-type code)
  (make <function-gene>
    :gene-type gene-type
    :code code))

(define-method object-compare ((g1 <gene>) (g2 <gene>))
  (compare (eval-value-of g1) (eval-value-of g2)))

; Gene -> Gene -> Boolean
(define-method applicable? ((g1 <gene>) (g2 <gene>))
  (applicable-gene-type? (gene-type-of g1) (gene-type-of g2)))

; Gene -> Gene -> Gene
(define-method apply ((g1 <gene>) (g2 <gene>))
  (let ((applied-gtype (apply-gene-type (gene-type-of g1) (gene-type-of g2))))
    (make <applied-gene>
      :gene-type applied-gtype
      :node-gene g1
      :arg g2)))

; Gene -> Gene -> Boolean
(define-method composable? ((g1 <gene>) (g2 <gene>))
  (composable-gene-type? (gene-type-of g1) (gene-type-of g2)))

; Gene -> Gene -> Gene
(define-method compose-gene ((g1 <gene>) (g2 <gene>))
  (let ((new-type (compose-gene-type (gene-type-of g1) (gene-type-of g2))))
    (make <composed-gene>
      :gene-type new-type
      :left-gene g1
      :right-gene g2)))

; Gene -> GeneType -> Boolean
(define-method has? ((gene <gene>) gtype)
  (equal? (gene-type-of gene) gtype))

(define (gen-args-list gtype)
  (if (or (null? gtype) (null? (cdr gtype)))
    '()
    (cons (gensym) (gen-args-list (cdr gtype)))))

; Gene -> SExp
(define-method gen-code ((gene <leaf-gene>))
  (code-of gene))

; Gene -> SExp
(define-method gen-code ((gene <function-gene>))
  (code-of gene))

; Gene -> SExp
(define-method gen-code ((gene <applied-gene>))
  (let ((node-code (gen-code (node-gene-of gene)))
        (arg (gen-code (arg-of gene)))
        (args (gen-args-list (gene-type-of gene))))
    (if (null? args)
      `(,node-code ,arg ,@args)
      `(lambda ,args (,node-code ,arg ,@args)))))

; Gene -> SExp
(define-method gen-code ((gene <composed-gene>))
  (let ((left-code (gen-code (left-gene-of gene)))
        (right-code (gen-code (right-gene-of gene)))
        (args (gen-args-list (gene-type-of gene))))
    `(lambda ,args (,left-code (,right-code ,@args)))))

;;; =================================================================
;;; Genetic Operators
;;; =================================================================

; Gene -> List<Gene> -> Gene
(define-method gen-new-gene ((gene <gene>) genes :key (root? #f))
  (if root?
    ((select-root-genetic-operator) gene genes)
    ((select-genetic-operator) gene genes)))

; Gene -> List<Gene> -> Gene
(define-method do-nothing ((gene <primitive-gene>) genes)
  gene)

(define-method do-nothing ((gene <applied-gene>) genes)
  (apply (node-gene-of gene)
         (gen-new-gene (arg-of gene) genes)))

(define-method do-nothing ((gene <composed-gene>) genes)
  (compose-gene (gen-new-gene (left-gene-of gene) genes)
           (gen-new-gene (right-gene-of gene) genes)))

; Gene -> List<Gene> -> Gene
(define-method point-mutation ((gene <primitive-gene>) genes)
  (let1 new-gene (random-find (cut has? <> (gene-type-of gene)) genes)
    (or new-gene gene)))

(define-method point-mutation ((gene <applied-gene>) genes)
  (let* ((node-gene (node-gene-of gene))
         (new-node-gene (random-find (cut has? <> (gene-type-of node-gene)) genes)))
    (apply (or new-node-gene node-gene)
           (gen-new-gene (arg-of gene) genes))))

(define-method point-mutation ((gene <composed-gene>) genes)
  (let* ((left-gene (left-gene-of gene))
         (right-gene (right-gene-of gene))
         (new-left-gene (random-find (cut has? <> (gene-type-of left-gene)) genes))
         (new-right-gene (random-find (cut has? <> (gene-type-of right-gene)) genes)))
    (compose-gene (or new-left-gene left-gene)
                  (or new-right-gene right-gene))))

; Gene -> List<Gene> -> Gene
(define-method mutation ((gene <gene>) genes)
  (let1 new-gene (random-find (cut has? <> (gene-type-of gene)) genes)
    (or new-gene gene)))

; Gene -> List<Gene> -> Gene
(define-method make-composed-gene ((gene <gene>) genes)
  (let1 composable-gene (random-find (cut composable? gene <>) genes)
    (if composable-gene
      (compose-gene gene composable-gene)
      gene)))

; Gene -> List<Gene> -> Gene
(define-method make-applied-gene ((gene <gene>) genes)
  (let1 applicable-gene (random-find (cut applicable? gene <>) genes)
    (if applicable-gene
      (apply gene applicable-gene)
      gene)))

(define genetic-operators
  (list mutation point-mutation do-nothing))

(define root-genetic-operators
  (list mutation point-mutation make-composed-gene make-applied-gene do-nothing))

; Random select operators
; (Gene -> List<Gene> -> Gene)
; FIXME
(define (select-genetic-operator)
  (car (shuffle genetic-operators)))

(define (select-root-genetic-operator)
  (car (shuffle root-genetic-operators)))

;;; =================================================================
;;; Genetic Programming Environment
;;; =================================================================

(define-class <gp-env> ()
  ((expect-type :init-keyword :expect-type :accessor expect-type-of)
   (input-list :init-keyword :input-list :accessor input-list-of)
   (eval-func :init-keyword :eval-func :accessor eval-func-of)
   (filter-pred :init-keyword :filter-pred :accessor filter-pred-of)
   (eval-module :init-keyword :eval-module :accessor eval-module-of)))

(define-method expected-gene-type? ((gp-env <gp-env>) gene)
  (equal? (expect-type-of gp-env) gene))

; GPEnv -> Gene -> Gene
(define-method evaluate ((gp-env <gp-env>) (gene <gene>))
  (let* ((eval-value ((eval-func-of gp-env)
                      gp-env
                      gene
                      (if (equal? (gene-type-of gene) (expect-type-of gp-env))
                        (map (cut execute gp-env gene <>) (input-list-of gp-env))
                        '())))
         (new-gene (clone gene)))
    (set! (ref new-gene 'eval-value) eval-value)
    new-gene))

; GPEnv -> List<Gene> -> List<Gene>
(define-method natural-select ((gp-env <gp-env>) genes)
  (let1 evaluated-genes (map (cut evaluate gp-env <>) genes)
    (select-genes gp-env evaluated-genes)))

; GPEnv -> List<Gene> -> List<Gene>
(define (clone-genes genes)
  (define (clone-genes-aux genes cloned)
    (if (null? genes)
      cloned
      (let1 gene (car genes)
        (clone-genes-aux (cdr genes) (cons gene (cons (clone gene) cloned))))))
  (clone-genes-aux genes '()))

               
(define (gen-new-generation genes)
  (let1 all-genes (append (get-all-base-genes) genes)
    (map (cut gen-new-gene <> all-genes :root? #t) (clone-genes genes))))

; GPEnv -> Int -> GPEnv
(define (exec-natural-selection-loop gp-env generation-count)
  (define (exec-natural-selection-loop-aux generation-count genes)
    (if (<= generation-count 0)
      genes
      (let* ((selected-genes (natural-select gp-env genes))
             (new-genes (gen-new-generation (filter normal-gene? selected-genes))))
        (exec-natural-selection-loop-aux (- generation-count 1) new-genes))))
  (exec-natural-selection-loop-aux generation-count (get-all-base-genes)))

(define (bug-gene? gene)
  (null? (gene-type-of gene)))

(define (normal-gene? gene)
  (pair? (gene-type-of gene)))

;FIXME
; GPEnv -> Gene(a->b) -> a -> b
(define-method execute ((gp-env <gp-env>) (gene <gene>) input)
  (eval `(,(gen-code gene) ',input) (eval-module-of gp-env)))
               
; GPEnv -> List<Gene> -> List<Gene>
(define-method select-genes ((gp-env <gp-env>) evaled-genes)
  (let1 sorted-genes (reverse (sort evaled-genes))
    (take (div *max-num-of-genes* 2) sorted-genes)))

(define-syntax @gene
  (syntax-rules (::)
    ((_ name :: . types)
     (let* ((gtype (@gene-type . types))
            (genes (hash-table-get *base-gene-table* gtype '()))
            (gene (if (func-gene-type? gtype)
                    (make-function-gene gtype 'name)
                    (make-leaf-gene gtype 'name))))
       (hash-table-put! *base-gene-table* gtype (cons gene genes))))))

; TODO implement high-order type
(define-syntax @gene-type
  (syntax-rules (->)
    ((_ type1)
     (if (gene-type-sym? 'type1)
       '(type1)
       (error #"gene type format error: ~'type1")))
    ((_ type1 -> . rest)
     (if (gene-type-sym? 'type1)
       `(type1 ,@(@gene-type . rest))
       (error #"gene type format error: ~'type1")))))

(define input-type-of car)
(define output-type-of last)

(provide "gpsystem")

