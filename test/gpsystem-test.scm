#!/usr/bin/env gosh

(use gauche.test)

(test-start "gpsystem tests")

(require "gpsystem")
(import gpsystem)
(test-module 'gpsystem)

(test-section "func:func-gene-type?")
(test* "single type is not function" #f (func-gene-type? (@gene-type a)))
(test* "type with arrow is function" #t (func-gene-type? (@gene-type a -> b)))
(test* "type with arrow is function" #t (func-gene-type? (@gene-type a -> b -> c)))

(test-section "func:applicable-gene-type?")
(define (x->boolean x) (if x #t #f))
(test* "" #t
       (x->boolean
         (applicable-gene-type?
          (@gene-type Integer -> String)
          (@gene-type Integer))))
(test* "" #t
       (x->boolean
        (applicable-gene-type?
          (@gene-type a -> String)
          (@gene-type Integer))))
(test* "" #t
       (x->boolean
        (applicable-gene-type?
          (@gene-type a -> b -> c)
          (@gene-type Integer))))

(test* "" #f
       (x->boolean
        (applicable-gene-type?
          (@gene-type a -> b)
          (@gene-type Integer -> String))))

(test-section "func:apply-gene-type")
(test* "" (@gene-type String)
       (apply-gene-type
         (@gene-type Integer -> String)
         (@gene-type Integer)))

(test* "" (@gene-type String)
       (apply-gene-type
         (@gene-type x -> String)
         (@gene-type Integer)))

(test* "" (@gene-type Integer -> Integer)
       (apply-gene-type
         (@gene-type x -> x -> x)
         (@gene-type Integer)))

(test* "" (@gene-type Integer -> Integer)
       (apply-gene-type
         (@gene-type x -> x -> x)
         (@gene-type Integer)))

(test* "" (@gene-type a -> x)
       (apply-gene-type
         (@gene-type x -> a -> x)
         (@gene-type a)))

(test-section "func:composable-gene-type?")
(test* "" #f
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer)
          (@gene-type Integer))))

(test* "" #f
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer -> Integer)
          (@gene-type Integer))))

(test* "" #f
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer)
          (@gene-type Integer -> Integer))))

(test* "" #t
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer -> Boolean)
          (@gene-type String -> Integer))))

(test* "" #t
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer -> Boolean)
          (@gene-type String -> Integer -> Integer))))

(test* "" #f
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer -> Integer -> Boolean)
          (@gene-type String -> Integer))))

(test* "" #t
       (x->boolean
         (composable-gene-type?
          (@gene-type x -> Boolean)
          (@gene-type String -> Integer))))

(test* "" #f
       (x->boolean
         (composable-gene-type?
          (@gene-type Integer -> Boolean)
          (@gene-type String -> x))))

(test-section "func:compose-gene-type")
(test* "" (@gene-type Integer -> Boolean)
       (compose-gene-type
         (@gene-type String -> Boolean)
         (@gene-type Integer -> String)))

(test* "" (@gene-type Integer -> Window -> Boolean)
       (compose-gene-type
         (@gene-type String -> Boolean)
         (@gene-type Integer -> Window -> String)))

(test* "" (@gene-type Integer -> String)
       (compose-gene-type
         (@gene-type x -> x)
         (@gene-type Integer -> String)))

(test-section "func:gene-type-variable-sym?")
(test* "" #t (x->boolean (gene-type-variable-sym? 'x)))
(test* "" #t (x->boolean (gene-type-variable-sym? 'xyz)))
(test* "" #f (x->boolean (gene-type-variable-sym? 'X)))
(test* "" #f (x->boolean (gene-type-variable-sym? 'x<y>)))

(test-section "func:gene-type-value-sym?")
(test* "" #t (x->boolean (gene-type-value-sym? 'X)))
(test* "" #t (x->boolean (gene-type-value-sym? 'Integer)))
(test* "" #t (x->boolean (gene-type-value-sym? 'List<Integer>)))
(test* "" #f (x->boolean (gene-type-value-sym? 'x)))
(test* "" #f (x->boolean (gene-type-value-sym? 'xyz)))

(test-end :exit-on-failure #t)
