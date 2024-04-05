(defpackage #:tokyo.tojo.lazy/lazy
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton-library/cell)
   (#:promise #:tokyo.tojo.lazy/promise))
  (:export #:Lazy
           #:delay
           #:force))

(in-package #:tokyo.tojo.lazy/lazy)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro delay (x)
  `(Lazy (promise:delay ,x)))

(cl:defmacro delay-force (x)
  `(Lazy (promise:delay-force (lazy-promise ,x))))

(coalton-toplevel
  (repr :transparent)
  (define-type (Lazy :m :a)
    (Lazy (promise:Promise (:m :a))))

  (define (lazy-promise (Lazy p)) p)

  (define (force (Lazy p))
    (promise:force p))

  (define-instance (Functor :f => Functor (Lazy :f))
    (define (map f (Lazy p))
      (delay (map f (promise:force p)))))

  (define-instance (Applicative (Lazy Optional))
    (define (pure x)
      (delay (pure x)))
    (define (liftA2 op (Lazy p1) (Lazy p2))
      (delay
       (match (promise:force p1)
         ((Some x1)
          (match (promise:force p2)
            ((Some x2) (Some (op x1 x2)))
            ((None) None)))
         ((None) None)))))

  (define-instance (Alternative (Lazy Optional))
    (define (alt (Lazy p1) (Lazy p2))
      (delay-force
       (Lazy
        (match (promise:force p1)
          ((Some _) p1)
          ((None) p2)))))
    (define empty (delay None)))

  (define-instance (Applicative (Lazy (Result :e)))
    (define (pure x)
      (delay (pure x)))
    (define (liftA2 op (Lazy p1) (Lazy p2))
      (delay
       (match (promise:force p1)
         ((Ok x1)
          (match (promise:force p2)
            ((Ok x2) (Ok (op x1 x2)))
            ((Err e) (Err e))))
         ((Err e) (Err e))))))

  (define-instance (Unwrappable :c => Unwrappable (Lazy :c))
    (define (unwrap-or-else f default c)
      (unwrap-or-else f default (force c)))))
