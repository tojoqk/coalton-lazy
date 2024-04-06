(defpackage #:tokyo.tojo.lazy/promise
  (:use #:coalton
        #:coalton-prelude)
  (:local-nicknames
   (#:cell #:coalton-library/cell))
  (:export #:delay-force
           #:delay
           #:Promise
           #:force))

(in-package #:tokyo.tojo.lazy/promise)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro delay-force (x)
  `(lazy (fn ((Unit)) ,x)))

(cl:defmacro delay (x)
  `(delay-force (eager ,x)))

(coalton-toplevel
  (repr :transparent)
  (define-type (Promise :a)
    (Promise (Cell (Cell (State :a)))))

  (define-type (State :a)
    (%Lazy (Unit -> (Promise :a)))
    (%Eager :a))

  (declare lazy ((Unit -> (Promise :a)) -> Promise :a))
  (define (lazy thunk)
    (Promise (cell:new (cell:new (%Lazy thunk)))))

  (declare eager (:a -> Promise :a))
  (define (eager x)
    (Promise (cell:new (cell:new (%Eager x)))))

  (declare force (Promise :a -> :a))
  (define (force (Promise current-outer-cell))
    (loop
      (match (cell:read (cell:read current-outer-cell))
        ((%Eager value) (return value))
        ((%Lazy thunk)
         (let (Promise next-outer-cell) = (thunk))
         (let current-inner-cell = (cell:read current-outer-cell))
         (match (cell:read current-inner-cell)
           ((%Eager value) (return value))
           ((%Lazy _)
            (let next-value = (cell:read (cell:read next-outer-cell)))
            (cell:write! current-inner-cell next-value)
            (cell:write! next-outer-cell current-inner-cell))))))
    (error "promsie:force: error"))

  (define-instance (Eq :a => Eq (Promise :a))
    (define (== x y)
      (== (force x) (force y))))

  (define-instance (Ord :a => Ord (Promise :a))
    (define (<=> x y)
      (<=> (force x) (force y))))

  (define-instance (Functor Promise)
    (define (map f p)
      (delay (f (force p)))))

  (define-instance (Applicative Promise)
    (define (pure x) (eager x))
    (define (liftA2 op p1 p2)
      (delay (op (force p1) (force p2)))))

  (define-instance (Monad Promise)
    (define (>>= p f)
      (f (force p))))

  (define-instance (Semigroup :e => Semigroup (Promise (Optional :e)))
    (define (<> a b)
      (delay
       (match (force a)
         ((Some x)
          (match (force b)
            ((Some y) (Some (<> x y)))
            ((None) None)))
         ((None) None)))))

  (define-instance (Monoid :e => Monoid (Promise (Optional :e)))
    (define mempty (delay mempty)))

  (define-instance (Semigroup :b => Semigroup (Promise (Result :a :b)))
    (define (<> pa pb)
      (delay
       (let ((a (force pa)))
         (match a
           ((Ok x)
            (let ((b (force pb)))
              (match b
                ((Ok y) (Ok (<> x y)))
                ((Err _) b))))
           ((Err _) a))))))

  (define-instance (Monoid :b => Monoid (Promise (Result :a :b)))
    (define mempty (delay mempty)))

  (define-instance (Default :a => Default (Promise :a))
    (define (default)
      (delay (default)))))
