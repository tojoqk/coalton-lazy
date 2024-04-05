(defpackage #:tokyo.tojo.lazy/stream
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:cons
           #:append
           #:take
           #:filter
           #:any
           #:repeat
           #:concat)
  (:local-nicknames
   (#:promise #:tokyo.tojo.lazy/promise))
  (:export #:make
           #:zip-with
           #:naturals-from
           #:take
           #:repeat))

(in-package #:tokyo.tojo.lazy/stream)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro cons (h t)
  `(Stream (promise:delay (%Cons ,h ,t))))

(cl:defmacro make (cl:&rest xs)
  (cl:if (cl:consp xs)
         `(cons ,(cl:first xs)
                (make ,@(cl:rest xs)))
         'null))

(cl:defmacro delay-force (s)
  `(Stream (promise:delay-force (stream-promise ,s))))

(coalton-toplevel
  (repr :transparent)
  (define-type (Stream :a)
    (Stream (promise:Promise (%Stream :a))))

  (define (stream-promise (Stream p)) p)

  (define-type (%Stream :a)
    (%Nil)
    (%Cons :a (Stream :a)))

  (declare null (Stream :a))
  (define null
    (Stream (promise:delay %Nil)))

  (declare force (Stream :a -> Optional (Tuple :a (Stream :a))))
  (define (force (Stream p))
    (match (promise:force p)
      ((%Cons h t) (Some (Tuple h t)))
      ((%Nil) None)))

  (define-instance (Functor Stream)
    (define (map f x)
      (delay-force
       (match (force x)
         ((None) null)
         ((Some (Tuple h t))
          (cons (f h) (map f t)))))))

  (declare append (Stream :a -> Stream :a -> Stream :a))
  (define (append s1 s2)
    (delay-force
     (match (force s1)
       ((None) s2)
       ((Some (Tuple h t))
        (cons h (append t s2))))))

  (declare concat ((Stream (Stream :a)) -> (Stream :a)))
  (define (concat s)
    (concat-map id s))

  (declare concat-map ((:a -> Stream :b) -> Stream :a -> Stream :b))
  (define (concat-map f s)
    (delay-force
     (match (force s)
       ((None) null)
       ((Some (Tuple h t))
        (append (f h) (concat-map f t))))))

  (define-instance (Applicative Stream)
    (define (pure x) (make x))
    (define (liftA2 op s1 s2)
      (concat-map (fn (x) (map (op x) s2))
                  s1)))

  (define-instance (Monad Stream)
    (define (>>= m f)
      (concat-map f m)))

  (define-instance (Alternative Stream)
    (define (alt s1 s2)
      (append s1 s2))
    (define empty null))

  (declare take (Integer -> (Stream :a) -> (Stream :a)))
  (define (take n s)
    (delay-force
     (if (<= n 0)
         null
         (match (force s)
           ((None) null)
           ((Some (Tuple h t))
            (cons h (take (1- n) t)))))))

  (define-instance (Into (Stream :a) (List :a))
    (define (into s)
      (match (force s)
        ((None) Nil)
        ((Some (Tuple h t))
         (coalton:Cons h (into t))))))

  (define-instance (Into (List :a) (Stream :a))
    (define (into lst)
      (match lst
        ((Nil) null)
        ((coalton:Cons h t)
         (cons h (into t))))))

  (declare zip-with ((:a -> :b -> :c) -> (Stream :a) -> (Stream :b) -> (Stream :c)))
  (define (zip-with f s1 s2)
    (delay-force
     (match (force s1)
       ((None) null)
       ((Some (Tuple h1 t1))
        (match (force s2)
          ((None) null)
          ((Some (Tuple h2 t2))
           (cons (f h1 h2)
                 (zip-with f t1 t2))))))))

  (declare repeat (:a -> (Stream :a)))
  (define (repeat x)
    (cons x (repeat x)))

  (declare naturals-from (Integer -> (Stream Integer)))
  (define (naturals-from n)
    (cons n (naturals-from (+ n 1))))

  (declare drop-while ((:a -> Boolean) -> (Stream :a) -> (Stream :a)))
  (define (drop-while p? s)
    (delay-force
     (match (force s)
       ((None) null )
       ((Some (Tuple h t))
        (if (p? h)
            (drop-while p? t)
            s)))))

  (declare filter ((:a -> Boolean) -> (Stream :a) -> (Stream :a)))
  (define (filter p? s)
    (delay-force
     (match (force s)
       ((None) null)
       ((Some (Tuple h t))
        (if (p? h)
            (cons h (filter p? t))
            (filter p? t)))))))
