(defpackage #:tokyo.tojo.lazy/stream
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:cons
           #:append
           #:take
           #:drop
           #:filter
           #:index
           #:head
           #:tail
           #:repeat
           #:length
           #:concat)
  (:local-nicknames
   (#:promise #:tokyo.tojo.lazy/promise)
   (#:iter #:coalton-library/iterator)
   (#:cell #:coalton-library/cell))
  (:export #:Stream
           #:cons
           #:make
           #:force
           #:delay-force
           #:null
           #:head
           #:tail
           #:index
           #:append
           #:concat
           #:concat-map
           #:take
           #:drop
           #:take-while
           #:drop-while
           #:zip-with
           #:iterate
           #:repeat
           #:length
           #:filter))

(in-package #:tokyo.tojo.lazy/stream)

(named-readtables:in-readtable coalton:coalton)

(cl:defmacro cons (h t)
  `(%Stream (promise:delay (Some (Tuple ,h ,t)))))

(cl:defmacro make (cl:&rest xs)
  (cl:if (cl:consp xs)
         `(cons ,(cl:first xs)
                (make ,@(cl:rest xs)))
         'null))

(cl:defmacro delay-force (s)
  `(%Stream (promise:delay-force (stream-promise ,s))))

(coalton-toplevel
  (repr :transparent)
  (define-type (Stream :a)
    (%Stream (promise:Promise (Optional (Tuple :a (Stream :a))))))

  (define (stream-promise (%Stream p)) p)

  (define-type (%Stream :a)
    (%Nil)
    (%Cons :a (Stream :a)))

  (declare force (Stream :a -> Optional (Tuple :a (Stream :a))))
  (define (force (%Stream p))
    (promise:force p))

  (declare null (Stream :a))
  (define null
    (%Stream (promise:delay None)))

  (declare head (Stream :a -> Optional :a))
  (define (head s)
    (match (force s)
      ((Some (Tuple h _)) (Some h))
      ((None) None)))

  (declare tail (Stream :a -> Optional (Stream :a)))
  (define (tail s)
    (match (force s)
      ((Some (Tuple _ t)) (Some t))
      ((None) None)))

  (declare index (UFix -> Stream :a -> Optional :a))
  (define (index idx s)
    (match (force s)
      ((Some (Tuple h t))
       (if (== idx 0)
           (Some h)
           (index (1- idx) t)))
      ((None) None)))

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

  (declare take (UFix -> (Stream :a) -> (Stream :a)))
  (define (take n s)
    (delay-force
     (if (<= n 0)
         null
         (match (force s)
           ((None) null)
           ((Some (Tuple h t))
            (cons h (take (1- n) t)))))))

  (declare drop (UFix -> (Stream :a) -> (Stream :a)))
  (define (drop n s)
    (delay-force
     (if (<= n 0)
         s
         (match (force s)
           ((None) null)
           ((Some (Tuple _h t))
            (drop (1- n) t))))))

  (declare take-while ((:a -> Boolean) -> (Stream :a) -> (Stream :a)))
  (define (take-while p? s)
    (delay-force
     (match (force s)
       ((None) null)
       ((Some (Tuple h t))
        (if (p? h)
            (cons h (take-while p? t))
            null)))))

  (declare drop-while ((:a -> Boolean) -> (Stream :a) -> (Stream :a)))
  (define (drop-while p? s)
    (delay-force
     (match (force s)
       ((None) null)
       ((Some (Tuple h t))
        (if (p? h)
            (drop-while p? t)
            s)))))

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

  (declare iterate ((:a -> :a) -> :a -> (Stream :a)))
  (define (iterate f x)
    (cons x (iterate f (f x))))

  (declare repeat (:a -> (Stream :a)))
  (define repeat (iterate id))

  (declare length (Stream :a -> UFix))
  (define (length s)
    (fold (fn (acc _) (1+ acc)) 0 s))

  (declare filter ((:a -> Boolean) -> (Stream :a) -> (Stream :a)))
  (define (filter p? s)
    (delay-force
     (match (force s)
       ((None) null)
       ((Some (Tuple h t))
        (if (p? h)
            (cons h (filter p? t))
            (filter p? t))))))

  (define-instance (Eq :a => Eq (Stream :a))
    (define (== s1 s2)
      (match (force s1)
        ((Some (Tuple h1 t1))
         (match (force s2)
           ((Some (Tuple h2 t2))
            (and (== h1 h2)
                 (== t1 t2)))
           (_ False)))
        ((None)
         (match (force s2)
           ((None) True)
           (_ False))))))

  (define-instance (Ord :a => Ord (Stream :a))
    (define (<=> s1 s2)
      (match (force s1)
        ((Some (Tuple h1 t1))
         (match (force s2)
           ((Some (Tuple h2 t2))
            (match (<=> h1 h2)
              ((LT) LT)
              ((GT) GT)
              ((EQ) (<=> t1 t2))))
           (_ GT)))
        ((None)
         (match (force s2)
           ((None) EQ)
           (_ LT))))))

  (define-instance (Functor Stream)
    (define (map f x)
      (delay-force
       (match (force x)
         ((None) null)
         ((Some (Tuple h t))
          (cons (f h) (map f t)))))))

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

  (define-instance (Foldable Stream)
    (define (fold f init s)
      (match (force s)
        ((None) init)
        ((Some (Tuple h t))
         (fold f (f init h) t))))
    (define (foldr f init s)
      (match (force s)
        ((None) init)
        ((Some (Tuple h t))
         (f h (foldr f init t))))))

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

  (define-instance (Traversable Stream)
    (define (traverse f s)
      (match (force s)
        ((Some (Tuple h t))
         (liftA2 (fn (x y) (cons x y)) (f h) (traverse f t)))
        ((None) (pure null)))))

  (define-instance (Semigroup (Stream :a))
    (define (<> s1 s2) (append s1 s2)))

  (define-instance (Monoid (Stream :a))
    (define mempty null))

  (define-instance (iter:IntoIterator (Stream :a) :a)
    (define (iter:into-iter s)
      (let cell = (cell:new s))
      (iter:new (fn ((Unit))
                  (match (force (cell:read cell))
                    ((None) None)
                    ((Some (Tuple h t))
                     (cell:write! cell t)
                     (Some h)))))))

  (define-instance (iter:FromIterator (Stream :a) :a)
    (define (iter:collect! iter)
      (into (the (List :a) (iter:collect! iter))))))
