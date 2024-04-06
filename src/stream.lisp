(defpackage #:tokyo.tojo.lazy/stream
  (:use #:coalton
        #:coalton-prelude)
  (:shadow #:Cons
           #:Nil
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
   (#:cell #:coalton-library/cell)
   (#:lazy #:tokyo.tojo.lazy/lazy))
  (:export #:Stream
           #:Cons
           #:Nil
           #:make
           #:force
           #:delay-force
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

(cl:defmacro make (cl:&rest xs)
  (cl:if (cl:consp xs)
         `(lazy:delay (Cons ,(cl:first xs)
                            (make ,@(cl:rest xs))))
         '(lazy:delay Nil)))

(coalton-toplevel
  (define-type (Stream :a)
    (Cons :a (lazy:Lazy Stream :a))
    Nil)

  (declare head (lazy:Lazy Stream :a -> Optional :a))
  (define (head s)
    (match (lazy:force s)
      ((Cons h _) (Some h))
      ((Nil) None)))

  (declare tail (lazy:Lazy Stream :a -> Optional (lazy:Lazy Stream :a)))
  (define (tail s)
    (match (lazy:force s)
      ((Cons _ t) (Some t))
      ((Nil) None)))

  (declare index (UFix -> lazy:Lazy Stream :a -> Optional :a))
  (define (index idx s)
    (match (lazy:force s)
      ((Cons h t)
       (if (== idx 0)
           (Some h)
           (index (1- idx) t)))
      ((Nil) None)))

  (declare append (lazy:Lazy Stream :a -> lazy:Lazy Stream :a -> lazy:Lazy Stream :a))
  (define (append s1 s2)
    (lazy:delay-force
     (match (lazy:force s1)
       ((Nil) s2)
       ((Cons h t)
        (lazy:delay (Cons h (append t s2)))))))

  (declare concat ((lazy:Lazy Stream (lazy:Lazy Stream :a)) -> (lazy:Lazy Stream :a)))
  (define (concat s)
    (concat-map id s))

  (declare concat-map ((:a -> lazy:Lazy Stream :b) -> lazy:Lazy Stream :a -> lazy:Lazy Stream :b))
  (define (concat-map f s)
    (lazy:delay-force
     (match (lazy:force s)
       ((Nil) (lazy:delay Nil))
       ((Cons h t)
        (append (f h) (concat-map f t))))))

  (declare take (UFix -> (lazy:Lazy Stream :a) -> (lazy:Lazy Stream :a)))
  (define (take n s)
    (lazy:delay-force
     (if (<= n 0)
         (lazy:delay Nil)
         (match (lazy:force s)
           ((Nil) (lazy:delay Nil))
           ((Cons h t)
            (lazy:delay (Cons h (take (1- n) t))))))))

  (declare drop (UFix -> (lazy:Lazy Stream :a) -> (lazy:Lazy Stream :a)))
  (define (drop n s)
    (if (<= n 0)
        s
        (match (lazy:force s)
          ((Nil) (lazy:delay Nil))
          ((Cons _h t)
           (drop (1- n) t)))))

  (declare take-while ((:a -> Boolean) -> (lazy:Lazy Stream :a) -> (lazy:Lazy Stream :a)))
  (define (take-while p? s)
    (lazy:delay-force
     (match (lazy:force s)
       ((Nil) (lazy:delay Nil))
       ((Cons h t)
        (if (p? h)
            (lazy:delay (Cons h (take-while p? t)))
            (lazy:delay Nil))))))

  (declare drop-while ((:a -> Boolean) -> lazy:Lazy Stream :a -> lazy:Lazy Stream :a))
  (define (drop-while p? s)
    (lazy:delay-force
     (match (lazy:force s)
       ((Nil) (lazy:delay Nil))
       ((Cons h t)
        (if (p? h)
            (drop-while p? t)
            s)))))

  (declare zip-with ((:a -> :b -> :c)
                     -> lazy:Lazy Stream :a
                     -> lazy:Lazy Stream :b
                     -> lazy:Lazy Stream :c))
  (define (zip-with f s1 s2)
    (lazy:delay-force
     (match (lazy:force s1)
       ((Nil) (lazy:delay Nil))
       ((Cons h1 t1)
        (match (lazy:force s2)
          ((Nil) (lazy:delay Nil))
          ((Cons h2 t2)
           (lazy:delay (Cons (f h1 h2)
                             (zip-with f t1 t2)))))))))

  (declare iterate ((:a -> :a) -> :a -> lazy:Lazy Stream :a))
  (define (iterate f x)
    (lazy:delay (Cons x (iterate f (f x)))))

  (declare repeat (:a -> lazy:Lazy Stream :a))
  (define repeat (iterate id))

  (declare length (lazy:Lazy Stream :a -> UFix))
  (define (length s)
    (fold (fn (acc _) (1+ acc)) 0 s))

  (declare filter ((:a -> Boolean) -> lazy:Lazy Stream :a -> lazy:Lazy Stream :a))
  (define (filter p? s)
    (lazy:delay-force
     (match (lazy:force s)
       ((Nil) (lazy:delay Nil))
       ((Cons h t)
        (if (p? h)
            (lazy:delay (Cons h (filter p? t)))
            (filter p? t))))))

  (define-instance (Eq :a => Eq (lazy:Lazy Stream :a))
    (define (== s1 s2)
      (match (lazy:force s1)
        ((Cons h1 t1)
         (match (lazy:force s2)
           ((Cons h2 t2)
            (and (== h1 h2)
                 (== (lazy:force t1) (lazy:force t2))))
           (_ False)))
        ((Nil)
         (match (lazy:force s2)
           ((Nil) True)
           (_ False))))))

  (define-instance (Ord :a => Ord (lazy:Lazy Stream :a))
    (define (<=> s1 s2)
      (match (lazy:force s1)
        ((Cons h1 t1)
         (match (lazy:force s2)
           ((Cons h2 t2)
            (match (<=> h1 h2)
              ((LT) LT)
              ((GT) GT)
              ((EQ) (<=> (lazy:force t1)
                         (lazy:force t2)))))
           (_ GT)))
        ((Nil)
         (match (lazy:force s2)
           ((Nil) EQ)
           (_ LT))))))

  (define-instance (Functor (lazy:Lazy Stream))
    (define (map f s)
      (match (lazy:force s)
        ((Nil) (lazy:delay Nil))
        ((Cons h t)
         (lazy:delay
          (Cons (f h) (map f t)))))))

  (define-instance (Applicative (lazy:Lazy Stream))
    (define (pure x) (make x))
    (define (liftA2 op s1 s2)
      (concat-map (fn (x) (map (op x) s2))
                  s1)))

  (define-instance (Monad (lazy:Lazy Stream))
    (define (>>= m f)
      (concat-map f m)))

  (define-instance (Alternative (lazy:Lazy Stream))
    (define (alt s1 s2)
      (append s1 s2))
    (define empty (make)))

  (define-instance (Foldable (lazy:Lazy Stream))
    (define (fold f init s)
      (match (lazy:force s)
        ((Nil) init)
        ((Cons h t)
         (fold f (f init h) t))))
    (define (foldr f init s)
      (match (lazy:force s)
        ((Nil) init)
        ((Cons h t)
         (f h (foldr f init t))))))

  (define-instance (Into (lazy:Lazy Stream :a) (List :a))
    (define (into s)
      (match (lazy:force s)
        ((Nil) coalton:Nil)
        ((Cons h t)
         (coalton:Cons h (into t))))))

  (define-instance (Into (List :a) (lazy:Lazy Stream :a))
    (define (into lst)
      (match lst
        ((coalton:Nil) (make))
        ((coalton:Cons h t)
         (lazy:delay (Cons h (into t)))))))

  (define-instance (Traversable (lazy:Lazy Stream))
    (define (traverse f s)
      (match (lazy:force s)
        ((Cons h t)
         (liftA2 (fn (x y) (lazy:delay (Cons x y)))
                 (f h)
                 (traverse f t)))
        ((Nil) (pure (make))))))

  (define-instance (Semigroup (lazy:Lazy Stream :a))
    (define (<> s1 s2) (append s1 s2)))

  (define-instance (Monoid (lazy:Lazy Stream :a))
    (define mempty (make)))

  (define-instance (iter:IntoIterator (lazy:Lazy Stream :a) :a)
    (define (iter:into-iter s)
      (let cell = (cell:new s))
      (iter:new (fn ((Unit))
                  (match (lazy:force (cell:read cell))
                    ((Nil) None)
                    ((Cons h t)
                     (cell:write! cell t)
                     (Some h)))))))

  (define-instance (iter:FromIterator (lazy:Lazy Stream :a) :a)
    (define (iter:collect! iter)
      (into (the (List :a) (iter:collect! iter))))))
