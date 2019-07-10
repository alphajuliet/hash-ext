#lang racket

; hash-ext package
; Calculations over hash maps
; AndrewJ 2018-08-31

(provide hash-add
         hash-sub
         hash-mul
         hash-sum
         hash-combine
         my-hash-map
         hash-intersection
         hash-dotp
         hash-scale
         hash-apply-factor
         hash-enumerate
         hash-collect)

(require racket/hash)

; Return the intersection of a list
;(: intersection (-> (Listof Any) (Listof Any) (Listof Any)))
(define (intersection l1 l2)
  (remove-duplicates
   (filter (λ (x) (member x l1))  
           l2)))

; Convert from a mutable to immutable hash
;(: mut->immut (-> HashTableTop HashTableTop))
(define (mut->immut h)
  (apply hash (flatten (hash->list h))))

; Create the missing hash-intersection function, but limited to two hashes
;(: hash-intersection (-> HSN HSN [#:combine/key (-> Any Number Number Number)] HSN))
(define (hash-intersection
         #:combine/key [combine/key (λ (k x y) (* x y))]
         h1 h2)
  (for/hash ([k (in-list (intersection (hash-keys h1) (hash-keys h2)))])
    (values k (combine/key k (hash-ref h1 k) (hash-ref h2 k)))))

; Generalised hash combine function using union/intersection and a function
; f is typically hash-union or hash-intersection
; g is the function applied over the values 
(define (hash-combine f g h1 h2)
  (f h1 h2 #:combine/key (λ (k v1 v2) (g v1 v2))))

; Add the values in two hashes where the keys match. For unmatched keys, include the value.
;   (hash-add (hash 'a 1 'b 2 'c 3) (hash 'a 4 'b 5)) => (hash 'a 5 'b 7 'c 3)
;(: hash-add (-> HSN HSN HSN))
(define hash-add
  (curry hash-combine hash-union +))

; Subtract the values in two hashes where the keys match. For unmatched keys, include the value.
;   (hash-sub (hash 'a 4 'b 5 'c 1) (hash 'a 1 'b 2)) => (hash 'a 3 'b 3 'c 1)
;(: hash-sub (-> HSN HSN HSN))
(define hash-sub
  (curry hash-combine hash-union -))

; Multiply the values in two hashes, matched by key. For unmatched keys, don't include the product.
;   (hash-add (hash 'a 1 'b 2 'c 3) (hash 'a 4 'b 5)) => (hash 'a 4 'b 8)
;(: hash-mul (-> HSN HSN HSN))
(define hash-mul
  (curry hash-combine hash-intersection *))

; Add all the values together in a hash
;   (hash-sum (hash 'a 1 'b 2)) => 3
;(: hash-sum (-> HSN Integer))
(define (hash-sum h)
  (foldl + 0 (hash-values h)))

; Do mapping over all values in a hash and return a new hash. `hash-map` only returns a list.
;(: my-hash-map (-> (-> Any Number) HSN))
(define (my-hash-map f h)
  (define hnew (make-hash))
  (for ([(key value) (in-hash h)])
    (hash-set! hnew key (f value)))
  (mut->immut hnew))

; Dot product over two hashes. 
;(: hash-dotp (-> HSN HSN HSN))
(define hash-dotp (compose hash-sum hash-mul))

; Scale all items in a hash by a generic scaling factor 
(define (hash-scale n h)
  (my-hash-map (curry * n) h))

; Selectively scale items in a hash
(define (hash-apply-factor f h)
  (hash-union f h #:combine/key (λ (k v1 v2) (* v1 v2))))

; Enumerate a hash into a list
(define (hash-enumerate h)
  (for*/list ([k (in-hash-keys h)]
              [i (range 0 (hash-ref h k))])
    k))

; Collect a list into a hash of counts
(define (hash-collect lst)
  (apply hash (flatten (map (λ (x) `(,(car x) ,(length x)))
                            (group-by identity lst)))))

;---------------------------
; Unit tests

(module+ test

  (require rackunit
           rackunit/text-ui)

  (define-test-suite hash-ext-tests

    (test-case "Test hash-add"
               (let ([h1 (hash 'a 1 'b 2)]
                     [h2 (hash 'a 3 'b 4)]
                     [h3 (hash 'a 5 'b 6 'c 7)])
                 (check-equal? (hash-add h1 h2)
                               (hash 'a 4 'b 6))
                 (check-equal? (hash-add h1 h3)
                               (hash 'a 6 'b 8 'c 7))))

    (test-case "Test hash-sub"
               (let ([h1 (hash 'a 1 'b 2)]
                     [h2 (hash 'a 3 'b 4)]
                     [h3 (hash 'a 5 'b 6 'c 7)])
                 (check-equal? (hash-sub h2 h1)
                               (hash 'a 2 'b 2))
                 (check-equal? (hash-sub h3 h1)
                               (hash 'a 4 'b 4 'c 7))))
    
    (test-case "Test hash-mul"
               (let ([h1 (hash 'a 1 'b 2)]
                     [h2 (hash 'a 3 'b 4)]
                     [h3 (hash 'a 5 'b 6 'c 7)])
                 (check-equal? (hash-mul h1 h2)
                               (hash 'a 3 'b 8))
                 (check-equal? (hash-mul h1 h3)
                               (hash 'a 5 'b 12))))
    
    (test-case "Test hash-sum"
               (let ([h1 (hash 'a 1 'b 2 'c 3)])
                 (check-equal? (hash-sum h1)
                               6)))

    (test-case "Test hash-map"
               (let ([h1 (hash 'a 1 'b 2)]
                     [f (λ (x) (* x 2))])
                 (check-equal? (my-hash-map f h1)
                               (hash 'a 2 'b 4))))

    (test-case "Test hash-dotp"
               (let ([h1 (hash 'a 1 'b 2)]
                     [h2 (hash 'a 3 'b 4)])
                 (check-equal? (hash-dotp h1 h2)
                               11)))

    (test-case "Test hash-intersection"
               (let ([h1 (hash 'a 1 'c 3)]
                     [h2 (hash 'a 4 'b 5 'c 6)]
                     [h3 (hash 'a 7 'b 8)])
                 (check-equal? (hash-intersection h1 h2)
                               (hash 'a 4 'c 18))
                 (check-equal? (hash-intersection h2 h3)
                               (hash 'a 28 'b 40))))

    (test-case "Test hash scale"
               (let ([h1 (hash 'a 1 'b 2)])
                 (check-equal? (hash-scale 3 h1)
                               (hash 'a 3 'b 6))))

    ;  (test-case "Test applying a factor"
    ;             (let ([h1 (hash 'a 1 'b 2 'c 3)]
    ;                   [f1 (hash 'a 2.0 'c 0.5)]
    ;                   [f2 (hash 'd 3.0)])
    ;               (check-equal? (hash-apply-factor f1 h1)
    ;                             (hash 'a 2.0 'b 2 'c 1.5))
    ;               (check-equal? (hash-apply-factor f2 h1)
    ;                             (hash 'a 1 'b 2 'c 3))))

    (test-case "Test hash enumerate"
               (let ([h1 (hash 'a 1 'b 2)])
                 (check-equal? (length (hash-enumerate h1)) 3)))

    (test-case "Test hash collect"
               (let ([lst '(a b b c c c)])
                 (check-equal? (hash-collect lst)
                                (hash 'a 1 'b 2 'c 3))))
    
    )
  (run-tests hash-ext-tests))
; The End
