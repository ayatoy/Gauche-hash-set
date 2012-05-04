(use gauche.test)

(test-start "hash-set")

(use extra.set)
(test-module 'extra.set)

(test* "make-hash-set" <hash-set>
       (class-of (make-hash-set)))

(test* "hash-set" <hash-set>
       (class-of (hash-set 'eqv?)))

(test* "hash-set?" #t
       (hash-set? (make-hash-set)))

(test* "hash-set-table" #t
       (hash-table? (hash-set-table (make-hash-set))))

(test* "hash-set-type" 'equal?
       (hash-set-type (make-hash-set 'equal?)))

(test* "hash-set-num-entries" 3
       (hash-set-num-entries (hash-set 'eqv? 1 2 3)))

(test* "hash-set->list" #t
       (let1 lst (hash-set->list (hash-set 'eqv? 1 2 3))
         (and (= 3 (length lst))
              (member 1 lst)
              (member 2 lst)
              (member 3 lst)
              #t)))

(test* "list->hash-set" #t
       (let1 lst (hash-set->list (list->hash-set '(1 2 3) 'eqv?))
         (and (= 3 (length lst))
              (member 1 lst)
              (member 2 lst)
              (member 3 lst)
              #t)))

(test* "hash-set-exists?" #t
       (let1 hs (hash-set 'eqv? 1)
         (and (hash-set-exists? hs 1)
              (not (hash-set-exists? hs 2)))))

(test* "hash-set-delete!" '()
       (let1 hs (hash-set 'eqv? 1)
         (hash-set-delete! hs 1)
         (hash-set->list hs)))

(test* "hash-set-clear!" '()
       (let1 hs (hash-set 'eqv? 1 2 3)
         (hash-set-clear! hs)
         (hash-set->list hs)))

(test* "hash-set<= 1" #t
       (hash-set<= (hash-set 'eqv? 1)
                   (hash-set 'eqv? 1 2)
                   (hash-set 'eqv? 1 2 3)))

(test* "hash-set<= 2" #f
       (hash-set<= (hash-set 'eqv? 1)
                   (hash-set 'eqv? 1 2 3)
                   (hash-set 'eqv? 1 2)))

(test* "hash-set= 1" #t
       (hash-set= (hash-set 'eqv? 1)
                  (hash-set 'eqv? 1)
                  (hash-set 'eqv? 1)))

(test* "hash-set= 2" #f
       (hash-set= (hash-set 'eqv? 1)
                  (hash-set 'eqv? 1 2)
                  (hash-set 'eqv? 1)))

(test* "hash-set-adjoin" #t
       (let* ([hs1 (hash-set 'eqv? 1 2 3)]
              [hs2 (hash-set-adjoin hs1 4 5)])
         (and (= 5 (hash-set-num-entries hs2))
              (not (eq? hs1 hs2)))))

(test* "hash-set-adjoin!" #t
       (let* ([hs1 (hash-set 'eqv? 1 2 3)]
              [hs2 (hash-set-adjoin! hs1 4 5)])
         (and (= 5 (hash-set-num-entries hs2))
              (eq? hs1 hs2))))

(test* "hash-set-union" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-union hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (memv 2 lst)
              (memv 3 lst)
              (not (eq? hs1 hs3)))))

(test* "hash-set-union!" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-union! hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (memv 2 lst)
              (memv 3 lst)
              (eq? hs1 hs3))))

(test* "hash-set-intersection" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-intersection hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (not (memv 1 lst))
              (memv 2 lst)
              (not (memv 3 lst))
              (not (eq? hs1 hs3)))))

(test* "hash-set-intersection!" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-intersection! hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (not (memv 1 lst))
              (memv 2 lst)
              (not (memv 3 lst))
              (eq? hs1 hs3))))

(test* "hash-set-difference" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-difference hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (not (memv 2 lst))
              (not (memv 3 lst))
              (not (eq? hs1 hs3)))))

(test* "hash-set-difference!" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-difference! hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (not (memv 2 lst))
              (not (memv 3 lst))
              (eq? hs1 hs3))))

(test* "hash-set-xor" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-xor hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (not (memv 2 lst))
              (memv 3 lst)
              (not (eq? hs1 hs3)))))

(test* "hash-set-xor!" #t
       (let* ([hs1 (hash-set 'eqv? 1 2)]
              [hs2 (hash-set 'eqv? 2 3)]
              [hs3 (hash-set-xor! hs1 hs2)]
              [lst (hash-set->list hs3)])
         (and (memv 1 lst)
              (not (memv 2 lst))
              (memv 3 lst)
              (eq? hs1 hs3))))

(test* "hash-set-for-each" #t
       (let1 lst '()
         (hash-set-for-each (hash-set 'eqv? 1 2 3) (^[x] (push! lst x)))
         (and (memv 1 lst)
              (memv 2 lst)
              (memv 3 lst)
              #t)))

(test* "hash-set-map" #t
       (let1 lst (hash-set-map (hash-set 'eqv? 1 2 3) (^[x] x))
         (and (memv 1 lst)
              (memv 2 lst)
              (memv 3 lst)
              #t)))

(test* "hash-set-fold" 6
       (hash-set-fold (hash-set 'eqv? 1 2 3) + 0))

(test* "hash-set-jaccard 1" 1/2
       (hash-set-jaccard (hash-set 'eqv? 1 2 3) (hash-set 'eqv? 2 3 4)))

(test* "hash-set-jaccard 2" +nan.0
       (hash-set-jaccard (hash-set 'eqv?) (hash-set 'eqv?)))

(test* "hash-set-jaccard 3" 1
       (hash-set-jaccard (hash-set 'eqv?) (hash-set 'eqv?) 1))

(test* "hash-set-dice 1" 2/3
       (hash-set-dice (hash-set 'eqv? 1 2 3) (hash-set 'eqv? 2 3 4)))

(test* "hash-set-dice 2" +nan.0
       (hash-set-dice (hash-set 'eqv?) (hash-set 'eqv?)))

(test* "hash-set-dice 3" 1
       (hash-set-dice (hash-set 'eqv?) (hash-set 'eqv?) 1))

(test* "hash-set-simpson 1" 2/3
       (hash-set-simpson (hash-set 'eqv? 1 2 3) (hash-set 'eqv? 2 3 4)))

(test* "hash-set-simpson 2" +nan.0
       (hash-set-simpson (hash-set 'eqv? 1 2 3) (hash-set 'eqv?)))

(test* "hash-set-simpson 3" 0
       (hash-set-simpson (hash-set 'eqv? 1 2 3) (hash-set 'eqv?) 0))

(test-end)
