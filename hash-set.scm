(define-module hash-set
  (use gauche.record)
  (export <hash-set>
          hash-set?
          hash-set-table
          make-hash-set
          hash-set-type
          hash-set-num-entries
          hash-set
          hash-set->list
          list->hash-set
          hash-set->vector
          vector->hash-set
          hash-set-exists?
          hash-set-delete!
          hash-set-clear!
          hash-set<=
          hash-set=
          hash-set-adjoin
          hash-set-adjoin!
          hash-set-union
          hash-set-union!
          hash-set-intersection
          hash-set-intersection!
          hash-set-difference
          hash-set-difference!
          hash-set-xor
          hash-set-xor!
          hash-set-for-each
          hash-set-map
          hash-set-fold
          hash-set-jaccard
          hash-set-dice
          hash-set-simpson))
(select-module hash-set)

(define-record-type <hash-set> %make-hash-set hash-set?
  (table hash-set-table))

(define-method write-object ((hs <hash-set>) oport)
  (format oport "#<hash-set ~s ~s>"
          (hash-set-type hs)
          (hash-set-num-entries hs)))

(define (make-hash-set :optional (type 'eq?))
  (%make-hash-set (make-hash-table type)))

(define (hash-set-type hs)
  (hash-table-type (hash-set-table hs)))

(define (hash-set-num-entries hs)
  (hash-table-num-entries (hash-set-table hs)))

(define (hash-set type . els)
  (%make-hash-set
   (rlet1 ht (make-hash-table type)
     (for-each (^[el] (hash-table-put! ht el (undefined))) els))))

(define (hash-set->list hs)
  (hash-table-keys (hash-set-table hs)))

(define (list->hash-set lst :optional (type 'eq?))
  (apply hash-set type lst))

(define (hash-set->vector hs)
  (list->vector (hash-set->list hs)))

(define (vector->hash-set vec :optional (type 'eq?))
  (list->hash-set (vector->list vec) type))

(define (hash-set-exists? hs el)
  (hash-table-exists? (hash-set-table hs) el))

(define (hash-set-delete! hs el)
  (hash-table-delete! (hash-set-table hs) el))

(define (hash-set-clear! hs)
  (hash-table-clear! (hash-set-table hs)))

(define (hash-set<= . hsets)
  (or (>= 1 (length hsets))
      (and (let1 ht (hash-set-table (cadr hsets))
             (let loop ([ks (hash-table-keys (hash-set-table (car hsets)))])
               (or (null? ks)
                   (and (hash-table-exists? ht (car ks))
                        (loop (cdr ks))))))
           (apply hash-set<= (cdr hsets)))))

(define (hash-set= . hsets)
  (let1 s (car hsets)
    (let loop ([rest (cdr hsets)])
      (or (null? rest)
          (and (let1 s1 (car rest)
                 (and (hash-set<= s s1) (hash-set<= s1 s)))
               (loop (cdr rest)))))))

(define (hash-set-adjoin hs . els)
  (let* ([ht  (hash-set-table hs)]
         [ht1 (make-hash-table (hash-table-type ht))])
    (hash-table-for-each ht (^[k v] (hash-table-put! ht1 k (undefined))))
    (for-each (^[el] (hash-table-put! ht1 el (undefined))) els)
    (%make-hash-set ht1)))

(define (hash-set-adjoin! hs . els)
  (let1 ht (hash-set-table hs)
    (for-each (^[el] (hash-table-put! ht el (undefined))) els))
  hs)

(define (hash-set-union . hsets)
  (let* ([ht  (hash-set-table (car hsets))]
         [ht1 (make-hash-table (hash-table-type ht))])
    (hash-table-for-each ht (^[k v] (hash-table-put! ht1 k (undefined))))
    (for-each (^[hs] (hash-table-for-each
                      (hash-set-table hs)
                      (^[k v] (hash-table-put! ht1 k (undefined)))))
              (cdr hsets))
    (%make-hash-set ht1)))

(define (hash-set-union! . hsets)
  (rlet1 hs (car hsets)
    (let1 ht (hash-set-table hs)
      (for-each (^[hs] (hash-table-for-each
                        (hash-set-table hs)
                        (^[k v] (hash-table-put! ht k (undefined)))))
                (cdr hsets)))))

(define (hash-set-intersection . hsets)
  (let* ([ht  (hash-set-table (car hsets))]
         [ht1 (make-hash-table (hash-table-type ht))])
    (hash-table-for-each ht (^[k v] (hash-table-put! ht1 k (undefined))))
    (for-each (^[hs] (let1 ht (hash-set-table hs)
                       (hash-table-for-each
                        ht1
                        (^[k v] (unless (hash-table-exists? ht k)
                                  (hash-table-delete! ht1 k))))))
              (cdr hsets))
    (%make-hash-set ht1)))

(define (hash-set-intersection! . hsets)
  (rlet1 hs (car hsets)
    (let1 ht (hash-set-table hs)
      (for-each (^[hs] (let1 ht1 (hash-set-table hs)
                         (hash-table-for-each
                          ht
                          (^[k v] (unless (hash-table-exists? ht1 k)
                                    (hash-table-delete! ht k))))))
                (cdr hsets)))))

(define (hash-set-difference . hsets)
  (let* ([ht  (hash-set-table (car hsets))]
         [ht1 (make-hash-table (hash-table-type ht))])
    (hash-table-for-each ht (^[k v] (hash-table-put! ht1 k (undefined))))
    (for-each (^[hs] (let1 ht (hash-set-table hs)
                       (hash-table-for-each
                        ht1
                        (^[k v] (when (hash-table-exists? ht k)
                                  (hash-table-delete! ht1 k))))))
              (cdr hsets))
    (%make-hash-set ht1)))

(define (hash-set-difference! . hsets)
  (rlet1 hs (car hsets)
    (let1 ht (hash-set-table hs)
      (for-each (^[hs] (let1 ht1 (hash-set-table hs)
                         (hash-table-for-each
                          ht
                          (^[k v] (when (hash-table-exists? ht1 k)
                                    (hash-table-delete! ht k))))))
              (cdr hsets)))))

(define (hash-set-xor . hsets)
  (let* ([ht  (hash-set-table (car hsets))]
         [ht1 (make-hash-table (hash-table-type ht))])
    (hash-table-for-each ht (^[k v] (hash-table-put! ht1 k (undefined))))
    (for-each (^[hs] (hash-table-for-each
                      (hash-set-table hs)
                      (^[k v] (if (hash-table-exists? ht1 k)
                                (hash-table-delete! ht1 k)
                                (hash-table-put! ht1 k (undefined))))))
              (cdr hsets))
    (%make-hash-set ht1)))

(define (hash-set-xor! . hsets)
  (rlet1 hs (car hsets)
    (let1 ht (hash-set-table hs)
      (for-each (^[hs] (hash-table-for-each
                        (hash-set-table hs)
                        (^[k v] (if (hash-table-exists? ht k)
                                  (hash-table-delete! ht k)
                                  (hash-table-put! ht k (undefined))))))
              (cdr hsets)))))

(define (hash-set-for-each hs proc)
  (hash-table-for-each (hash-set-table hs) (^[k v] (proc k))))

(define (hash-set-map hs proc)
  (hash-table-map (hash-set-table hs) (^[k v] (proc k))))

(define (hash-set-fold hs kons knil)
  (hash-table-fold (hash-set-table hs) (^[k v ac] (kons k ac)) knil))

(define (hash-set-jaccard hs1 hs2 :optional if-zero)
  (let1 d (hash-set-num-entries (hash-set-union hs1 hs2))
    (if (and (= d 0) (not (undefined? if-zero)))
      if-zero
      (/ (hash-set-num-entries (hash-set-intersection hs1 hs2))
         d))))

(define (hash-set-dice hs1 hs2 :optional if-zero)
  (let1 d (+ (hash-set-num-entries hs1) (hash-set-num-entries hs2))
    (if (and (= d 0) (not (undefined? if-zero)))
      if-zero
      (/ (* 2 (hash-set-num-entries (hash-set-intersection hs1 hs2)))
         d))))

(define (hash-set-simpson hs1 hs2 :optional if-zero)
  (let1 d (min (hash-set-num-entries hs1) (hash-set-num-entries hs2))
    (if (and (= d 0) (not (undefined? if-zero)))
      if-zero
      (/ (hash-set-num-entries (hash-set-intersection hs1 hs2))
         d))))
