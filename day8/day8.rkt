#lang typed/racket
(module+ test
  (require typed/rackunit))

(struct Node
  ([children : (Listof Node)]
   [metadata : (Listof Nonnegative-Integer)])
  #:transparent)

(define-type Numbers (Listof Nonnegative-Integer))

(: parse-node (-> Numbers (Values Node Numbers)))
(define (parse-node input)
  (: parse-nodes (-> Numbers Nonnegative-Integer (Values (Listof Node) Numbers)))
  (define (parse-nodes input n)
    (if (equal? n 0)
        (values empty input)
        (let*-values
            ([(node rest) (parse-node input)]
             [(nodes rest) (parse-nodes rest (cast (- n 1) Nonnegative-Integer))])
          (values (cons node nodes) rest))))
  (match input
    [(list-rest child-num meta-num rest)
      (let*-values
          ([(children rest) (parse-nodes rest child-num)]
           [(meta rest) (split-at rest meta-num)])
        (values (Node children meta) rest))]
    [_ (error "Input is too short")]))

(: parse-tree (-> Numbers Node))
(define (parse-tree input)
  (let-values
      ([(root rest) (parse-node input)])
    (if (empty? rest)
        root
        (error "Input is too long"))))

(module+ test
  (check-equal? (parse-tree '(2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2))
                (Node (list (Node empty '(10 11 12))
                            (Node (list (Node empty '(99)))
                                  '(2)))
                      '(1 1 2))))

(: sum-meta (-> Node Nonnegative-Integer))
(define (sum-meta node)
  (let
      ([children-sum (foldl + 0 (map sum-meta (Node-children node)))]
       [meta-sum (foldl + 0 (Node-metadata node))])
    (+ children-sum meta-sum)))

(module+ test
  (define test-tree
    (Node (list (Node empty '(10 11 12))
                (Node (list (Node empty '(99)))
                      '(2)))
          '(1 1 2))))

(module+ test
  (check-equal? (sum-meta test-tree) 138))

(: node-value (-> Node Nonnegative-Integer))
(define (node-value node)
  (: child-value (-> Nonnegative-Integer Nonnegative-Integer))
  (define (child-value index)
    (if (or (equal? index 0) (> index (length (Node-children node))))
        0
        (node-value (list-ref (Node-children node) (- index 1)))))
  (if (empty? (Node-children node))
      (foldl + 0 (Node-metadata node))
      (foldl + 0 (map child-value (Node-metadata node)))))

(module+ test
  (check-equal? (node-value test-tree) 66))

(: string->noneg-number (-> String Nonnegative-Integer))
(define (string->noneg-number str)
  (cast (string->number str) Nonnegative-Integer))

(: main (-> Input-Port Void))
(define (main in)
  (let*
      ([input (map string->noneg-number (string-split (string-trim (port->string in)) " "))]
       [root (parse-tree input)]
       [checksum (sum-meta root)]
       [root-value (node-value root)])
    (printf "Checksum: ~a\n" checksum)
    (printf "Root node value: ~a\n" root-value)))

(module+ main
  (: input-file-path String)
  (define input-file-path
    (command-line
      #:args (path)
      (cast path String)))
  (call-with-input-file input-file-path main))
