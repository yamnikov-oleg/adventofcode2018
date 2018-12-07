#lang racket
(module+ test
  (require rackunit))

(struct dependency (of on) #:transparent)

(define (parse-dep-line line)
  (match (regexp-match #px"Step (\\w) must be finished before step (\\w) can begin." line)
    [#f (error "Invalid line" line)]
    [(list _ fst snd) (dependency (string-ref snd 0) (string-ref fst 0))]))

(module+ test
  (check-equal? (parse-dep-line "Step C must be finished before step A can begin.")
                (dependency #\A #\C)))

(define (make-dep-map deps)
  (for/fold
      ([dep-map (hash)])
      ([dep deps])
    (let
        ([deps-of-of (hash-ref dep-map (dependency-of dep) (set))]
        [deps-of-on (hash-ref dep-map (dependency-on dep) (set))])
      (hash-set* dep-map (dependency-of dep) (set-add deps-of-of (dependency-on dep))
                         ; Make sure the dependency is present in the hash as a key
                         (dependency-on dep) deps-of-on))))

(module+ test
  (define test-deps
    (list (dependency #\A #\C)
          (dependency #\F #\C)
          (dependency #\B #\A)
          (dependency #\D #\A)
          (dependency #\E #\B)
          (dependency #\E #\D)
          (dependency #\E #\F))))

(module+ test
  (check-equal? (make-dep-map test-deps)
                (hash #\A (set #\C)
                      #\B (set #\A)
                      #\C (set)
                      #\D (set #\A)
                      #\E (set #\B #\D #\F)
                      #\F (set #\C))))

(define (runnable-steps deps-map done-steps)
  (for/set
      ([(step deps) deps-map]
      #:when (and (subset? deps done-steps) (not (set-member? done-steps step))))
    step))

(module+ test
  (define test-dep-map (make-dep-map test-deps)))

(module+ test
  (check-equal? (runnable-steps test-dep-map (set))
                (set #\C))
  (check-equal? (runnable-steps test-dep-map (set #\C))
                (set #\A #\F))
  (check-equal? (runnable-steps test-dep-map (set #\C #\A))
                (set #\B #\D #\F))
  (check-equal? (runnable-steps test-dep-map (set #\C #\A #\B))
                (set #\D #\F))
  (check-equal? (runnable-steps test-dep-map (set #\C #\A #\B #\D))
                (set #\F))
  (check-equal? (runnable-steps test-dep-map (set #\C #\A #\B #\D #\F))
                (set #\E))
  (check-equal? (runnable-steps test-dep-map (set #\C #\A #\B #\D #\F #\E))
                (set)))

(define (run-step deps-map done-list)
  (match (sort (set->list (runnable-steps deps-map (list->set done-list))) char<?)
    [(list) #f]
    [(cons next-step _) (append done-list (list next-step))]))

(module+ test
  (check-equal? (run-step test-dep-map (list))
                (list #\C))
  (check-equal? (run-step test-dep-map (list #\C))
                (list #\C #\A))
  (check-equal? (run-step test-dep-map (list #\C #\A))
                (list #\C #\A #\B))
  (check-equal? (run-step test-dep-map (list #\C #\A #\B))
                (list #\C #\A #\B #\D))
  (check-equal? (run-step test-dep-map (list #\C #\A #\B #\D))
                (list #\C #\A #\B #\D #\F))
  (check-equal? (run-step test-dep-map (list #\C #\A #\B #\D #\F))
                (list #\C #\A #\B #\D #\F #\E))
  (check-equal? (run-step test-dep-map (list #\C #\A #\B #\D #\F #\E))
                #f))

(define (run-seq deps-map)
  (define (run-seq-rec deps-map done-list)
    (match (run-step deps-map done-list)
      [#f done-list]
      [new-done-list (run-seq-rec deps-map new-done-list)]))
  (run-seq-rec deps-map empty))

(module+ test
  (check-equal? (run-seq test-dep-map) (list #\C #\A #\B #\D #\F #\E)))

(define (main in)
  (let*
      ([deps (map parse-dep-line (port->lines in))]
      [deps-map (make-dep-map deps)]
      [steps-seq (run-seq deps-map)])
    (printf "Steps sequence: ~a\n" (list->string steps-seq))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
