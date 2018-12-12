#lang racket
(module+ test
  (require rackunit))

;;; `on` must be finished before `of` can begin.
(struct dependency (of on) #:transparent)

(define (parse-dep-line line)
  (match (regexp-match #px"Step (\\w) must be finished before step (\\w) can begin." line)
    [#f (error "Invalid line" line)]
    [(list _ fst snd) (dependency (string-ref snd 0) (string-ref fst 0))]))

(module+ test
  (check-equal? (parse-dep-line "Step C must be finished before step A can begin.")
                (dependency #\A #\C)))

;;; Takes a list of `depedency` structs and returns dependency map:
;;; a hash which maps every step to a set of other steps it depends on.
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

;;; Returns a list of steps which can be currently run according
;;; to the depedency map and a set of finished steps.
(define (steps-to-run deps-map done-set)
  (define (runnable-steps deps-map done-steps)
    (for/set
        ([(step deps) deps-map]
        #:when (and (subset? deps done-steps) (not (set-member? done-steps step))))
      step))
  (sort (set->list (runnable-steps deps-map done-set)) char<?))

(module+ test
  (define test-dep-map (make-dep-map test-deps)))

(module+ test
  (check-equal? (steps-to-run test-dep-map (set))
                (list #\C))
  (check-equal? (steps-to-run test-dep-map (set #\C))
                (list #\A #\F))
  (check-equal? (steps-to-run test-dep-map (set #\C #\A))
                (list #\B #\D #\F))
  (check-equal? (steps-to-run test-dep-map (set #\C #\A #\B))
                (list #\D #\F))
  (check-equal? (steps-to-run test-dep-map (set #\C #\A #\B #\D))
                (list #\F))
  (check-equal? (steps-to-run test-dep-map (set #\C #\A #\B #\D #\F))
                (list #\E))
  (check-equal? (steps-to-run test-dep-map (set #\C #\A #\B #\D #\F #\E))
                (list)))

;;; Runs the next available step and returns the new list of finished steps
;;; (done-list with the last step appended to it).
;;; If no step can be run, returns #f.
(define (run-step deps-map done-list)
  (match (steps-to-run deps-map (list->set done-list))
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

;;; Completes all the steps and returns the list of these steps in the order
;;; of running.
(define (run-seq deps-map)
  (define (run-seq-rec deps-map done-list)
    (match (run-step deps-map done-list)
      [#f done-list]
      [new-done-list (run-seq-rec deps-map new-done-list)]))
  (run-seq-rec deps-map empty))

(module+ test
  (check-equal? (run-seq test-dep-map) (list #\C #\A #\B #\D #\F #\E)))

;;; Returns time required to finish the step which equals to its number
;;; in the English alphabet (1..26) + `base`.
(define (step-time step [base 60])
  (+ (char->integer step)
     (* -1 (char->integer #\A))
     base
     1))

(module+ test
  (check-equal? (step-time #\A) 61)
  (check-equal? (step-time #\Z) 86)
  (check-equal? (step-time #\Z 0) 26))

(struct task (step finish-time) #:transparent)

;;; Make a new vector of worker queues. Each queue is a list of `task`
;;; in the order from last to the first.
(define (empty-worker-queues worker-cnt)
  (make-vector worker-cnt (list)))

;;; Returns a set of steps completed by workers at the given time.
(define (done-steps queues cur-time)
  (for/fold
      ([steps (set)])
      ([queue queues])
    (for/fold
        ([steps steps])
        ([task queue]
        #:when (<= (task-finish-time task) cur-time))
      (set-add steps (task-step task)))))

(module+ test
  (define test-queues (vector (list (task #\B 12) (task #\A 6))
                              (list (task #\C 9)))))

(module+ test
  (check-equal? (done-steps test-queues 0) (set))
  (check-equal? (done-steps test-queues 5) (set))
  (check-equal? (done-steps test-queues 6) (set #\A))
  (check-equal? (done-steps test-queues 7) (set #\A))
  (check-equal? (done-steps test-queues 9) (set #\A #\C))
  (check-equal? (done-steps test-queues 12) (set #\A #\B #\C)))

;;; Returns a set of steps ever assigned to the workers with the given queues.
(define (assigned-steps queues)
  (for/fold
      ([steps (set)])
      ([queue queues])
    (for/fold
        ([steps steps])
        ([task queue])
      (set-add steps (task-step task)))))

(module+ test
  (check-equal? (assigned-steps test-queues) (set #\A #\B #\C)))

;;; Returns #t if a worker with given queue can be assigned new task at the time.
(define (worker-available? queue cur-time)
  (for/and
    ([t queue])
    (<= (task-finish-time t) cur-time)))

(module+ test
  (check-equal? (worker-available? (vector-ref test-queues 0) 8)
                #f)
  (check-equal? (worker-available? (vector-ref test-queues 0) 12)
                #t)
  (check-equal? (worker-available? (vector-ref test-queues 1) 8)
                #f)
  (check-equal? (worker-available? (vector-ref test-queues 1) 9)
                #t)
  (check-equal? (worker-available? (vector-ref test-queues 1) 12)
                #t))

;;; Returns the first available worker's index in the given time or #f
;;; if not workers are available.
(define (available-worker queues cur-time)
  (for/first
      ([queue queues]
      [ix (in-naturals)]
      #:when (worker-available? queue cur-time))
    ix))

;;; I couldn't find immutable version of `vector-set!` function.
(define (vector-set vec ix val)
  (for/vector
      ([cur-val vec]
      [cur-ix (in-naturals)])
    (if (equal? cur-ix ix)
        val
        cur-val)))

;;; Assigns the task of completing the given step starting at `cur-time`
;;; to the first available worker in the given worker queues.
;;; If not worker is available returns worker queues unchanged.
;;; Optional arg step-time-base is used to passed to `step-time`.
(define (maybe-assign-work queues step cur-time [step-time-base 60])
  (match (available-worker queues cur-time)
    [#f queues]
    [worker
      (let*
          ([old-queue (vector-ref queues worker)]
          [new-queue (cons (task step (+ cur-time (step-time step step-time-base))) old-queue)])
        (vector-set queues worker new-queue))]))

(module+ test
  (check-equal? (maybe-assign-work test-queues #\D 8)
                (vector (list (task #\B 12) (task #\A 6))
                        (list (task #\C 9))))
  (check-equal? (maybe-assign-work test-queues #\D 9)
                (vector (list (task #\B 12) (task #\A 6))
                        (list (task #\D 73) (task #\C 9))))
  (check-equal? (maybe-assign-work test-queues #\D 10)
                (vector (list (task #\B 12) (task #\A 6))
                        (list (task #\D 74) (task #\C 9))))
  (check-equal? (maybe-assign-work test-queues #\D 12)
                (vector (list (task #\D 76) (task #\B 12) (task #\A 6))
                        (list (task #\C 9))))
  (check-equal? (maybe-assign-work test-queues #\D 12 0)
                (vector (list (task #\D 16) (task #\B 12) (task #\A 6))
                        (list (task #\C 9)))))

;;; Runs all the tasks in the `deps-map` with the given number of workers
;;; and returns their queues after all the steps have been completed.
;;; Optional step-time-base is passed to `maybe-assign-work`.
(define (run-par deps-map worker-cnt [step-time-base 60])
  (for*/fold
      ([queues (empty-worker-queues worker-cnt)])
      ([cur-time (in-naturals)]
      #:break (equal? (set-count (done-steps queues cur-time))
                      (length (hash-keys deps-map)))
      [step (steps-to-run deps-map (done-steps queues cur-time))]
      #:unless (set-member? (assigned-steps queues) step))
    (maybe-assign-work queues step cur-time step-time-base)))

(module+ test
  (check-equal? (run-par test-dep-map 2 0)
                (vector (list (task #\E 15) (task #\D 10) (task #\B 6) (task #\A 4) (task #\C 3))
                        (list (task #\F 9)))))

;;; Maps every worker queue to the time when last task in the queue is completed.
(define (finish-times queues)
  (for/vector
      ([queue queues])
    (match queue
      [(list) 0]
      [(cons t _) (task-finish-time t)])))

;;; Gets the time at which all the tasks in worker queues are finished.
(define (work-finished-at queues)
  (for/fold
      ([fin-time #f])
      ([time (finish-times queues)])
    (cond
      [(false? fin-time) time]
      [(> time fin-time) time]
      [else fin-time])))

(module+ test
  (check-equal? (work-finished-at (run-par test-dep-map 2 0)) 15))

(define (main in worker-cnt step-time-base)
  (let*
      ([deps (map parse-dep-line (port->lines in))]
      [deps-map (make-dep-map deps)]
      [steps-seq (run-seq deps-map)]
      [work-queues (run-par deps-map worker-cnt step-time-base)]
      [finished-at (work-finished-at work-queues)])
    (printf "Steps sequence: ~a\n" (list->string steps-seq))
    (printf "With ~a workers the processed is finished in ~a seconds.\n" worker-cnt finished-at)))

(module+ main
  (define worker-cnt (make-parameter 2))
  (define step-time-base (make-parameter 0))
  (define (require-number s)
    (or (string->number s)
        (error "Invalid number" s)))
  (define input-file-path
    (command-line
      #:program "day7"
      #:once-each
      [("-w" "--workers") cnt
                          "Number of workers (2)"
                          (worker-cnt (require-number cnt))]
      [("-t" "--step-time-base") base
                                 "The basic step duration (0)"
                                 (step-time-base (require-number base))]
      #:args (path)
      path))
  (call-with-input-file input-file-path (lambda (in) (main in (worker-cnt) (step-time-base)))))
