#lang racket
(module+ test
  (require rackunit))

;;; Converts a list of string IDs into a hash map which maps ID pattern into
;;; a set of IDs which match this pattern.
;;; E.g. patterns for "abc" are "*bc", "a*c" and "ab*".
;;; Patterns for "abd" are "*bd", "a*d" and "ab*".
;;; (list "abc" "abd") would be conveted into
;;; (hash
;;;   "*bc" (set "abc")
;;;   "a*c" (set "abc")
;;;   "ab*" (set "abc" "abd")
;;;   "*bd" (set "abd")
;;;   "a*d" (set "abd"))
(define (index-partial-matches ids)
  (define (make-patterns id)
    (for/list ([i (in-range (string-length id))])
      (string-append
        (substring id 0 i)
        "*"
        (substring id (+ 1 i)))))
  (define (index-patterns id hsh)
    (define (put-pattern pattern hsh)
      (let*
          ([old-lst (hash-ref hsh pattern (set))]
          [new-lst (set-add old-lst id)])
        (hash-set hsh pattern new-lst)))
    (foldl put-pattern hsh (make-patterns id)))
  (foldl index-patterns (hash) ids))

(module+ test
  (check-equal? (index-partial-matches (list "abc"))
                (hash "*bc" (set "abc")
                      "a*c" (set "abc")
                      "ab*" (set "abc")))
  (check-equal? (index-partial-matches (list "abc" "abd"))
                (hash "*bc" (set "abc")
                      "a*c" (set "abc")
                      "ab*" (set "abc" "abd")
                      "*bd" (set "abd")
                      "a*d" (set "abd"))))

;;; Finds a set of IDs which differ only by one letter in the list of ids.
;;; Returns their pattern and the set of them.
;;; E.g. for (list "abc" "abd" "bbb") this function would return
;;; (values "ab*" (set "abc" "abd"))
(define (find-partial-match ids)
  (let*
      ([index (index-partial-matches ids)]
       [pattern (for/first
                    ([(pattern ids-set) index]
                    #:when (> (set-count ids-set) 1))
                  pattern)])
    (values
      pattern
      (if (false? pattern) (set) (hash-ref index pattern)))))

(module+ test
  (let-values
      ([(pattern ids) (find-partial-match (list "abc" "abd" "bbb"))])
    (check-equal? pattern "ab*")
    (check-equal? ids (set "abc" "abd")))
  (let-values
      ([(pattern ids) (find-partial-match (list "abc" "def" "ghi"))])
    (check-equal? pattern #f)
    (check-equal? ids (set))))

;;; Read input from the port and print the solution.
(define (main in)
  (let*-values
      ([(ids) (port->lines in)]
       [(pattern matching-ids) (find-partial-match ids)])
    (match pattern
      [#f (printf "No matching IDs found\n")]
      [_
        (printf "Matching IDs:\n")
        (for ([id matching-ids])
            (printf "  ~a\n" id))
        (printf "Matching letters: ~a\n" (string-replace pattern "*" ""))])))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
