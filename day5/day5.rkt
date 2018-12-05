#lang racket
(module+ test
  (require rackunit))

;;; Returns #t if two polymers react with each other
;;; (they are the same letter of difference case).
(define (react? u1 u2)
  (and (equal? (char-downcase u1) (char-downcase u2))
       (or (and (char-lower-case? u1) (char-upper-case? u2))
           (and (char-lower-case? u2) (char-upper-case? u1)))))

(module+ test
  (check-true (react? #\A #\a))
  (check-true (react? #\a #\A))
  (check-true (react? #\J #\j))
  (check-false (react? #\A #\A))
  (check-false (react? #\a #\a))
  (check-false (react? #\A #\b)))

(define (run-reaction polymer-stack)
  (cond
    [(< (length polymer-stack) 2) polymer-stack]
    [(react? (first polymer-stack) (second polymer-stack)) (drop polymer-stack 2)]
    [else polymer-stack]))

(module+ test
  (check-equal? (run-reaction (list #\A #\a #\B))
                (list #\B))
  (check-equal? (run-reaction (list #\A #\b #\a))
                (list #\A #\b #\a)))

(define (run-all-reactions polymer)
  (reverse
    (for/fold
        ([stack empty])
        ([p polymer])
      (run-reaction (cons p stack)))))

(module+ test
  (check-equal? (run-all-reactions (string->list "dbcCCBcCcD"))
                (string->list "dbCBcD")))

(define (remove-unit polymer unit-type)
  (filter (lambda (p) (not (equal? (char-downcase p) (char-downcase unit-type))))
          polymer))

(module+ test
  (check-equal? (remove-unit (list #\A #\b #\a #\B #\c) #\a)
                (list #\b #\B #\c)))

(define (unit-types polymer)
  (for/fold
      ([types (set)])
      ([unit polymer])
    (set-add types (char-downcase unit))))

(module+ test
  (check-equal? (unit-types (list #\A #\b #\a #\C))
                (set #\a #\b #\c)))

(define (try-removing-each-type polymer)
  (for/fold
      ([types-to-chains (hash)])
      ([unit-type (unit-types polymer)])
    (hash-set types-to-chains
              unit-type
              (run-all-reactions (remove-unit polymer unit-type)))))

(module+ test
  (check-equal? (try-removing-each-type (string->list "dabAcCaCBAcCcaDA"))
                (hash #\a (string->list "dbCBcD")
                      #\b (string->list "daCAcaDA")
                      #\c (string->list "daDA")
                      #\d (string->list "abCBAc"))))

(define (find-problematic-unit polymer)
  (define (find-in-hash types-to-chains)
    (for/fold
        ([min-len-unit #f])
        ([(unit-type polymer) types-to-chains])
      (cond
        [(false? min-len-unit) unit-type]
        [(< (length polymer) (length (hash-ref types-to-chains min-len-unit))) unit-type]
        [else min-len-unit])))
  (let*
      ([types-to-chains (try-removing-each-type polymer)]
      [problematic-type (find-in-hash types-to-chains)])
    (values problematic-type (hash-ref types-to-chains problematic-type))))

(module+ test
  (let-values
      ([(type chain) (find-problematic-unit (string->list "dabAcCaCBAcCcaDA"))])
    (check-equal? type #\c)
    (check-equal? chain (string->list "daDA"))))

(define (main in)
  (let*-values
    ([(polymer) (string->list (string-trim (port->string in)))]
    [(reduced-polymer) (run-all-reactions polymer)]
    [(problematic-type shortest-chain) (find-problematic-unit polymer)])
  (printf "Final polymer's length: ~a\n" (length reduced-polymer))
  (printf "Problematic unit: ~a, removing it produces polymer of length ~a\n"
          problematic-type (length shortest-chain))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
