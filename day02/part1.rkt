#lang racket
(module+ test
  (require rackunit))

;;; Sorts chars in the string in alphabetical order.
;;; "bcacc" -> "abccc"
(define (sort-string str)
  (list->string (sort (string->list str) char<?)))

(module+ test
  (check-equal? (sort-string "abcabc") "aabbcc")
  (check-equal? (sort-string "") ""))

;;; If the string has exatly N same consequent chars, return this char.
;;; Otherwise return #f.
(define (has-n-same-chars sorted-string n-chars)
  ; Recursive subfunction.
  ; lst - list of chars;
  ; char - char that is currently being counted;
  ; count - number of times `char` has been found previously.
  (define (has-rec lst char count)
    (cond
      ; If the list is empty, we simply compare number of accumulated chars
      ; with the required number.
      [(empty? lst)
        (if (equal? count n-chars)
          char
          #f)]
      ; The list is not empty. If the next char is different from the `char`,
      ; if compare the numbers again. But this time, if them don't match,
      ; we start counting the next char.
      [(not (equal? (first lst) char))
        (if (equal? count n-chars)
          char
          (has-rec (rest lst) (first lst) 1))]
      ; The list is not empty and the next char is the same as `char`.
      ; Increase the `count`.
      [else (has-rec (rest lst) char (+ 1 count))]))
  (has-rec (string->list sorted-string) #f 0))

(module+ test
  (check-equal? (has-n-same-chars "abc" 1) #\a)
  (check-equal? (has-n-same-chars "abc" 2) #f)
  (check-equal? (has-n-same-chars "aabbcc" 2) #\a)
  (check-equal? (has-n-same-chars "aabbcc" 3) #f)
  (check-equal? (has-n-same-chars "abbcc" 2) #\b)
  (check-equal? (has-n-same-chars "abbccc" 3) #\c)
  (check-equal? (has-n-same-chars "aaabbcc" 2) #\b)
  (check-equal? (has-n-same-chars "aaabc" 2) #f))

;;; Read input from the port and print the solution.
(define (main in)
  (define (has-2-same-chars str)
    (has-n-same-chars str 2))
  (define (has-3-same-chars str)
    (has-n-same-chars str 3))
  (let*
      ([ids (port->lines in)]
       [sorted-ids (map sort-string ids)]
       [ids-with-2-chars (count has-2-same-chars sorted-ids)]
       [ids-with-3-chars (count has-3-same-chars sorted-ids)])
    (printf "IDs with 2 same chars: ~a\n" ids-with-2-chars)
    (printf "IDs with 3 same chars: ~a\n" ids-with-3-chars)
    (printf "Checksum: ~a\n" (* ids-with-2-chars ids-with-3-chars))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
