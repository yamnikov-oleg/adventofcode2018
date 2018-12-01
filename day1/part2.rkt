#lang racket

;;; Parses a line from the input file into a number.
;;; Returns #f if the line could not be parsed.
(define (parse-line line)
  (let
      ([num (string->number (substring line 1))])
    (cond
      [(false? num) #f]
      [(string-prefix? line "+") num]
      [(string-prefix? line "-") (* -1 num)]
      [else #f])))

;;; Applies parse-line to the every line in the input port and returns
;;; a list of numbers.
(define (parse-lines in)
  (for/list ([line (in-lines in)])
    (parse-line line)))

;;; Applies change to the current-freq and adds it to the prev-freqs set.
;;; Returns new prev-freqs set, new current-freq and a third value,
;;; which equals to the new current-freq if it has been repeated before
;;; or #f otherwise.
(define (apply-freq prev-freqs current-freq change)
  (let*
      ([new-freq (+ current-freq change)]
       [repeated-freq (if (set-member? prev-freqs new-freq)
                          new-freq
                          #f)])
    (values
      (set-add prev-freqs new-freq)
      new-freq
      repeated-freq)))

;;; Find the first repeated frequence after applies changes-list in endless loop.
;;; Returns the frequence and the number steps required to achieve it.
(define (find-repeated-freq changes-list)
  (define (find-rec changes-stream prev-freqs current-freq steps)
    (let-values
        ([(new-prev-freqs new-current-freq repeated-freq)
          (apply-freq prev-freqs current-freq (stream-first changes-stream))])
      (if repeated-freq
        (values repeated-freq (+ 1 steps))
        (find-rec (stream-rest changes-stream) new-prev-freqs new-current-freq (+ 1 steps)))))
  (find-rec (sequence->stream (in-cycle changes-list)) (set) 0 0))

;;; Solves the problem for the given input port and prints the solution.
(define (main in)
  (let*-values
      ([(changes) (parse-lines in)]
       [(repeated steps) (find-repeated-freq changes)])
    (printf "~a changes in the list\n" (length changes))
    (printf "~a repeated in ~a steps\n" repeated steps)))

(define input-file-path
  (command-line
    #:args (path)
    path))

(call-with-input-file input-file-path main)
