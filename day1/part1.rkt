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

;;; Solves the problem for the given input port and prints the solution.
(define (main in)
  (let*
      ([freqs (parse-lines in)]
       [sum (foldl + 0 freqs)])
    (printf "~a\n" sum)))

(define input-file-path
  (command-line
    #:args (path)
    path))

(call-with-input-file input-file-path main)
