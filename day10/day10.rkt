#lang racket
(module+ test
  (require rackunit))

(struct vec (x y) #:transparent)

;;; Parses "X, X" string into (vec X X)
(define (parse-vector str)
  (let
      ([mch (regexp-match #px"\\s*(-?\\d+),\\s*(-?\\d+)\\s*" str)])
    (match mch
      [#f (error "Invalid vector" str)]
      [(list _ x-str y-str) (vec (string->number x-str) (string->number y-str))])))

(module+ test
  (check-equal? (parse-vector " 9,  1") (vec 9 1))
  (check-equal? (parse-vector "-1, -2") (vec -1 -2)))

(struct point (init-pos velocity) #:transparent)

;;; Parses input line into point struct
(define (parse-point line)
  (let
      ([mch (regexp-match #px"position=<([^>]+)> velocity=<([^>]+)>" line)])
    (match mch
      [#f (error "Invalid point" line)]
      [(list _ pos-str vel-str)
        (point (parse-vector pos-str) (parse-vector vel-str))])))

(module+ test
  (check-equal? (parse-point "position=< 9,  1> velocity=< 0,  2>")
                (point (vec 9 1) (vec 0 2))))

(define (vec-mult v m)
  (vec (* m (vec-x v)) (* m (vec-y v))))

(define (vec-add v1 v2)
  (vec (+ (vec-x v1) (vec-x v2))
       (+ (vec-y v1) (vec-y v2))))

;;; Calculates points position at the given time
(define (point-pos pnt time)
  (vec-add (point-init-pos pnt) (vec-mult (point-velocity pnt) time)))

;;; Returns a set of vec coordinates where points are located at the given time.
(define (set-coordinates pnts time)
  (for/fold
      ([set-coords (set)])
      ([pnt pnts])
    (set-add set-coords (point-pos pnt time))))

(define max-picture-size 100)

;;; Returns #f if the picture size excedes max-picture-size either by width
;;; or by height.
(define (picture-too-big coords)
  (define min-x (vec-x (argmin vec-x (set->list coords))))
  (define max-x (vec-x (argmax vec-x (set->list coords))))
  (define min-y (vec-y (argmin vec-y (set->list coords))))
  (define max-y (vec-y (argmax vec-y (set->list coords))))
  (or (> (- max-x min-x) max-picture-size)
      (> (- max-y min-y) max-picture-size)))

;;; Converts a set of vec structs into a string visualizing this set.
;;; Each coordinates from the coords list is displayed as "#"
;;; Other coordinates around them are displayed as ".".
(define (visualize coords)
  (define min-x (vec-x (argmin vec-x (set->list coords))))
  (define max-x (vec-x (argmax vec-x (set->list coords))))
  (define min-y (vec-y (argmin vec-y (set->list coords))))
  (define max-y (vec-y (argmax vec-y (set->list coords))))
  (define (coord->char coord)
    (if (set-member? coords coord)
        #\#
        #\.))
  (define (visualize-line y)
    (let
        ([pnts (map (lambda (x) (vec x y)) (range min-x (add1 max-x)))])
      (list->string (map coord->char pnts))))
  (string-join (map visualize-line (range min-y (add1 max-y))) "\n"))

(module+ test
  (check-equal? (visualize (list (vec 0 0) (vec 2 0)))
                "#.#")
  (check-equal? (visualize (list (vec 1 1) (vec 3 2)))
                (string-join (list "#.."
                                   "..#")
                             "\n")))

(define (main in)
  (let
      ([pnts (map parse-point (port->lines in))])
    (for
        ([time (in-naturals)])
      (let ([coords (set-coordinates pnts time)])
        (when (not (picture-too-big coords))
            (printf "Time: ~a\n" time)
            (printf "~a\n" (visualize coords))
            (printf "\n")
            (sleep 1))))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
