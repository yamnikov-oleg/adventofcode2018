#lang racket
(module+ test
  (require rackunit))

(struct pnt (x y)
        #:transparent)

;;; Parses coordinates string into pnt struct. Raises an error if parsing fails.
;;; Example: (parse-coords "8, 3") => (pnt 8 3)
(define (parse-coords line)
  (match (string-split line ", ")
    [(list x-str y-str) (pnt (string->number x-str) (string->number y-str))]
    [_ (error "Invalid line ~s" line)]))

(module+ test
  (check-equal? (parse-coords "8, 3") (pnt 8 3)))

;;; Finds the "best" element in the list using the better-than? function.
;;; better-than? accept two elements of the list and should return #t
;;; if the first element is "better" than the second, or #f otherwise.
(define (find-best lst better-than?)
  (for/fold
      ([best-elem #f])
      ([elem lst])
    (cond
      [(false? best-elem) elem]
      [(better-than? elem best-elem) elem]
      [else best-elem])))

(struct borders (left top right bottom)
        #:transparent)

;;; Finds the leftmost and rightmost X coordinates and topmost and bottommost
;;; Y coordinates of the points in the list and returns them as a borders struct.
(define (field-borders pnts)
  (borders
    (pnt-x (find-best pnts (lambda (p1 p2) (< (pnt-x p1) (pnt-x p2)))))
    (pnt-y (find-best pnts (lambda (p1 p2) (< (pnt-y p1) (pnt-y p2)))))
    (pnt-x (find-best pnts (lambda (p1 p2) (> (pnt-x p1) (pnt-x p2)))))
    (pnt-y (find-best pnts (lambda (p1 p2) (> (pnt-y p1) (pnt-y p2)))))))

(module+ test
  (check-equal? (field-borders (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                     (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                (borders 1 1 8 9)))

;;; Returns Manhattan distance between two points.
(define (dist p1 p2)
  (+ (abs (- (pnt-x p1) (pnt-x p2)))
     (abs (- (pnt-y p1) (pnt-y p2)))))

(module+ test
  (check-equal? (dist (pnt 1 2) (pnt 3 0)) 4))

;;; Returns a hash which maps a distance value to the set a points from `pnts`
;;; which are located in the said distance from the point `pnt`.
(define (distances pnt pnts)
  (for/fold
      ([dists (hash)])
      ([p pnts])
    (let*
        ([d (dist p pnt)]
        [same-dist-set (hash-ref dists d (set))])
      (hash-set dists d (set-add same-dist-set p)))))

;;; Returns the closest point to the `pnt` in the `pnts` list.
;;; If there multiple candidates for the closest point (having the same distance
;;; to the `pnt`), `closest` returns #f.
(define (closest pnt pnts)
  (let*
      ([dist-hash (distances pnt pnts)]
      [shortest-dist (find-best (hash-keys dist-hash) <)])
    (match (set->list (hash-ref dist-hash shortest-dist))
      [(list p) p]
      [else #f])))

(module+ test
  (check-equal? (closest (pnt 2 1) (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                         (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                (pnt 1 1))
  (check-equal? (closest (pnt 5 2) (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                         (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                (pnt 5 5))
  (check-equal? (closest (pnt 0 4) (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                         (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                #f))

;;; Returns a set of points located on the edges of provided borders
;;; (`borders` struct).
(define (border-edges brd)
  (set-union
    (for/set
        ([x (in-range (borders-left brd) (+ 1 (borders-right brd)))])
      (pnt x (borders-top brd)))
    (for/set
        ([x (in-range (borders-left brd) (+ 1 (borders-right brd)))])
      (pnt x (borders-bottom brd)))
    (for/set
        ([y (in-range (borders-top brd) (+ 1 (borders-bottom brd)))])
      (pnt (borders-left brd) y))
    (for/set
        ([y (in-range (borders-top brd) (+ 1 (borders-bottom brd)))])
      (pnt (borders-right brd) y))))

(module+ test
  (check-equal? (border-edges (borders 1 1 3 4))
                (set (pnt 1 1) (pnt 2 1)
                     (pnt 3 1) (pnt 3 2) (pnt 3 3)
                     (pnt 3 4) (pnt 2 4)
                     (pnt 1 4) (pnt 1 3) (pnt 1 2))))

;;; Returns a set of points, which are located on the edge of the field of
;;; points `pnts`. Edge point is the point, whose surrounding area would be
;;; inifinitely large.
;;; This function recognises and point as an edge point, if it's the closest
;;; to one of the locations on the edges of the field (found by `border-edges`).
(define (edge-pnts pnts)
  (for/set
      ([b-edge (border-edges (field-borders pnts))]
      #:when (not (false? (closest b-edge pnts))))
    (closest b-edge pnts)))

(module+ test
  (check-equal? (edge-pnts (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                 (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                (set (pnt 1 1) (pnt 1 6) (pnt 8 3) (pnt 8 9))))

;;; Returns a set of all the locations in the field inside the given borders.
(define (field-pnts brd)
  (for*/set
      ([x (in-range (borders-left brd) (+ 1 (borders-right brd)))]
       [y (in-range (borders-top brd) (+ 1 (borders-bottom brd)))])
    (pnt x y)))

(module+ test
  (check-equal? (field-pnts (borders 1 1 3 4))
                (set (pnt 1 1) (pnt 1 2) (pnt 1 3) (pnt 1 4)
                     (pnt 2 1) (pnt 2 2) (pnt 2 3) (pnt 2 4)
                     (pnt 3 1) (pnt 3 2) (pnt 3 3) (pnt 3 4))))

;;; Returns a hash which map each non-edge point to its surrounding area.
(define (calc-areas-around pnts)
  (define edges (edge-pnts pnts))
  (for/fold
      ([areas (hash)])
      ([p (field-pnts (field-borders pnts))])
    (match (closest p pnts)
      [#f areas]
      [cp #:when (set-member? edges cp) areas]
      [cp (hash-set areas cp (+ 1 (hash-ref areas cp 0)))])))

(module+ test
  (check-equal? (calc-areas-around (list (pnt 1 1) (pnt 1 6) (pnt 8 3)
                                         (pnt 3 4) (pnt 5 5) (pnt 8 9)))
                (hash (pnt 3 4) 9
                      (pnt 5 5) 17)))

;;; Returns the sum of distances from `pnt` to every point in `pnts`.
(define (sum-dists pnt pnts)
  (for/sum
      ([p pnts])
    (dist pnt p)))

;;; Counts locations inside the field of `pnts`, whose `sum-dists` is
;;; less than `max-dist`.
(define (count-points-within-reach pnts max-dist)
  (for/sum
      ([p (field-pnts (field-borders pnts))])
    (if (< (sum-dists p pnts) max-dist)
        1
        0)))

(define (main in max-dist)
  (let*
      ([pnts (map parse-coords (port->lines in))]
       [areas (calc-areas-around pnts)]
       [biggest-area-pnt (find-best (hash-keys areas)
                                    (lambda (p1 p2) (> (hash-ref areas p1)
                                                       (hash-ref areas p2))))]
       [center-pnts-cnt (count-points-within-reach pnts max-dist)])
    (printf "Biggest area: ~a\n" (hash-ref areas biggest-area-pnt))
    (printf "Around point: ~a\n" biggest-area-pnt)
    (printf "Locations within ~a distance of all other points: ~a\n" max-dist center-pnts-cnt)))

(module+ main
  (define args
    (command-line
      #:args (path max-dist)
      (cons path (or (string->number max-dist)
                     (error "Invalid number:" max-dist)))))
  (call-with-input-file (car args) (lambda (in) (main in (cdr args)))))
