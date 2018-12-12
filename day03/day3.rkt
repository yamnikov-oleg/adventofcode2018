#lang racket

(struct rect (id left top width height)
        #:transparent)

(struct cell (x y)
        #:transparent)

;;; Parses a line from input into a rect struct.
(define (parse-rect line)
  (match
      (regexp-match #px"^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)$" line)
    [#f #f]
    [(list _ id-s left-s top-s width-s height-s)
      (rect (string->number id-s)
            (string->number left-s)
            (string->number top-s)
            (string->number width-s)
            (string->number height-s))]))

;;; Returns a list of cells this rect contains.
;;; E.g. rect "#3 @ 5,5: 2x2" contains cells
;;; (cell 5 5), (cell 5 6), (cell 6 5) and (cell 6 6)
(define (rect-cells r)
  (for*/list
      ([i (in-range (rect-width r))]
       [j (in-range (rect-height r))])
    (cell (+ (rect-left r) i)
           (+ (rect-top r) j))))

;;; Marks every cell of rect r as belonging to this rect on the cell-map.
;;; cell-map is hash which maps cells (cell struct) onto sets of rect ids, which
;;; contain this cell.
(define (mark-cells cell-map r)
  (for/fold
      ([cell-map cell-map])
      ([cll (rect-cells r)])
    (hash-set cell-map
              cll
              (set-add (hash-ref cell-map cll (set))
                       (rect-id r)))))

;;; Makes a cell-map out of rects list.
(define (make-cell-map rects)
  (for/fold
      ([cell-map (hash)])
      ([r rects])
    (mark-cells cell-map r)))

;;; Counts how many cells in the cell-map belong to more than one rect.
(define (count-multi-rect-cells cell-map)
  (for/fold
      ([count 0])
      ([(cll rects-set) cell-map])
    (if (> (set-count rects-set) 1)
        (+ 1 count)
        count)))

;;; Convert cell-map into overlap-map.
;;; overlap-map is a hash which maps claim ids onto the number of cells of
;;; the claim which are overlapped with another claim.
;;; A claim without any overlaps would have this number equal to 0.
(define (count-overlaps cell-map)
  (for/fold
      ([overlap-map (hash)])
      ([(cll rects-set) cell-map])
    (for/fold
        ([overlap-map overlap-map])
        ([r-id rects-set])
      (hash-set overlap-map r-id
                (+ (hash-ref overlap-map r-id 0)
                   (set-count rects-set)
                   -1)))))

;;; Returns a set of ids of claims which have to overlaps with other claims.
(define (find-unoverlapped-claims overlap-map)
  (for/set
      ([(r-id overlaps-count) overlap-map]
       #:when (= overlaps-count 0))
    r-id))

;;; Read input from the port and print the solution.
(define (main in)
  (let*
      ([rects (map parse-rect (port->lines in))]
       [cell-map (make-cell-map rects)]
       [overlap-map (count-overlaps cell-map)]
       [unoverlapped-claims (find-unoverlapped-claims overlap-map)])
    (printf "Number of cells belonging to multiple rects: ~a\n" (count-multi-rect-cells cell-map))
    (printf "Claims without any overlaps: ~a\n"
            (string-join (map number->string (set->list unoverlapped-claims))
                         ", "))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
