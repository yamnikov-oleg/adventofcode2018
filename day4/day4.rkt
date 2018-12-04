#lang racket
(require gregor)
(module+ test
  (require rackunit))

(struct input-line (datetime)
        #:transparent)
(struct begin-shift input-line (guard-id)
        #:transparent)
(struct fall-asleep input-line ()
        #:transparent)
(struct wake-up input-line ()
        #:transparent)

(define (input-line-before? line1 line2)
  (datetime<? (input-line-datetime line1) (input-line-datetime line2)))

(define (parse-line line)
  (let*
      ([line-match (or (regexp-match #px"\\[(.+)\\] (.+)" line)
                       (raise (format "Invalid line format: ~s" line)))]
      [datetime-str (second line-match)]
      [rest-str (third line-match)]
      [datetime (parse-datetime datetime-str "yyyy-MM-dd HH:mm")]
      [begin-shift-match (regexp-match #px"Guard #(\\d+) begins shift" rest-str)]
      [fall-asleep-match (equal? "falls asleep" rest-str)]
      [wake-up-match (equal? "wakes up" rest-str)])
    (cond
      [begin-shift-match (begin-shift datetime (string->number (second begin-shift-match)))]
      [fall-asleep-match (fall-asleep datetime)]
      [wake-up-match (wake-up datetime)]
      [else (raise "Could not match line: ~s" line)])))

(module+ test
  (check-equal? (parse-line "[1518-11-03 00:05] Guard #10 begins shift")
                (begin-shift (datetime 1518 11 3 0 5) 10))
  (check-equal? (parse-line "[1518-11-01 00:05] falls asleep")
                (fall-asleep (datetime 1518 11 1 0 5)))
  (check-equal? (parse-line "[1518-11-01 00:55] wakes up")
                (wake-up (datetime 1518 11 1 0 55))))

(define (parse-input in)
  (map parse-line (port->lines in)))

(struct sleepy-time (guard-id from-minutes to-minutes)
        #:transparent)

(define (calculate-sleepy-times input-lines)
  (define (calc-rec sleepy-times current-guard sleeping-since input-lines)
    (if (empty? input-lines)
        sleepy-times
        (match (first input-lines)
          [(begin-shift dt guard-id)
            (calc-rec sleepy-times guard-id #f (rest input-lines))]
          [(fall-asleep dt)
            (calc-rec sleepy-times current-guard (->minutes dt) (rest input-lines))]
          [(wake-up dt)
            (calc-rec (append sleepy-times (list (sleepy-time current-guard
                                                              sleeping-since
                                                              (->minutes dt))))
                      current-guard #f (rest input-lines))])))
  (calc-rec empty #f #f input-lines))

(module+ test
  (check-equal? (calculate-sleepy-times (list (begin-shift (datetime 1518 11 01 00 00 00) 10)
                                              (fall-asleep (datetime 1518 11 01 00 05 00))
                                              (wake-up (datetime 1518 11 01 00 25 00))
                                              (fall-asleep (datetime 1518 11 01 00 30 00))
                                              (wake-up (datetime 1518 11 01 00 55 00))
                                              (begin-shift (datetime 1518 11 01 23 58 00) 99)
                                              (fall-asleep (datetime 1518 11 02 00 40 00))
                                              (wake-up (datetime 1518 11 02 00 50 00))
                                              (begin-shift (datetime 1518 11 03 00 05 00) 10)
                                              (fall-asleep (datetime 1518 11 03 00 24 00))
                                              (wake-up (datetime 1518 11 03 00 29 00))
                                              (begin-shift (datetime 1518 11 04 00 02 00) 99)
                                              (fall-asleep (datetime 1518 11 04 00 36 00))
                                              (wake-up (datetime 1518 11 04 00 46 00))
                                              (begin-shift (datetime 1518 11 05 00 03 00) 99)
                                              (fall-asleep (datetime 1518 11 05 00 45 00))
                                              (wake-up (datetime 1518 11 05 00 55 00))))
                (list (sleepy-time 10 5 25)
                      (sleepy-time 10 30 55)
                      (sleepy-time 99 40 50)
                      (sleepy-time 10 24 29)
                      (sleepy-time 99 36 46)
                      (sleepy-time 99 45 55))))

(define (sum-sleep-minutes sleepy-times)
  (for/fold
      ([minutes (hash)])
      ([time sleepy-times])
    (hash-set minutes (sleepy-time-guard-id time)
              (+ (hash-ref minutes (sleepy-time-guard-id time) 0)
                 (- (sleepy-time-to-minutes time)
                    (sleepy-time-from-minutes time))))))

(define (count-sleep-per-minute sleepy-times guard-id)
  (for/fold
      ([counts (hash)])
      ([time sleepy-times]
       #:when (equal? guard-id (sleepy-time-guard-id time)))
    (for/fold
        ([counts counts])
        ([minute (in-range (sleepy-time-from-minutes time)
                           (sleepy-time-to-minutes time))])
      (let ([old-count (hash-ref counts minute 0)])
           (hash-set counts minute (+ old-count 1))))))

(define (find-sleepiest-guard sleepy-times)
  (let
      ([sleep-sums (sum-sleep-minutes sleepy-times)])
    (for/fold
        ([sleepiest #f])
        ([(guard minutes) sleep-sums])
      (cond
        [(false? sleepiest) guard]
        [(> minutes (hash-ref sleep-sums sleepiest))
          guard]
        [else sleepiest]))))

(define (find-sleepiest-minute counts-per-minute)
  (for/fold
      ([max #f])
      ([(minute count) counts-per-minute])
    (cond
      [(false? max) minute]
      [(> count (hash-ref counts-per-minute max)) minute]
      [else max])))

(struct sleepiest-stats (guard-id minute count)
        #:transparent)

(define (make-sleepiest-stats sleepy-times)
  (for/list
      ([time sleepy-times])
    (let*
        ([counts (count-sleep-per-minute sleepy-times (sleepy-time-guard-id time))]
        [sleepiest-minute (find-sleepiest-minute counts)]
        [count (hash-ref counts sleepiest-minute)])
      (sleepiest-stats (sleepy-time-guard-id time)
                       sleepiest-minute
                       count))))

(define (find-sleepiest-stat stats)
  (for/fold
      ([max #f])
      ([stat stats])
    (cond
      [(false? max) stat]
      [(> (sleepiest-stats-count stat) (sleepiest-stats-count max)) stat]
      [else max])))

;;; Read input from the port and print the solution.
(define (main in)
  (let*
      ([input-lines (parse-input in)]
      [sorted-lines (sort input-lines input-line-before?)]
      [sleepy-times (calculate-sleepy-times sorted-lines)]
      [sleepiest-guard (find-sleepiest-guard sleepy-times)]
      [counts-per-minute (count-sleep-per-minute sleepy-times sleepiest-guard)]
      [sleepiest-minute (find-sleepiest-minute counts-per-minute)]
      [sleepiest-stats (make-sleepiest-stats sleepy-times)]
      [sleepiest-stat (find-sleepiest-stat sleepiest-stats)])
    (printf "Sleepiest guard: ~a\n" sleepiest-guard)
    (printf "Sleeps most on minute: ~a\n" sleepiest-minute)
    (printf "Answer 1: ~a\n" (* sleepiest-guard sleepiest-minute))
    (printf "\n")
    (printf "Most sleeps on minute: ~a\n" (sleepiest-stats-minute sleepiest-stat))
    (printf "By guard: ~a\n" (sleepiest-stats-guard-id sleepiest-stat))
    (printf "(~a times)\n" (sleepiest-stats-count sleepiest-stat))
    (printf "Answer 2: ~a\n" (* (sleepiest-stats-guard-id sleepiest-stat)
                                (sleepiest-stats-minute sleepiest-stat)))))

(module+ main
  (define input-file-path
    (command-line
      #:args (path)
      path))
  (call-with-input-file input-file-path main))
