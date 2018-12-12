#lang typed/racket
(module+ test
  (require typed/rackunit))

(struct Circle
  ([vals-before : (Listof Integer)]
   [vals-after : (Listof Integer)])
  #:transparent)

(: forward (-> Circle Circle))
(define (forward cl)
  (if (empty? (Circle-vals-after cl))
      (forward (Circle empty (reverse (Circle-vals-before cl))))
      (Circle (cons (first (Circle-vals-after cl)) (Circle-vals-before cl))
              (rest (Circle-vals-after cl)))))

(module+ test
  (check-equal? (forward (Circle '(3 2 1) '(4 5 6)))
                (Circle '(4 3 2 1) '(5 6)))
  (check-equal? (forward (Circle '(3 2 1) '()))
                (Circle '(1) '(2 3))))


(: backward (-> Circle Circle))
(define (backward cl)
  (if (empty? (Circle-vals-before cl))
      (backward (Circle (reverse (Circle-vals-after cl)) empty))
      (Circle (rest (Circle-vals-before cl))
              (cons (first (Circle-vals-before cl)) (Circle-vals-after cl)))))

(module+ test
  (check-equal? (backward (Circle '(3 2 1) '(4 5 6)))
                (Circle '(2 1) '(3 4 5 6)))
  (check-equal? (backward (Circle '() '(4 5 6)))
                (Circle '(5 4) '(6))))

(: Circle->list (-> Circle (Listof Integer)))
(define (Circle->list cl)
  (append (reverse (Circle-vals-before cl)) (Circle-vals-after cl)))

(module+ test
  (check-equal? (Circle->list (Circle '(3 2 1) '(4 5 6)))
                '(1 2 3 4 5 6)))

(: move (-> Circle Integer Circle))
(define (move cl rel)
  (cond
    [(equal? rel 0) cl]
    [(> rel 0) (move (forward cl) (- rel 1))]
    [(< rel 0) (move (backward cl) (+ rel 1))]))

(module+ test
  (check-equal? (move (Circle '(3 2 1) '(4 5 6)) 4)
                (Circle '(1) '(2 3 4 5 6)))
  (check-equal? (move (Circle '(3 2 1) '(4 5 6)) -4)
                (Circle '(5 4 3 2 1) '(6))))

(: current-value (-> Circle Integer))
(define (current-value cl)
  (cond
    [(and (empty? (Circle-vals-before cl)) (empty? (Circle-vals-after cl))) (error "Circle is empty")]
    [(empty? (Circle-vals-after cl)) (last (Circle-vals-before cl))]
    [else (first (Circle-vals-after cl))]))

(module+ test
  (check-equal? (current-value (Circle '(3 2 1) '(4 5 6))) 4)
  (check-equal? (current-value (Circle '(6 5 4 3 2 1) '())) 1))

(: insert-before (-> Circle Integer Circle))
(define (insert-before cl val)
  (Circle (Circle-vals-before cl)
          (cons val (Circle-vals-after cl))))

(module+ test
  (check-equal? (insert-before (Circle '(3 2 1) '(4 5 6)) 7)
                (Circle '(3 2 1) '(7 4 5 6))))

(: insert-after (-> Circle Integer Circle))
(define (insert-after cl val)
  (insert-before (forward cl) val))

(module+ test
  (check-equal? (insert-after (Circle '(3 2 1) '(4 5 6)) 7)
                (Circle '(4 3 2 1) '(7 5 6))))

(: remove (-> Circle Circle))
(define (remove cl)
  (Circle (Circle-vals-before cl) (rest (Circle-vals-after cl))))

(module+ test
  (check-equal? (remove (Circle '(3 2 1) '(4 5 6)))
                (Circle '(3 2 1) '(5 6))))

(struct Game
  ([circle : Circle]
   [scores : (Listof Integer)]
   [next-player : Integer]
   [next-marble : Integer]
   [last-marble : Integer])
  #:transparent)

(: new-game (-> Integer Integer Game))
(define (new-game players last-marble)
  (Game
    (Circle '() '(0))
    (build-list players (lambda (x) 0))
    0
    1
    last-marble))

(: Game-players-total (-> Game Integer))
(define (Game-players-total game)
  (length (Game-scores game)))

(: next-turn (-> Game Game))
(define (next-turn game)
  (define next-player
    (if (>= (+ 1 (Game-next-player game))
            (Game-players-total game))
        0
        (+ 1 (Game-next-player game))))
  (define (just-place-marble)
    (Game
      (insert-before (move (Game-circle game) 2) (Game-next-marble game))
      (Game-scores game)
      next-player
      (+ 1 (Game-next-marble game))
      (Game-last-marble game)))
  (define (special-23-stuff)
    (let*
        ([current-marble (Game-next-marble game)]
         [removed-marble (current-value (move (Game-circle game) -7))]
         [player (Game-next-player game)]
         [current-score (list-ref (Game-scores game) player)]
         [new-circle (remove (move (Game-circle game) -7))])
      (Game
        new-circle
        (list-set (Game-scores game) player (+ current-score current-marble removed-marble))
        next-player
        (+ 1 (Game-next-marble game))
        (Game-last-marble game))))
  (if (equal? (modulo (Game-next-marble game) 23) 0)
    (special-23-stuff)
    (just-place-marble)))

(: next-turn* (-> Game Integer Game))
(define (next-turn* game turns)
  (if (zero? turns)
      game
      (next-turn* (next-turn game) (- turns 1))))

(module+ test
  ;;; 1 turn
  (let
      ([game (next-turn (new-game 9 25))])
    (check-equal? (Circle->list (Game-circle game))
                  '(0 1))
    (check-equal? (Game-scores game) '(0 0 0 0 0 0 0 0 0))
    (check-equal? (Game-next-player game) 1)
    (check-equal? (Game-next-marble game) 2))
  ;;; 5 turns
  (let
      ([game (next-turn* (new-game 9 25) 5)])
    (check-equal? (Circle->list (Game-circle game))
                  '(0 4 2 5 1 3))
    (check-equal? (Game-scores game) '(0 0 0 0 0 0 0 0 0))
    (check-equal? (Game-next-player game) 5)
    (check-equal? (Game-next-marble game) 6))
  ;;; 15 turns
  (let
      ([game (next-turn* (new-game 9 25) 15)])
    (check-equal? (Circle->list (Game-circle game))
                  '(0 8 4 9 2 10 5 11 1 12 6 13 3 14 7 15))
    (check-equal? (Game-scores game) '(0 0 0 0 0 0 0 0 0))
    (check-equal? (Game-next-player game) 6)
    (check-equal? (Game-next-marble game) 16))
  ;;; 23 turns
  (let
      ([game (next-turn* (new-game 9 25) 23)])
    (check-equal? (Circle->list (Game-circle game))
                  '(0 16 8 17 4 18 19 2 20 10 21 5 22 11 1 12 6 13 3 14 7 15))
    (check-equal? (Game-scores game) '(0 0 0 0 32 0 0 0 0))
    (check-equal? (Game-next-player game) 5)
    (check-equal? (Game-next-marble game) 24))
  ;;; 25 turns
  (let
      ([game (next-turn* (new-game 9 25) 25)])
    (check-equal? (Circle->list (Game-circle game))
                  '(0 16 8 17 4 18 19 2 24 20 25 10 21 5 22 11 1 12 6 13 3 14 7 15))
    (check-equal? (Game-scores game) '(0 0 0 0 32 0 0 0 0))
    (check-equal? (Game-next-player game) 7)
    (check-equal? (Game-next-marble game) 26)))

(: play (-> Integer Integer Game))
(define (play players last-marble)
  (: play-rec (-> Game Game))
  (define (play-rec game)
    (if (> (Game-next-marble game) (Game-last-marble game))
        game
        (play-rec (next-turn game))))
  (play-rec (new-game players last-marble)))
(provide play)

(: winning-score (-> Game Integer))
(define (winning-score game)
  (: id (-> Integer Integer))
  (define (id x) x)
  (argmax id (Game-scores game)))
(provide winning-score)

(module+ test
  (check-equal? (winning-score (play 9 25)) 32)
  (check-equal? (winning-score (play 10 1618)) 8317)
  (check-equal? (winning-score (play 17 1104)) 2764)
  (check-equal? (winning-score (play 21 6111)) 54718)
  (check-equal? (winning-score (play 30 5807)) 37305))

(module+ main
  (define players (make-parameter 9))
  (define last-marble (make-parameter 25))
  (: require-number (-> Any Integer))
  (define (require-number s)
    (or (cast (string->number (cast s String)) Integer)
        (error "Invalid number" s)))
  (command-line
      #:program "day9"
      #:once-each
      [("-p" "--players") cnt
                          "Number of players (9)"
                          (players (require-number cnt))]
      [("-m" "--last-marble") mrb
                              "The basic step duration (25)"
                              (last-marble (require-number mrb))])
  (printf "Winning score: ~a\n"
    (winning-score (play (players) (last-marble)))))
