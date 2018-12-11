#lang racket
(require profile)
(require "./day9.rkt")
(profile-thunk (thunk (begin (play 459 7132000) #f)))
