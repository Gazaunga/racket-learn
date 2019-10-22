#! usr/bin/env racket
#lang racket

;; parameter My-Name is one of:
;; - #false
;; - String
(define my-name (make-parameter #false))

;; command line parser
(define parser
  (command-line
   #:usage-help
   "Have the computer greet you!"

   #:once-each
   [("-n" "--name") NAME
                    "Set your name"
                    (my-name NAME)]

   #:args () (void)))

;; get-greeting : My-Name -> String
;; Gets the greeting for the given My-Name
(define (get-greeting mn)
  (cond
    [(boolean? mn) "Hello, unknown person!"]
    [(string? mn) (string-append "Hello, " mn "!")]))

;; prints result to the command line
(printf "~a\n" (get-greeting (my-name)))
