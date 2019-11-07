#! /usr/bin/env racket
#lang racket

;; set up a new haskell script

(define (write-to-a-file path txt)
  (call-with-output-file path
    (lambda (output-port)
      (display txt output-port))
    #:exists 'append))

;; make (write-header) function
(define (write-header)
  (write-to-a-file "script.hs" "{-# LANGUAGE OverloadedStrings #-}")
  (write-to-a-file "script.hs" "\n\n"))

;; body
(define body #<<EOF
  main = do
    dir  <- pwd
    time <- datefile dir
    print time
EOF
)

(define (write-body)
  (write-to-a-file "script.hs" body))


;; main

(write-header)
(write-body)
