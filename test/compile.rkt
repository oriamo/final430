#lang racket

(provide run-compile
         run-compile/io
         compile-test-suite
         compile-io-test-suite)

(require "../compile.rkt"
         "../run.rkt"
         "../parse.rkt"

         "tests.rkt")

(define (run-compile . es)
  (run (compile (apply parse es))))

(define (run-compile/io in . es)
  (run/io (compile (apply parse es)) in))

(define compile-test-suite (make-test-suite "compile" run-compile))
(define compile-io-test-suite (make-io-test-suite "compile/io" run-compile/io))

(module+ test
  (run-tests compile-test-suite)
  (run-tests compile-io-test-suite))
