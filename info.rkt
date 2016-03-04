#lang info
(define version "0.1")
(define collection 'multi)
(define pkg-desc "Cameron - a racket dialect")
(define pkg-authors '(jwdevantier ditlevtojner))
(define deps '(["racket" "6.3"]
               "base"
               "rackunit-lib"
               ["threading" "1.1"]))
(define build-deps '("sandbox-lib"
                     "rackunit-lib"
                     "racket-doc"
                     "scribble-lib"))
