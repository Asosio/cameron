#lang racket
(require "defs.rkt"
         "hash.rkt"
         threading)
(provide
 (except-out (all-from-out racket)
             define
             lambda)
 (all-from-out "defs.rkt"
               threading)
 (prefix-out h/ (all-from-out "hash.rkt"))
 (rename-out [lambda fn]))
