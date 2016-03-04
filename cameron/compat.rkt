#lang racket/base

(require (prefix-in b: (only-in racket/base set!)))

(provide (rename-out [b:set! assign!]))
