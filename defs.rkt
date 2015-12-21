#lang racket
(require threading)

(provide def fn defn)

;; Missing test proving hygiene
;; e.g. (let ([define "<overwritten>"]) (defn foo [x] x))

(define-syntax (def stx)
  (syntax-parse stx
    [(_ name expr)
     #`(define name expr)]))

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ (args ... (~datum &) r) . body)
     #`(lambda (args ... . r) . body)]
    [(_ (args ...) . body)
     #`(lambda (args ...) . body)]))

(define-syntax (defn stx)
  (syntax-parse stx
    [(_ name (args ... (~datum &) r) .  body)
     #`(define (name args ... . r) . body)]
    [(_ name (args ...) . body)
     #`(define (name args ...) . body)]))

(module+ test
  (require rackunit)
  ;; def tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "[def] define constant"
    (check-equal?
     (~> #'(def foo 2) (expand-once) (syntax->datum))
     '(define foo 2)))

  ;; fn tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  (test-case
;;      "[fn] no-args function"
;;    (check-equal?
;;     (~>  #'(fn () 10) (expand-syntax-to-top-form) (syntax->datum))
;;     '(#%expression (lambda () 10))))

;;  (test-case
;;      "[fn] single-arg function"
;;    (check-equal?
;;     (~>  #'(fn (x) (+ x 10)) (expand-syntax-to-top-form) (syntax->datum))
;;     '(#%expression (lambda (x) (+ x 10)))))

;;   (test-case
;;      "[fn] multi-arg function"
;;    (check-equal?
;;     (~>  #'(fn (x y) (+ x y)) (expand-syntax-to-top-form) (syntax->datum))
;;     '(#%expression (lambda (x y) (+ x y)))))

;;    (test-case
;;      "[fn] multi-body function"
;;    (check-equal?
;;     (~>  #'(fn (x y) (println "stuff") (+ x y)) (expand-syntax-to-top-form) (syntax->datum))
;;     '(#%expression (lambda (x y) (println "stuff") (+ x y)))))

  ;; defn tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "[defn] no-args function"
    (check-equal?
     (~>  #'(defn foo () 10) (expand-once) (syntax->datum))
     '(define (foo) 10)))

  (test-case
      "[defn] single-arg function"
    (check-equal?
     (~>  #'(defn foo (x) (+ x 10)) (expand-once) (syntax->datum))
     '(define (foo x) (+ x 10))))

   (test-case
      "[defn] multi-arg function"
    (check-equal?
     (~>  #'(defn foo (x y) (+ x y)) (expand-once) (syntax->datum))
     '(define (foo x y) (+ x y))))

    (test-case
      "[defn] multi-body function"
    (check-equal?
     (~>  #'(defn foo () (println "stuff") (+ x y)) (expand-once) (syntax->datum))
     '(define (foo) (println "stuff") (+ x y))))

  (test-case
      "defn - rest-arg only"
   (let ((f-stx #'(defn foo (& opt) opt)))
     (check-equal?
      (~> f-stx (expand-once) (syntax->datum))
      '(define (foo . opt) opt))))

  (test-case
      "defn - 1 arg + rest-arg"
    (let ((f-stx #'(defn foo (x & opt) (string-append x (car opt)))))
      (check-equal?
       (~> f-stx (expand-once) (syntax->datum))
       '(define (foo x . opt) (string-append x (car opt))))))

  )
