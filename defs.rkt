#lang racket
(require threading)

(provide def defn)

;; (def <id> <expr>) => (define <id> <expr>)
(define-syntax (def stx)
  (syntax-case stx ()
    [(_ id expr)
     #'(define id expr)]))

;; (defn foo [a1 a2 . opts] (println "."))
;; ==> (define (foo a1 a2 . opts) (println "."))
(require (for-syntax racket/match))
(define-syntax (defn stx)
  (match (syntax->list stx)
    [(list-rest _ name (list-rest args ... rst-args) body)
     (datum->syntax stx `(define (,name ,@args . ,rst-args) ,@body))]))

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

;;  (test-case
;;      "defn - opt-arg only"
;;   (let ((f-stx #'(defn foo (. opt) opt)))
;;     (check-equal?
;;      (~> f-stx (expand-once) (syntax->datum))
;;      '(define (foo . opt) opt))))
  )
