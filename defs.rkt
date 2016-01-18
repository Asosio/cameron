#lang racket/base
(require
 (for-syntax racket/base))
;; Parsing syntax
;; Why: basically the grammar for pattern matching in syntax-parse
;; file:///opt/racket/share/doc/racket/syntax/Parsing_Syntax.html?q=syntax-parse#%28form._%28%28lib._syntax%2Fparse..rkt%29._syntax-parse%29%29

;; Phases & Reusable Syntax Classes
;; Why: because syntax classes are cool and can reduce complexity (ESP repetition)
;; http://docs.racket-lang.org/syntax/Phases_and_Reusable_Syntax_Classes.html

(provide def fn defn case-fn)

;; Missing test proving hygiene
;; e.g. (let ([define "<overwritten>"]) (defn foo [x] x))
(require (for-syntax syntax/parse))
(define-syntax (def stx)
  (syntax-parse stx
    [(_ name:id expr)
     #`(define name expr)]))

(define-syntax (fn stx)
  (syntax-parse stx
    [(_ (args ... (~datum &) r) . body)
     #`(lambda (args ... . r) . body)]
    [(_ (args ...) . body)
     #`(lambda (args ...) . body)]))

(define-syntax (defn stx)
  (syntax-parse stx
    [(_ name:id (args ... (~datum &) r) .  body)
     #`(define (name args ... . r) . body)]
    [(_ name:id (args ...) . body)
     #`(define (name args ...) . body)]))

(begin-for-syntax
  (define-syntax-class fn-body
    #:datum-literals (&)
    (pattern ([(~and args (~not &)) ...
              (~optional (~seq & r) #:defaults ([r #'()]))]
              . body))))

(define-syntax (case-fn stx)
  (syntax-parse stx
    [(_ fns:fn-body ...)
     #`(case-lambda [(fns.args ... . fns.r) . fns.body] ...)]))

(module+ test
  (require rackunit
           threading
           (only-in racket/local local)
           (only-in racket/string string-join))
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
    (check-equal?
     (~> #'(defn foo (x & opt) (string-append x (car opt))) (expand-once) (syntax->datum))
     '(define (foo x . opt) (string-append x (car opt)))))

  (test-case
      "defn - eval no args"
    (check-equal?
     (let () (defn f [] 10) (f))
     10))

  (test-case
      "defn - eval 1 arg"
    (check-equal? (let () (defn f [x] x) (f "hello")) "hello"))

  (test-case
      "defn - eval rest-args"
    (check-equal? (let () (defn f [& r] (string-join r ","))
                       (f "a" "b" "c")) "a,b,c"))

  (test-case
      "defn - hygienic test"
    (check-equal?
     (let ([define (lambda (x . r) "overwritten")])
       (defn f [x y] (+ x y)) (f 1 3))
     4))

  ;; case-fn tests
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (test-case
      "case-fn - test application"
    (local [(def cfn (case-fn
                      [(x) x]
                      [(x y z & r) (apply + x y z r)]))]
           ;; Test application of the first function
           (check-equal? (cfn 10) 10)
           (check-equal? (cfn 11) 11)
           ;; check that the cfn (defined above) rightly doesn't accept 2 arguments
           ;; (ensures that not all fn's erroneously have a rst argument component)
           (check-equal?
            (with-handlers ([exn:fail:contract:arity? (lambda [e] "99 problems")])
              (cfn 1 2)) "99 problems")
           ;; check that rest arguments are properly filled
           (check-equal? (cfn 0 0 0 1 7 13) 21)))

  (test-case
      "case-fn - test expansion"
    (check-equal?
     (~> #'(case-fn [(x) (println x)]
                    [(x y) (println (string-join '(x y) "-"))]
                    [(x y & r) (println (string-join (list* x y r) "-"))])
         (expand-once) (syntax->datum))
     '(case-lambda [(x) (println x)]
                   [(x y) (println (string-join '(x y) "-"))]
                   [(x y . r) (println (string-join (list* x y r) "-"))]))))
