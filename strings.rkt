#lang racket/base

(require cameron/defs)

(provide substring?)

;; Returns #t iff substring 'sub' is found in string 'str'
;; From:  https://gist.github.com/bennn/0772b7d59bfdb421c048
(defn substring? [str sub]
  (def l1 (string-length str))
  (def l2 (string-length sub))
  (def d (- l1 l2))
  (or (zero? l2)
      (let loop ([start 0])
        (and (<= start d)
             (or (let loop2 ([offset 0])
                   (or (= offset l2)
                       (and (char=? (string-ref str (+ start offset))
                                    (string-ref sub offset))
                            (loop2 (add1 offset)))))
                 (loop (add1 start)))))))
