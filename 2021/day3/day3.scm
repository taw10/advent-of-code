(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (srfi srfi-9)
             (srfi srfi-26))


(define (read-file-as-lines filename)
  (delete ""
          (string-split
            (call-with-input-file
              filename
              (lambda (port)
                (get-string-all port)))
            #\newline)))


(define (char->bit c)
  (cond
    ((eq? c #\0) 0)
    ((eq? c #\1) 1)
    (else #f)))


(define (string->binary str)
  (map char->bit
       (string->list str)))


(define (read-binary-from-file filename)
  (map string->binary
       (read-file-as-lines filename)))


(define (add-lists a b)
  (map + a b))


(define (most-popular-bits input)
  (let ((n-input (length input)))
    (map
      (lambda (a)
        (if (>= a (/ n-input 2))
          1
          0))
      (reduce add-lists #f input))))


(define (binary-complement r)
  (map
    (lambda (a) (- 1 a))
    r))


(define (powers-of-2-rec n pwr so-far)
  (if (> n 0)
    (powers-of-2-rec
      (- n 1)
      (+ pwr 1)
      (cons (expt 2 pwr)
            so-far))
    so-far))


(define (powers-of-2 n)
  (powers-of-2-rec n 0 '()))


(define (binary->number r)
  (reduce + #f
          (map * r
               (powers-of-2 (length r)))))


(define input (read-binary-from-file "input"))


;; Position goes from left to right, starting at index zero
(define (bit-match-at-position a b pos)
  (eq?
    (list-ref a pos)
    (list-ref b pos)))


(define (filter-matching-bit-position input bits pos)
  (filter
    (cut bit-match-at-position <> bits pos)
    input))


(define (one-item? l)
  (nil? (cdr l)))


(define (last-match-rec input bit-func pos)
  (let ((filt (filter-matching-bit-position input
                                            (bit-func input)
                                            pos)))
    (if (one-item? filt)
      filt
      (last-match-rec filt
                      bit-func
                      (+ pos 1)))))


(define (last-match input bit-func)
  (car (last-match-rec input bit-func 0)))


(define (least-popular-bits input)
  (binary-complement
    (most-popular-bits input)))


(let ((gamma-rate (most-popular-bits input)))
  (let ((epsilon-rate (binary-complement gamma-rate)))
    (format #t "Part 1: ~a\n" (* (binary->number gamma-rate)
                                 (binary->number epsilon-rate)))))

(format #t "Part 2: ~a\n" (* (binary->number (last-match input most-popular-bits))
                             (binary->number (last-match input least-popular-bits))))
