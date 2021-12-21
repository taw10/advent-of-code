(use-modules
  (srfi srfi-1)
  (ice-9 match)
  (ice-9 receive)
  (ice-9 textual-ports))


;;(define raw-input (call-with-input-file "input" get-line))
(define raw-input "D2FE28")

(define (char->binary c)
  (match c
         (#\0 '(0 0 0 0))
         (#\1 '(0 0 0 1))
         (#\2 '(0 0 1 0))
         (#\3 '(0 0 1 1))
         (#\4 '(0 1 0 0))
         (#\5 '(0 1 0 1))
         (#\6 '(0 1 1 0))
         (#\7 '(0 1 1 1))
         (#\8 '(1 0 0 0))
         (#\9 '(1 0 0 1))
         (#\A '(1 0 1 0))
         (#\B '(1 0 1 1))
         (#\C '(1 1 0 0))
         (#\D '(1 1 0 1))
         (#\E '(1 1 1 0))
         (#\F '(1 1 1 1))))


(define (hexnums->binary input)
  (fold (lambda (digit prev)
          (append (char->binary digit) prev))
        '()
        (reverse input)))


(define (read-packet bin)
  (receive
    (packet-version-bits bin)
    (split-at bin 3)
    (receive
      (packet-type-id-bits bin)
      (split-at bin 3)
      (values packet-version-bits packet-type-id-bits bin))))


(let ((input (hexnums->binary (string->list raw-input))))
  (read-packet input))
