(use-modules
  (ice-9 receive)
  (srfi srfi-26)
  (srfi srfi-1)
  (ice-9 textual-ports))


(define (string->rule str)
  (let ((sp (string-split str #\space)))
    (cons
      (cons (string->symbol (substring (car sp) 0 1))
            (string->symbol (substring (car sp) 1 2)))
      (string->symbol (caddr sp)))))


(define (read-rules port so-far)
  (let ((line (get-line port)))
    (if (eof-object? line)
      so-far
      (read-rules port
                  (cons
                    (string->rule line)
                    so-far)))))


(define (subchr->list str len pos so-far)
  (if (= pos len)
    so-far
    (subchr->list str
                  len
                  (+ pos 1)
                  (cons (string->symbol
                          (substring str pos (1+ pos)))
                        so-far))))


(define (string->symlist str)
  (reverse (cons 'end (subchr->list str (string-length str) 0 '(start)))))


(define (read-input port)
  (let* ((initial-seq (string->symlist (get-line port)))
         (junk (get-line port))
         (pair-insertion-rules (read-rules port '())))
    (values pair-insertion-rules initial-seq)))


(define (seq+ n pairs key)
  (let ((cur-val (assoc-ref pairs key)))
    (assoc-set! pairs key
                (+ n (if cur-val cur-val 0)))))


(define (seq+twice n pairs key1 key2)
  (seq+ n
    (seq+ n pairs key1)
    key2))


(define (make-pairs seq)
  (fold
    (lambda (a b pairs)
      (seq+ 1 pairs (cons a b)))
    '()
    seq
    (cdr seq)))


(define (run-rules pair-freqs rules)
  (fold
    (lambda (pair-freq new-pairs)
      (let ((pair (car pair-freq))
            (freq (cdr pair-freq)))
        (let ((ins (assoc-ref rules pair)))
          (if ins
            (seq+twice freq
                       new-pairs
                       (cons (car pair) ins)
                       (cons ins (cdr pair)))
            (seq+ freq new-pairs pair)))))
    '()
    pair-freqs))


(define (run-times n proc prev)
  (if (= n 0)
    prev
    (run-times (1- n)
               proc
               (proc prev))))


(define (element-frequencies pair-freqs)
  (fold
    (lambda (pair-freq prev)
      (let ((element (car (car pair-freq))))
        (if (eq? element 'start)
          prev
          (seq+ (cdr pair-freq) prev element))))
    '()
    pair-freqs))


(define (cmp-freq a b)
  (< (cdr a) (cdr b)))


(define (max-min lst)
  (let ((sorted-lst (sort lst cmp-freq)))
    (- (cdr (last sorted-lst))
       (cdr (first sorted-lst)))))


(define (run-test)
  (receive
    (pair-insertion-rules initial-seq)
    (call-with-input-file "input" read-input)
    (values
      (max-min
        (element-frequencies
          (run-times 10
                     (cut run-rules <> pair-insertion-rules)
                     (make-pairs initial-seq))))
      (max-min
        (element-frequencies
          (run-times 40
                     (cut run-rules <> pair-insertion-rules)
                     (make-pairs initial-seq)))))))
