(use-modules
  (srfi srfi-1)
  (ice-9 receive)
  (ice-9 textual-ports))


(define (read-input-rec port so-far)
  (let ((line (get-line port)))
    (if (eof-object? line)
      so-far
      (read-input-rec port
                      (cons (string->list line)
                            so-far)))))


(define (read-input port)
  (read-input-rec port '()))


(define (open-bracket? a)
  (or
    (eq? a #\<)
    (eq? a #\[)
    (eq? a #\()
    (eq? a #\{)))


(define (brackets-match? a b)
  (or

    (and (char=? a #\<)
         (char=? b #\>))

    (and (char=? a #\[)
         (char=? b #\]))

    (and (char=? a #\()
         (char=? b #\)))

    (and (char=? a #\{)
         (char=? b #\}))))


(define (first-wrong-bracket-rec l open-list)
  (cond

    ((nil? l) open-list)

    ((open-bracket? (car l))
     (first-wrong-bracket-rec (cdr l)
                              (cons (car l)
                                    open-list)))

    ((brackets-match? (car open-list)
                      (car l))
     (first-wrong-bracket-rec (cdr l)
                              (cdr open-list)))

    (else (car l))))


(define (first-wrong-bracket l)
  (first-wrong-bracket-rec l '()))


(define (bracket->score c)
  (cond
    ((char=? c #\)) 3)
    ((char=? c #\]) 57)
    ((char=? c #\}) 1197)
    ((char=? c #\>) 25137)
    (else 0)))


(define (list-sum c)
  (reduce + #f c))


(define (score-to-close c)
  (cond
    ((char=? c #\() 1)
    ((char=? c #\[) 2)
    ((char=? c #\{) 3)
    ((char=? c #\<) 4)
    (else #f)))


(define (close-expr e)
  (fold
    (lambda (v prev)
      (+ (score-to-close v)
         (* 5 prev)))
    0 e))


(define (median l)
  (let ((sorted (sort l >)))
    (list-ref sorted
              (/ (- (length sorted) 1) 2))))


(define (run-test)
  (let ((input (call-with-input-file "input" read-input)))
    (receive
      (wrong-brackets brackets-left-open)
      (partition char?
                 (map first-wrong-bracket input))
      (values
        (list-sum
          (map bracket->score wrong-brackets))
        (median
          (map close-expr brackets-left-open))))))
