(use-modules (srfi srfi-1)
             (ice-9 textual-ports))


(define (read-file-as-lines filename)
  (delete ""
          (string-split
            (call-with-input-file
              filename
              (lambda (port)
                (get-string-all port)))
            #\newline)))


(define (string->command str)
  (let ((strs (string-split str #\space)))
    (cons (string->symbol (car strs))
          (string->number (cadr strs)))))


(define (read-commands-from-file filename)
  (map string->command
       (read-file-as-lines filename)))


(define input (read-commands-from-file "input"))


(define (cmd-delta cmd)
  (case (car cmd)
    ((forward) (cons (cdr cmd) 0))
    ((up) (cons 0 (- (cdr cmd))))
    ((down) (cons 0 (cdr cmd)))))


(define (sum-pair a b)
  (cons
    (+ (car a) (car b))
    (+ (cdr a) (cdr b))))


(define (sum-positions input)
  (fold sum-pair
        (cons 0 0)
        (map cmd-delta input)))


(let ((final-pos (sum-positions input)))
  (format #t "Part 1: ~a\n" (* (car final-pos) (cdr final-pos))))
