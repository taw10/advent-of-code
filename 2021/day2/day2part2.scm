(use-modules (srfi srfi-1)
             (ice-9 textual-ports)
             (srfi srfi-9))


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


(define-record-type <sub-state>
  (sub-state x-pos
             depth
             aim)
  sub-state?
  (x-pos   x-pos)
  (depth   depth)
  (aim     aim))


(define (sum-positions input)
  (fold
    (lambda (cmd-pair state)
      (let ((cmd (car cmd-pair))
            (val (cdr cmd-pair)))
        (cond
          ((eq? cmd 'forward)
           (sub-state (+ (x-pos state) val)
                      (+ (depth state) (* val (aim state)))
                      (aim state)))

          ((eq? cmd 'down)
           (sub-state (x-pos state)
                      (depth state)
                      (+ (aim state) val)))

          ((eq? cmd 'up)
           (sub-state (x-pos state)
                      (depth state)
                      (- (aim state) val))))))
    (sub-state 0 0 0)
    input))


(let ((final-pos (sum-positions input)))
  (format #t "Part 2: ~a\n" (* (x-pos final-pos) (depth final-pos))))
