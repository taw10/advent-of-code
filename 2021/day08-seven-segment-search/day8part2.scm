(use-modules
  (ice-9 textual-ports)
  (ice-9 receive)
  (srfi srfi-26)
  (srfi srfi-1))


(define (char->sigsym s)
  (cond
    ((eq? s #\a) 'a)
    ((eq? s #\b) 'b)
    ((eq? s #\c) 'c)
    ((eq? s #\d) 'd)
    ((eq? s #\e) 'e)
    ((eq? s #\f) 'f)
    ((eq? s #\g) 'g)))


(define (string->sigpatt s)
  (map char->sigsym
       (string->list s)))


(define (read-input-rec port signal-patterns outputs)
  (let ((l (get-line port)))
    (if (eof-object? l)
      (values signal-patterns outputs)
      (let ((new-signal-patterns
              (map string->sigpatt (string-split (substring l 0 58) #\space)))
            (new-outputs
              (map string->sigpatt (string-split (substring l 61) #\space))))
        (read-input-rec port
                        (cons new-signal-patterns signal-patterns)
                        (cons new-outputs outputs))))))


(define (read-input port)
  (read-input-rec port '() '()))


;; Turn the list of connections into a more useful association list
(define (make-key connections)
  (map cons
       connections
       '(A B C D E F G)))


(define (segments->digit n)
  (cond
    ((lset= eq? n '(A B C E F G)) 0)
    ((lset= eq? n '(C F)) 1)
    ((lset= eq? n '(A C D E G)) 2)
    ((lset= eq? n '(A C D F G)) 3)
    ((lset= eq? n '(B C D F)) 4)
    ((lset= eq? n '(A B D F G)) 5)
    ((lset= eq? n '(A B D E F G)) 6)
    ((lset= eq? n '(A C F)) 7)
    ((lset= eq? n '(A B C D E F G)) 8)
    ((lset= eq? n '(A B C D F G)) 9)
    (else #f)))


;;           s = the signal pattern to be decoded, e.g. '(c e d a)
;; connections = the decoder key, e.g. '(b c d e g a f)
;;    (means: signal 'b lights up segment A, 'c -> 'B, etc)
;; Returns the digit being shown, or #f if it's an invalid assignment
(define (decode-signals s connections)
  (segments->digit
    (map (cute assq-ref (make-key connections) <>)
         s)))


(define (key-ok? k signal-patterns)
  (every (cut decode-signals <> k) signal-patterns))


(define (flatmap proc seq)
  (apply append (map proc seq)))

(define (permutations s)
  (if (nil? s) 
    (list '())
    (flatmap (lambda (x)
           (map (lambda (p) (cons x p))
                    (permutations (delete x s))))
         s)))


(define (digits->number-rec l so-far fac)
  (if (nil? l)
    so-far
    (digits->number-rec (cdr l)
                        (+ so-far
                           (* fac (car l)))
                        (* fac 10))))


(define (digits->number l)
  (digits->number-rec (reverse l) 0 1))


(define (decode-display signal-patterns digits)
  (let ((key (find (cut key-ok? <> signal-patterns)
                   (permutations '(a b c d e f g)))))
    (digits->number
      (map (cut decode-signals <> key)
           digits))))


(define (list-sum c)
  (reduce + #f c))


(define (part2)
  (receive
    (signal-patterns digits)
    (call-with-input-file "input" read-input)
    (list-sum
      (map decode-display signal-patterns digits))))
