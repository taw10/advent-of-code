(use-modules
  (srfi srfi-1)
  (srfi srfi-26)
  (ice-9 receive)
  (ice-9 textual-ports))


(define (read-input-rec port so-far)
  (let ((line (get-line port)))
    (if (eof-object? line)
      so-far
      (read-input-rec port
                      (cons
                        (map string->symbol
                             (string-split line #\-))
                        so-far)))))


(define (read-input port)
  (read-input-rec port '()))


(define (unique-items l)
  (apply lset-adjoin eq? '() l))


(define (where-can-i-go connections cave)
  (map cadr
       (filter
         (lambda (connection)
           (eq? (car connection) cave))
         connections)))


(define (add-reverse-connections l)
  (append l (map (lambda (n)
                 (list (cadr n)
                       (car n))) l)))


(define (connections->exit-list connections)
  (let ((caves (unique-items
                 (apply append connections)))
        (conn (add-reverse-connections connections)))
    (map
      (lambda (cave)
        (cons cave
              (where-can-i-go conn cave)))
      caves)))


(define (small-cave? cave)
  (char-lower-case?
    (car
      (string->list
        (symbol->string cave)))))


(define paths '())

(define (valid-part1-path? path)
  (let ((last-cave (car path)))
    (if (small-cave? last-cave)
      (if (memq last-cave (cdr path))
        #f
        #t)
      #t)))


(define (first-repeated-cave path)
  (if (nil? path)
    #f
    (if (memq (car path) (cdr path))
      (car path)
      (first-repeated-cave (cdr path)))))


(define (max-1-double? path)
  (let ((first-rep (first-repeated-cave path)))
    (receive
      (first-rep-instances others)
      (partition (cut eq? first-rep <>) path)
      (or
        (not first-rep)
        (and
          (= (length first-rep-instances) 2)
          (not (first-repeated-cave others)))))))


(define (valid-part2-path? path)
  (max-1-double? (filter small-cave? path)))


(define (find-all-paths path-so-far end exit-list valid-path?)
  (when (valid-path? path-so-far)
    (for-each
      (lambda (ex)
        (unless (eq? ex 'start)
          (if (eq? ex end)
            (set! paths (cons (reverse (cons end path-so-far)) paths))
            (find-all-paths (cons ex path-so-far)
                            end
                            exit-list
                            valid-path?))))
      (assq-ref exit-list
                (car path-so-far)))))


(define (run-test)
  (let ((connections (call-with-input-file "input" read-input)))

    (set! paths '())
    (find-all-paths
      (list 'start)
      'end
      (connections->exit-list connections)
      valid-part1-path?)
    (format #t "Part 1: ~a\n" (length paths))

    (set! paths '())
    (find-all-paths
      (list 'start)
      'end
      (connections->exit-list connections)
      valid-part2-path?)
    (format #t "Part 2: ~a\n" (length paths))))
