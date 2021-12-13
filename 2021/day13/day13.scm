(use-modules
  (ice-9 receive)
  (srfi srfi-26)
  (srfi srfi-1)
  (ice-9 textual-ports))


(define (blank-line? l)
  (string= l ""))


(define (read-points port so-far)
  (let ((line (get-line port)))
    (if (or
          (eof-object? line)
          (blank-line? line))
      so-far
      (read-points port
                   (cons
                     (map string->number
                          (string-split line #\,))
                     so-far)))))


(define (string->fold str)
  (let ((spec (string-split
                (caddr
                  (string-split str #\space))
                #\=)))
    (cons (string->symbol (car spec))
          (string->number (cadr spec)))))


(define (read-folds port so-far)
  (let ((line (get-line port)))
    (if (eof-object? line)
      so-far
      (read-folds port
                  (cons
                    (string->fold line)
                    so-far)))))


(define (read-input port)
  (let ((points (read-points port '())))
    (let ((folds (read-folds port '())))
      (values points (reverse folds)))))


(define (do-fold dir pos point)
  (list
    (if (and (eq? dir 'x)
             (> (car point) pos))
      (- (* 2 pos) (car point))
      (car point))
    (if (and (eq? dir 'y)
             (> (cadr point) pos))
      (- (* 2 pos) (cadr point))
      (cadr point))))
  

(define (coords-equal c1 c2)
  (and
    (= (car c1) (car c2))
    (= (cadr c1) (cadr c2))))
        

(define (apply-fold points the-fold)
  (delete-duplicates
    (map
      (cut do-fold (car the-fold) (cdr the-fold) <>)
      points)
    coords-equal))


(define (apply-folds points folds)
  (if (nil? folds) 
    points
    (let ((new-points (apply-fold points
                                  (car folds))))
      (apply-folds new-points
                   (cdr folds)))))


(define (run-test)
  (receive
    (points folds)
    (call-with-input-file "input" read-input)
    (values
      (length
        (apply-fold points (car folds)))
      (apply-folds points folds))))
