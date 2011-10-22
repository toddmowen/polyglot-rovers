;;; bearings

(define N '(0 . 1))
(define E '(1 . 0))
(define S '(0 . -1))
(define W '(-1 . 0))

(define (compass-bearing b)
  (cond ((equal? b '(0 . 1)) 'N)
        ((equal? b '(1 . 0)) 'E)
        ((equal? b '(0 . -1)) 'S)
        ((equal? b '(-1 . 0)) 'W)))


;;; 'rover' structure

(define-structure (rover (constructor rover)) x y bearing)

; return unevaluated code that calls the rover constructor
(define (deconstruct-rover r)
  (list 'rover (rover-x r) (rover-y r) (compass-bearing (rover-bearing r))))

(define (unparse-rover unparser-state r)
  (with-current-unparser-state unparser-state
    (lambda (out) (write (deconstruct-rover r) out))))

(set-record-type-unparser-method! rtd:rover unparse-rover)


;;; transformations

(define (M r)
  (let ((bx (car (rover-bearing r)))
        (by (cdr (rover-bearing r))))
    (rover (+ bx (rover-x r)) (+ by (rover-y r)) (rover-bearing r))))

(define (L r)
  (let ((bx (car (rover-bearing r)))
        (by (cdr (rover-bearing r))))
    (rover (rover-x r) (rover-y r) (cons (- by) bx))))

(define (R r)
  (let ((bx (car (rover-bearing r)))
        (by (cdr (rover-bearing r))))
    (rover (rover-x r) (rover-y r) (cons by (- bx)))))


;;; top-level 'run-rovers' function:
;;; (run-rovers x y (list (cons (rover x y N) (list c o m m a n d s))
;;;                       (cons (rover x y N) (list c o m m a n d s)) ...))
;;;
;;; we need to learn about macros in order to make this call prettier!

(define (run-rovers xbound ybound rovers&commands)
  (define (do-command r command)
    (let ((r_ (command r)))
      (if (or (< (rover-x r_) 0)
              (> (rover-x r_) xbound)
              (< (rover-y r_) 0)
              (> (rover-y r_) ybound))
          (error "Rover fell off plateau (i.e. out of bounds)"))
      r_))
  (define (run-rover rover&commands)
    (let ((r (car rover&commands))
          (commands (cdr rover&commands)))
      (fold-left do-command r commands)))
  (map run-rover rovers&commands))


;;; test

(newline)
(display
 (run-rovers
  5 5
  (list
   (cons (rover 1 2 N)
         (list L M L M L M L M M))
   (cons (rover 3 3 E)
         (list M M R M M R M R R M)))))

; expected output:
; ((rover 1 3 n) (rover 5 1 e))
