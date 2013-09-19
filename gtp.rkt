#lang racket

(require rackunit)

;list utils
(define (transpose-lists lists) (apply map list lists))
(define coords (sequence->list (in-range 3)))

(define (change-nth n l new)
  (cond ((< n 0) l)
        ((equal? n 0) (cons new (cdr l)))
        (else (cons (car l) (change-nth (- n 1) (cdr l) new)))))

(define (change-n-first n l f-new)
  (if (<= n 0)
      l
      (append (map f-new (sequence->list (in-range n))) (list-tail l n))))

(check-equal?
 (change-n-first 3 '(0 0 0 0 0) (lambda (n) n))
 '(0 1 2 0 0)
 "change-n-first")

(define (n-times n x)
  (build-list n (lambda (i) x)))

(check-equal?
 (n-times 3 'x)
 '(x x x)
 "n-times")

; functional utils
(define (compose-power f power)
  (apply compose (n-times power f)))


;transforms
;quarter turns
(define (qx pt) (make-point (point-x pt) (-(point-z pt)) (point-y pt)))
(define (qy pt) (make-point (-(point-z pt)) (point-y pt) (point-x pt)))
(define (qz pt) (make-point (-(point-y pt)) (point-x pt) (point-z pt)))
(define quarters (list qx qy qz))

;transformer
; a transformer is made of a rotor containing the powers applied to quarter turns
; and a shiftor = translation vector
(define (transformer-rotor transformer) (car transformer))
(define (transformer-shiftor transformer) (cadr transformer))
(define (make-transformer rotor shiftor) (list rotor shiftor))
         
         
; a point is a 3-uple impemented as a 3 elements list
(define (make-point x y z) (list x y z))
(define (point-x pt) (car pt))
(define (point-y pt) (cadr pt))
(define (point-z pt) (caddr pt))
(define (point-coord pt n) (list-ref pt n))
(define (point-shift pt shiftor) (map + pt shiftor))
(define (point-rotate pt rotor)
  ((apply compose (map compose-power quarters rotor))
   pt))
(define (point-move pt transformer)
  (point-shift (point-rotate pt (transformer-rotor transformer)) 
               (transformer-shiftor transformer)))

(check-equal?
 (point-rotate
  (make-point 1 0 0)
  '(1 0 0))
 (make-point 1 0 0)
 "point-rotate")
(check-equal? (point-rotate (make-point 1 0 0) '(0 1 0)) 
              (make-point 0 0 1) "point-rotate")
(check-equal? (point-rotate (make-point 1 0 0) '(0 0 1)) 
              (make-point 0 1 0) "point-rotate")
(check-equal? (point-rotate (make-point 1 0 0) '(0 2 0)) 
              (make-point -1 0 0) "point-rotate")
(check-equal? (point-rotate (make-point -1 0 0) '(0 0 1)) 
              (make-point 0 -1 0 ) "point-rotate")
(check-equal? (point-rotate (make-point 1 0 0) '(0 1 2)) 
              (make-point 0 0 -1) "point-rotate")
(check-equal? (point-rotate (make-point 1 0 0) '(1 1 2)) 
              (make-point 0 1 0) "point-rotate")
     
;boxes
;intervals
(define (make-interval lower upper) (list lower upper))
(define (interval-lower i) (car i))
(define (interval-upper i) (cadr i))
;boxes
(define (make-box ix iy iz) (list ix iy iz))
(define (box-min-corner box) (map car box))
(define (box-max-corner box) (map cadr box))
(define (box-lower-corner box) (map car box))
(define (box-upper-corner box) (map cadr box))
(define (box-interval box coord) (list-ref box coord))

;polyminos implemented as lists of points
(define (make-mino . pts) pts)
(define (mino-points mino) mino)
(define (transform-mino t mino) (map t mino))
(define (mino-intersect mino-left mino-right)
    (ormap
     (lambda (pt-right)
       (ormap
        (lambda (pt-left)
          (equal? pt-right pt-left))
        (mino-points mino-left)))
     (mino-points mino-right)))

(check-equal? 
 (mino-intersect
  (make-mino (make-point 0 0 0) (make-point 0 0 1))
  (make-mino (make-point 0 0 0) (make-point 0 1 0)))
 #t
 "mino-intersect")
(check-equal? 
 (mino-intersect
  (make-mino (make-point 0 0 2) (make-point 0 0 1))
  (make-mino (make-point 0 0 0) (make-point 0 1 0)))
 #f
 "mino-intersect")

(define (mino-rotate mino rotor)
  (map (lambda (point) (point-rotate point rotor)) mino)) 

(check-equal? 
 (mino-rotate 
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  '(0 0 1))
 (make-mino (make-point 0 1 1) (make-point 0 0 1) (make-point -1 1 1) (make-point -2 1 1))
 "mino-rotate")

(check-equal?
 (mino-rotate 
  (make-mino 
   (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  '(1 0 0))
 (make-mino (make-point 1 -1 0) (make-point 0 -1 0) (make-point 1 -1 1) (make-point 1 -1 2))
 "mino-rotate")

(define (mino-shift mino shiftor)
  (map (lambda (point) (point-shift point shiftor)) mino))

(check-equal? 
 (mino-shift 
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  '(0 1 2))
 (make-mino (make-point 1 1 3) (make-point 0 1 3) (make-point 1 2 3) (make-point 1 3 3))
 "mino-shift")

(define (mino-transform mino transformer)
  (mino-shift 
   (mino-rotate mino (transformer-rotor transformer))
   (transformer-shiftor transformer)))

(check-equal? 
 (mino-transform 
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-transformer '(1 0 0) '(0 1 2)))
 (make-mino (make-point 1 0 2) (make-point 0 0 2) (make-point 1 0 3) (make-point 1 0 4))
 "mino-transform")


(define (mino-box mino)
  (map
   (lambda (l)
     (make-interval (apply min l) (apply max l)))
   (transpose-lists mino)))

(check-equal?
 (mino-box 
  (make-mino (make-point 0 0 0) (make-point 0 0 1) (make-point 1 0 0) (make-point 0 1 0)))
 (make-box (make-interval 0 1) (make-interval 0 1) (make-interval 0 1))
 "mino-box")

(define (mino-interval mino coord)
  (let ((coords (point-coord (transpose-lists mino) coord)))
    (make-interval  (apply min coords) (apply max coords))))

(check-equal?
 (mino-interval
  (make-mino (make-point 0 0 0) (make-point 0 0 1) (make-point 1 0 0) (make-point 0 1 0) (make-point 0 2 0))
  1)
 (make-interval 0 2)
 "mino-interval")

;(define (mino-init-shiftor mino gamebox) 
;  (map - (box-min-corner gamebox) (box-min-corner (mino-box mino))))

(define (interval-overstep? overstepper overstepped)
  (> (interval-upper overstepper) (interval-upper overstepped)))

;(define (mino-init-shiftor-1 mino game-interval coord)
;  (let ((res (- (interval-lower game-interval)
;                (interval-lower (mino-interval mino coord)))))
;        (if (> (interval-upper (mino-interval mino coord)) 
;               (interval-upper game-interval))
;            '()
;            res)))


(define (mino-init-shiftor-1 mino game-interval coord)
  (let ((mi (mino-interval mino coord)))
    (if (interval-overstep? mi game-interval)
        null
        (- (interval-lower game-interval)
           (interval-lower mi)))))

(check-equal? 
 (mino-init-shiftor-1
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '(0 0) 0)
 '()
 "mino-init-shiftor-1")

(check-equal?
 (mino-init-shiftor-1
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-interval 0 2)
  2)
 -1
 "mino-init-shiftor-1")

(define (mino-init-shiftor mino gamebox)
  (let ((res 
         (map 
          (lambda (coord) 
            (mino-init-shiftor-1 mino (box-interval gamebox coord) coord))
          coords)))
    (if (member null res)
        null
        res)))

(check-equal? 
 (mino-init-shiftor
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '((0 0) (0 0) (0 1)))
 '()
 "mino-init-shiftor")

(check-equal?
 (mino-init-shiftor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2)))
 '(0 0 -1)
 "mino-init-shiftor")

(define (next-rotor rotor)
  (define (aux rotor coord)
    (if (> coord 2)
        null
        (let ((power (point-coord rotor coord)))
          (if (< power 3)
              (change-n-first coord 
                              (change-nth coord rotor (+ 1 power))
                              (lambda (n) 0))
              (aux rotor (+ coord 1))))))
  (aux rotor 0))

(check-equal? (next-rotor '(2 1 2)) '(3 1 2) "next-rotor")
(check-equal? (next-rotor '(3 0 0)) '(0 1 0) "next-rotor")

(define (mino-next-rotor current-rotor mino gamebox)
  (let ((next-rotor (next-rotor current-rotor)))
    (cond ((null? next-rotor) null)
          ((not (null? (mino-init-shiftor (mino-rotate mino next-rotor) gamebox)))
            next-rotor)
          (else (mino-next-rotor next-rotor mino gamebox)))))

(check-equal? 
 (mino-next-rotor
  '(0 0 0)
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '((0 0) (0 0) (0 1)))
 '(0 1 0)
 "mino-next-rotor")
  
(check-equal? 
 (mino-next-rotor
  '(3 3 3)
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '((0 0) (0 0) (0 1)))
 null
 "mino-next-rotor")

(define (mino-init-rotor mino gamebox)
    (if (not (null? (mino-init-shiftor mino gamebox)))
        '(0 0 0)
        (mino-next-rotor '(0 0 0) mino gamebox)))

(check-equal? 
 (mino-init-rotor
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '((0 0) (0 0) (0 1)))
 '(0 1 0)
 "mino-init-rotor")

(check-equal?
 (mino-init-rotor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2)))
 '(0 0 0)
 "mino-init-rotor")

;(define (mino-init-transformer mino gamebox)
;  (let* ((init-shiftor (mino-init-shiftor mino gamebox))
;         (init-transformer (make-transformer '(0 0 0) init-shiftor)))
;    (if (not (null? init-shiftor))
;        init-transformer
;        (mino-next-transformer 
;         mino gamebox       
;         (make-transformer '(0 0 0)
;                           (map - (box-upper-corner gamebox) 
;                                (box-lower-corner (mino-box mino))))))))


(define (mino-next-shiftor-1 mino gamebox coord current)
  (let ((next (+ current 1)))
    (if (> (+ (interval-upper (mino-interval mino coord)) next) 
           (point-coord (box-upper-corner gamebox) coord))
        '()
        next)))

(check-equal?
 (mino-next-shiftor-1
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 3) (make-interval 0 3) (make-interval 0 3))
  1
  -1)
 0
 "mino-next-shiftor-1")

(check-equal?
 (mino-next-shiftor-1
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 3) (make-interval 0 3) (make-interval 0 3))
  1
  1)
 null
 "mino-next-shiftor-1")

(define (mino-next-shiftor-aux mino gamebox coord current)
  (if (> coord 2)
      null
      (let ((next-1 (mino-next-shiftor-1 mino gamebox coord (point-coord current coord))))
        (if (not (null? next-1))
            (let ((next (change-n-first 
                         coord
                         (change-nth coord current next-1)
                         (lambda (lcoord) (mino-init-shiftor-1 mino (box-interval gamebox lcoord) lcoord)))))
              (if (member '() next)
                  '()
                  next))
            (mino-next-shiftor-aux mino gamebox (+ coord 1) current)))))

(check-equal?
 (mino-next-shiftor-aux
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2))
  2
  '(1 1 0))
 '(0 0 1)
 "mino-next-shiftor-aux")

(check-equal?
 (mino-next-shiftor-aux '((0 0 0) (-1 0 0) (-1 1 0) (-2 1 0)) '((0 2) (0 2) (0 2)) 2 '(2 1 0))
 '(2 0 1)
 "mino-next-shiftor-aux")

(check-equal?
 (mino-next-shiftor-aux 
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  '((0 2) (0 2) (0 2)) 2 '(1 0 0))
 '(0 0 1)
 "mino-next-shiftor-aux")

(define (mino-next-shiftor mino gamebox current)
  (mino-next-shiftor-aux mino gamebox 0 current))

(check-equal?
 (mino-next-shiftor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2))
  '(0 0 -1))
 '(1 0 -1)
 "mino-next-shiftor")

(check-equal?
 (mino-next-shiftor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2))
  '(1 0 -1))
 '(0 0 0)
 "mino-next-shiftor")

(check-equal?
 (mino-next-shiftor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2))
  '(1 1 0))
 '(0 0 1)
 "mino-next-shiftor")

(check-equal?
 (mino-next-shiftor
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 2) (make-interval 0 2) (make-interval 0 2))
  '(1 1 1))
 null
 "mino-next-shiftor")


(check-equal?
 (mino-next-shiftor '((0 0 0) (-1 0 0) (-1 1 0) (-2 1 0)) '((0 2) (0 2) (0 2)) '(2 1 0))
 '(2 0 1)
 "mino-next-shiftor")
 

(define (mino-next-transformer mino gamebox transformer)
  (let* ((rotor (transformer-rotor transformer))
         (rotated (mino-rotate mino rotor))
         (next-shiftor 
          (mino-next-shiftor rotated gamebox (transformer-shiftor transformer))))
    (if (not (null? next-shiftor))
        (make-transformer  rotor next-shiftor)
        (let ((next-rotor (mino-next-rotor rotor mino gamebox)))
          (if (null? next-rotor)
              null
              (make-transformer 
               next-rotor 
               (mino-init-shiftor (mino-rotate mino next-rotor) gamebox)))))))

(check-equal?
 (mino-next-transformer
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
  (make-box (make-interval 0 3) (make-interval 0 3) (make-interval 0 3))
  (make-transformer '(0 0 0) '(2 1 2)))
 (make-transformer '(1 0 0) '(0 1 0))
 "mino-next-transformer")

(check-equal?
 (mino-next-transformer '((0 0 0) (1 0 0) (1 1 0) (2 1 0)) '((0 2) (0 2) (0 2)) '((0 2 0) (2 1 0)))
 '((0 2 0) (2 0 1))
 "mino-next-transformer")

(check-equal?
 (mino-next-transformer '((0 0 0) (1 0 0) (1 1 0) (2 1 0)) '((0 2) (0 2) (0 2)) '((0 2 0) (2 2 1)))
 '((0 2 0) (2 0 2))
 "mino-next-transformer")

(check-equal?
 (mino-next-transformer 
  (make-mino (make-point 0 0 0) (make-point 1 0 0)) '((0 0) (0 0) (0 1)) '((0 0 0) (1 0 0)))
 '((0 1 0) (0 0 0))
 "mino-next-transformer")
 

(define (mino-init-transformer mino gamebox)
  (let ((init-rotor (mino-init-rotor mino gamebox)))
    (make-transformer 
     init-rotor 
     (mino-init-shiftor (mino-rotate mino init-rotor) gamebox))))

(check-equal? 
 (mino-init-transformer
  (make-mino (make-point 0 0 0) (make-point 1 0 0))
  '((0 0) (0 0) (0 1)))
 '((0 1 0) (0 0 0))
 "mino-init-transformer")

(define (make-empty-puzzle dx dy dz)
  (list '(); tranformed minos
        (make-box (list  0 (- dx 1)) (list  0 (- dy 1)) (list  0 (- dz 1)))
        '(); original minos
        '())); transformers

(define (puzzle-minos puzzle)
  (car puzzle))
(define (puzzle-box puzzle)
  (cadr puzzle))

(define (puzzle-intersect-mino puzzle mino)
  (ormap 
   (lambda (puzzle-mino)
     (mino-intersect mino puzzle-mino))
   (puzzle-minos puzzle)))
 
(check-equal?
 (puzzle-intersect-mino
  (make-empty-puzzle 3 3 3)
  (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1)))
 #f
 "puzzle-intersect-mino empty-puzzle")

(define (try puzzle mino transformer)
  (not (puzzle-intersect-mino
        puzzle
        (mino-transform mino transformer))))

(define (puzzle-add-mino puzzle mino transformer)
  (list
   (cons (mino-transform mino transformer) (puzzle-minos puzzle))
   (puzzle-box puzzle)
   (cons mino (caddr puzzle))
   (cons transformer (cadddr puzzle))))

(check-equal? 
 (puzzle-minos 
  (puzzle-add-mino
   (make-empty-puzzle 3 3 3)   
   (make-mino (make-point 1 0 1) (make-point 0 0 1) (make-point 1 1 1) (make-point 1 2 1))
   (make-transformer '(1 0 0) '(0 1 2))))
 (list (make-mino (make-point 1 0 2) (make-point 0 0 2) (make-point 1 0 3) (make-point 1 0 4)))
 "puzzle-add-mino")


(define (solve puzzle minos)
  (if (null? minos)
      puzzle
      (let ((mino (car minos)))
        (solve1 puzzle (cdr minos) mino (mino-init-transformer mino (puzzle-box puzzle))))))

(define (solve1 puzzle minos mino transformer)
  (if (null? transformer)
      null
      (let ((next-transformer (mino-next-transformer mino (puzzle-box puzzle) transformer)))
        (if (try puzzle mino transformer)
            (if (null? minos)
                (puzzle-add-mino puzzle mino transformer)
                (let ((sol (solve (puzzle-add-mino puzzle mino transformer) minos)))
                  (if (not (null? sol))
                      sol
                      (solve1 puzzle minos mino next-transformer ))))
            (solve1 puzzle minos mino next-transformer)))))

(check-equal? 
 (solve (make-empty-puzzle 1 1 1) (list (make-mino (make-point 0 0 0))))
 ;'((((0 0 0))) ((0 0) (0 0) (0 0)))
 '((((0 0 0))) ((0 0) (0 0) (0 0)) (((0 0 0))) (((0 0 0) (0 0 0))))
 "solve 1")

(check-equal? 
 (solve (make-empty-puzzle 1 1 2) 
         (list
          (make-mino (make-point 0 0 0))
          (make-mino (make-point 0 0 0))))
 ;'((((0 0 1)) ((0 0 0))) ((0 0) (0 0) (0 1)))
 '((((0 0 1)) ((0 0 0))) ((0 0) (0 0) (0 1)) (((0 0 0)) ((0 0 0))) (((0 0 0) (0 0 1)) ((0 0 0) (0 0 0))))
 "solve 2x1")

(check-equal? 
 (solve (make-empty-puzzle 1 1 2) 
         (list
          (make-mino (make-point 0 0 0) (make-point 1 0 0))))
 '((((0 0 0) (0 0 1))) ((0 0) (0 0) (0 1)) (((0 0 0) (1 0 0))) (((0 1 0) (0 0 0))))
 "solve 1x2")

(check-equal? 
 (solve (make-empty-puzzle 1 2 2) 
         (list
          (make-mino (make-point 0 0 0) (make-point 1 0 0))
          (make-mino (make-point 0 0 0) (make-point 1 0 0))))
 '((((0 1 0) (0 1 1)) ((0 0 0) (0 0 1))) ((0 0) (0 1) (0 1)) (((0 0 0) (1 0 0)) ((0 0 0) (1 0 0))) (((0 1 0) (0 1 0)) ((0 1 0) (0 0 0))))
 "solve 2x2")

(check-equal? 
 (solve (make-empty-puzzle 1 2 2) 
         (list
          (make-mino (make-point 0 0 0) (make-point 1 0 0) (make-point 0 1 0))
          (make-mino (make-point 0 0 0))))
 '((((0 1 1)) ((0 0 0) (0 0 1) (0 1 0))) ((0 0) (0 1) (0 1)) (((0 0 0)) ((0 0 0) (1 0 0) (0 1 0))) (((0 0 0) (0 1 1)) ((0 1 0) (0 0 0))))
 "solve 3+1")

(check-equal? 
 (solve (make-empty-puzzle 2 2 2) 
         (list
          (make-mino (make-point 0 0 0) (make-point 1 0 0) (make-point 0 1 0) (make-point 0 0 1))
          (make-mino (make-point 0 0 0) (make-point 1 0 0) (make-point 0 1 0) (make-point 0 0 1))))
 '((((1 1 1) (1 1 0) (1 0 1) (0 1 1)) ((0 0 0) (1 0 0) (0 1 0) (0 0 1))) ((0 1) (0 1) (0 1)) (((0 0 0) (1 0 0) (0 1 0) (0 0 1)) ((0 0 0) (1 0 0) (0 1 0) (0 0 1))) (((2 1 0) (1 1 1)) ((0 0 0) (0 0 0))))
  "solve 2x2x2")
 