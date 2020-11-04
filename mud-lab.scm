(define-module (mud-lab)
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-64))

(define inc
  (λ (x) (+ x 1)))

(define dec
  (λ (x) (- x 1)))

(define-public make-player
  (λ (name)
    (list (cons "name" name)
	  (cons "inventory" '())
	  (cons "location" (cons 0 0)))))

(define location-of
  (λ (input)
    (cdr (assoc "location" input))))

(define loc-north
  (λ (location)
    (car location)))

(define loc-east
  (λ (location)
    (cdr location)))

(define update-location-of
  (λ (player north east)
    (let ((loc (location-of player)))
      (update-player player "location" (λ () (cons (north (loc-north loc))
						   (east (loc-east loc))))))))

(define north
  (λ (player)
    (update-location-of player inc identity)))

(define south
  (λ (player)
    (update-location-of player dec identity)))

(define east
  (λ (player)
    (update-location-of player identity inc)))

(define west
  (λ (player)
    (update-location-of player identity dec)))

(define name-of
  (λ (x)
    (cdr (assoc "name" x))))

(define description-of
  (λ (x)
    (cdr (assoc "description" x))))

(define exits-of
  (λ (x)
    (cdr (assoc "exits" x))))

(define-public inventory-of
  (λ (x)
    (cdr (assoc "inventory" x))))

(define move
  (λ (player direction)
    (cond ((equal? direction "north") (north player))
	  ((equal? direction "south") (south player))
	  ((equal? direction "east") (east player))
	  ((equal? direction "west") (west player)))))

(define check-for-exits
  (λ (location direction player)
    (define recur
      (λ (l)
	(cond ((null? l) #f)
	      ((equal? (car l) direction) #t)
	      (else (recur (cdr l))))))
    (recur (exits-of location))))

(define-public go-to
  (λ (direction player rooms)
    (cond ((null? rooms) player)
	  ((check-for-exits (get-location-of player rooms) direction player) (move player direction)))))

(define-public add-to-inventory-of
  (λ (player object)
    (let ((inv (inventory-of player)))
      (update-player player "inventory" (λ () (cons object inv))))))

(define-public make-room
  (λ (name description loc exits)
    (list (cons "name" name)
	  (cons "description" description)
	  (cons "location" loc)
	  (cons "objects" '())
	  (cons "exits" exits))))

(define get-location-of
  (λ (player rooms)
    (if (equal? (location-of player) (location-of (car rooms)))
	(car rooms)
	(get-location-of player (cdr rooms)))))

(define-public display-location-of
  (λ (player rooms)
    (if (equal? (location-of player) (location-of (car rooms)))
	(description-of (car rooms))
	(display-location-of player (cdr rooms)))))

(define-public print-location-of
  (λ (player rooms)
    (display (display-location-of player rooms))
    (newline)
    (display-exits-of player rooms)
    (newline)
    (display-objects-of player rooms)
    (newline)))

(define-public game
  (λ (player rooms)
    (define loop
      (λ (input)
	(let ((cmd (car input)))
	  (cond ((equal? cmd "go") (begin (go-to (cadr input) player rooms)
					  (print-location-of player rooms)
					  (loop (get-a-lot-of-input))))
		((equal? cmd "look") (begin (print-location-of player rooms)
					    (loop (get-a-lot-of-input))))
		((equal? cmd "i") (begin (display-inventory-of player)
					 (newline)
					 (loop (get-a-lot-of-input))))
		((equal? cmd "take") (begin (take (cadr input) player (get-location-of player rooms))
					    (loop (get-a-lot-of-input))))
		((equal? cmd "drop") (begin (drop (cadr input) (get-location-of player rooms) player)
					    (loop (get-a-lot-of-input))))
		((equal? cmd "exit") (display "Goodbye!\n"))))))
    (loop (get-a-lot-of-input))))

(define get-a-lot-of-input
  (λ ()
    (let ((input (readline)))
      (string-tokenize input))))

(define display-exits-of
  (λ (player rooms)
    (display (exits-of (get-location-of player rooms)))))

(define display-objects-of
  (λ (player rooms)
    (display (map name-of (objects-of (get-location-of player rooms))))))

(define display-inventory-of
  (λ (player)
    (display (map name-of (inventory-of player)))))

(define take
  (λ (name player location)
    (let ((object (get-object-of-location location name)))
      (if (null? object)
	  player
	  (begin (add-to-inventory-of player object)
		 (set! location (assoc-set! location "objects" (delete object (objects-of location)))))))))

(define get-object-of-location
  (λ (location name)
    (get-object-from (objects-of location) name)))

(define objects-of
  (λ (location)
    (cdr (assoc "objects" location))))

(define get-object-from
  (λ (objects name)
    (cond ((null? objects) '())
	  ((equal? (name-of (car objects)) name) (car objects))
	  (else get-object-from (cdr objects)))))

(define get-object-from-inventory-of
  (λ (player name)
    (get-object-from (inventory-of player) name)))

(define drop
  (λ (name location player)
    (let ((object (get-object-from-inventory-of player name))
	  (inv (inventory-of player)))
      (if (null? object)
	  player
	  (begin (add-to-objects-of location object)
		 (update-player player "inventory" (λ () (delete object inv))))))))


(define add-to-objects-of
  (λ (location object)
    (set! location (assoc-set! location "objects" (cons object (objects-of location))))))

(define update-player
  (λ (player property f)
    (set! player (assoc-set! player property (f)))))
