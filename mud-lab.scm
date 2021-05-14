(define-module (mud-lab)
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-64))

(define inc
  (λ (x) (+ x 1)))

(define dec
  (λ (x) (- x 1)))

(define (make-objects-string lst)
  (if (> (length lst) 1)
      (string-join lst ", ")
      (car lst)))

(define-public make-player
  (lambda (nm)
    (let ((invent '())
          (loc (cons 0 0)))
      (lambda (x . args) (cond ((eq? x 'name) nm)
                               ((eq? x 'inventory) invent)
                               ((eq? x 'add-to-inventory) (set! invent (cons (car args) invent)))
                               ((eq? x 'location) loc)
                               ((eq? x 'set-location!) (set! loc (car args)))
                               ((eq? x 'remove-from-inventory) (set! invent (delete (car args) invent))))))))

(define-public location-of
  (λ (input)
    (input 'location)))

(define loc-north
  (λ (location)
    (car location)))

(define loc-east
  (λ (location)
    (cdr location)))

(define update-location-of
  (λ (player north east)
    (let ((loc (location-of player)))
      (player 'set-location! (cons (north (loc-north loc))
                                   (east (loc-east loc)))))))

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
    (x 'name)))

(define-public description-of
  (λ (x)
    (x 'description)))

(define exits-of
  (λ (x)
    (x 'exits)))

(define-public inventory-of
  (λ (x)
    (x 'inventory)))

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
      (player 'add-to-inventory object))))

(define-public make-room
  (lambda (nm descr loc exts . objs)
    (lambda (x . args) (cond ((eq? x 'name) nm)
                             ((eq? x 'description) descr)
                             ((eq? x 'location) loc)
                             ((eq? x 'exits) exts)
                             ((eq? x 'objects) objs)
                             ((eq? x 'add-object) (set! objs (cons (car args) objs)))
                             ((eq? x 'delete-object) (set! objs (delete (car args) objs)))))))

(define-public get-location-of
  (λ (player rooms)
    (if (equal? (location-of player) (location-of (car rooms)))
	      (car rooms)
	      (get-location-of player (cdr rooms)))))

(define-public display-location-of
  (λ (player rooms)
    (description-of (get-location-of player rooms))))

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
	        (cond ((equal? cmd "go") (begin (go-to (cadr input)
                                                 player
                                                 rooms)
					                                (print-location-of player rooms)
					                                (loop (get-a-lot-of-input))))
		            ((equal? cmd "look") (begin (print-location-of player rooms)
					                                  (loop (get-a-lot-of-input))))
		            ((or (equal? cmd "i")
                     (equal? cmd "inventory")) (begin (display-inventory-of player)
					                                            (newline)
					                                            (loop (get-a-lot-of-input))))
		            ((equal? cmd "take") (begin (take (cadr input)
                                                  player
                                                  (get-location-of player rooms))
                                            (newline)
					                                  (loop (get-a-lot-of-input))))
		            ((equal? cmd "drop") (begin (drop (cadr input)
                                                  (get-location-of player rooms)
                                                  player)
                                            (newline)
					                                  (loop (get-a-lot-of-input))))
		            ((equal? cmd "read") (begin (read-object (cadr input)
                                                         player
                                                         (get-location-of player rooms))
                                            (newline)
					                                  (loop (get-a-lot-of-input))))
		            ((equal? cmd "say") (begin (display (string-join (cdr input)
                                                                 " "))
                                           (newline)
					                                 (loop (get-a-lot-of-input))))
		            ((equal? cmd "exit") (display "Goodbye!\n"))
		            (else (begin (display "I don't know how to do that.\n")
			                       (loop (get-a-lot-of-input))))))))
    (print-location-of player rooms)
    (loop (get-a-lot-of-input))))

(define get-a-lot-of-input
  (λ ()
    (let ((input (readline)))
      (string-tokenize input))))

(define (test-and-print what fail success)
  (if (null? what)
      (display failure)
      (display (string-join
                (list success
                      (make-objects-string
                       what)) " "))))

(define display-exits-of
  (λ (player rooms)
    (test-and-print (exits-of (get-location-of player rooms))
                    "You can't get out!"
                    "The exits are:")))

(define display-objects-of
  (λ (player rooms)
    (let ((objects (objects-of (get-location-of player rooms))))
      (if (null? objects)
          (display "There's nothing here.")
          (display (string-join
                    (list "There is"
                          (make-objects-string
                           (map (lambda (obj) (obj 'name-with-article))
                                objects))) " "))))))

(define display-inventory-of
  (λ (player)
    (display (string-join
              (list "You have"
                    (make-objects-string 
                     (map (lambda (obj) (obj 'name-with-article))
                          (inventory-of player)))) " "))
    (newline)))

(define take
  (λ (name player location)
    (let ((object (get-object-of-location location name)))
      (if (null? object)
	        player
	        (begin (add-to-inventory-of player object)
                 (location 'delete-object object)
                 (display (string-concatenate (list "You take the " (name-of object)))))))))

(define get-object-of-location
  (λ (location name)
    (get-object-from (objects-of location) name)))

(define objects-of
  (λ (location)
    (location 'objects)))

(define get-object-from
  (λ (objects name)
    (cond ((null? objects) '())
	        ((equal? (name-of (car objects)) name) (car objects))
	        (else (get-object-from (cdr objects) name)))))

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
		             (player 'remove-from-inventory object)
                 (display (string-concatenate (list "You drop the " (name-of object)))))))))

(define check-for-object
  (λ (objects name)
    (cond ((null? objects) #f)
	        ((equal? (name-of (car objects)) name) #t)
	        (else (check-for-object (cdr objects) name)))))

(define add-to-objects-of
  (λ (location object)
    (location 'add-object object)))

(define read-object
  (λ (name player location)
    (let ((object (cond ((check-for-object (inventory-of player) name)
                         (get-object-from-inventory-of player name))
			                  ((check-for-object (objects-of location) name)
                         (get-object-of-location location name))
			                  (else '()))))
      (if (null? object)
	        #f
	        (object 'read)))))
