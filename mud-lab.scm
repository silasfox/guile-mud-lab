(define-module (mud-lab)
  #:use-module (ice-9 readline)
  #:use-module (srfi srfi-64))

(define (inc x)
  (+ x 1))

(define (dec x)
  (- x 1))

(define (identity x)
  x)

(define (get-from-list test lst)
  (call/cc
   (lambda (k)
     (map (lambda (x) (if (test x) (k x)))
          lst)
     '())))

(define (make-objects-string lst)
  (if (> (length lst) 1)
      (string-join lst ", ")
      (car lst)))

(define-public make-player
  (lambda (nm)
    (let ((invent '())
          (loc (list 0 0 0)))
      (lambda (x . args) (cond ((eq? x 'name) nm)
                               ((eq? x 'inventory) invent)
                               ((eq? x 'add-to-inventory) (set! invent (cons (car args) invent)))
                               ((eq? x 'location) loc)
                               ((eq? x 'set-location!) (set! loc (car args)))
                               ((eq? x 'remove-from-inventory) (set! invent (delete (car args) invent))))))))

(define location-of
  (lambda (input)
    (input 'location)))

(define loc-north
  (lambda (location)
    (car location)))

(define loc-east
  (lambda (location)
    (cadr location)))

(define loc-up
  (lambda (location)
    (caddr location)))

(define update-location-of
  (lambda (player north east up)
    (let ((loc (location-of player)))
      (player 'set-location! (list (north (loc-north loc))
                                   (east (loc-east loc))
                                   (up (loc-up loc)))))))

(define north
  (lambda (player)
    (update-location-of player inc identity identity)))

(define south
  (lambda (player)
    (update-location-of player dec identity identity)))

(define east
  (lambda (player)
    (update-location-of player identity inc identity)))

(define west
  (lambda (player)
    (update-location-of player identity dec identity)))

(define up
  (lambda (player)
    (update-location-of player identity identity inc)))

(define down
  (lambda (player)
    (update-location-of player identity identity dec)))

(define name-of
  (lambda (x)
    (x 'name)))

(define description-of
  (lambda (x)
    (x 'description)))

(define exits-of
  (lambda (x)
    (x 'exits)))

(define inventory-of
  (lambda (x)
    (x 'inventory)))

(define move
  (lambda (player direction)
    (cond ((equal? direction "north") (north player))
	        ((equal? direction "south") (south player))
	        ((equal? direction "east") (east player))
	        ((equal? direction "west") (west player))
          ((equal? direction "up") (up player))
          ((equal? direction "down") (down player)))))

(define check-for-exits
  (lambda (location direction)
    (member direction (exits-of location))))

(define go-to
  (lambda (direction player rooms)
    (cond ((null? rooms) player)
	        ((check-for-exits (get-location-of player rooms) direction) (move player direction)))))

(define add-to-inventory-of
  (lambda (player object)
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

(define get-location-of
  (lambda (player rooms)
    (get-from-list (lambda (room) (equal? (location-of player)
                                          (location-of room)))
                   rooms)))

(define display-location-of
  (lambda (player rooms)
    (description-of (get-location-of player rooms))))

(define print-location-of
  (lambda (player rooms)
    (display (display-location-of player rooms))
    (newline)
    (display-exits-of player rooms)
    (newline)
    (display-objects-of player rooms)
    (newline)))

(define-public game
  (lambda (player rooms)
    (define loop
      (lambda (input)
        (if (null? input)
            (loop (get-a-lot-of-input))
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
                                                      player
                                                      (get-location-of player rooms))
                                                (newline)
					                                      (loop (get-a-lot-of-input))))
		                ((equal? cmd "say") (begin (display (string-join (cdr input)
                                                                     " "))
                                               (newline)
					                                     (loop (get-a-lot-of-input))))
		                ((equal? cmd "exit") (begin (display "Goodbye!\n")
                                                (exit)))
		                (else (begin (dispatch input
                                           player
                                           (get-location-of player rooms))
			                           (loop (get-a-lot-of-input)))))))))
    (print-location-of player rooms)
    (loop (get-a-lot-of-input))))

(define get-a-lot-of-input
  (lambda ()
    (let ((input (readline)))
      (string-tokenize input))))

(define dispatch
  (lambda (commands player location)
    (let* ((name (cadr commands))
           (command-symbols (map string->symbol (cons (car commands) (cddr commands))))
           (object (cond ((object-exists? (inventory-of player) name)
                         (get-object-from-inventory-of player name))
			                  ((object-exists? (objects-of location) name)
                         (get-object-of-location location name))
			                  (else '()))))
      (if (null? object)
          (display "You can't find that object.")
          (apply object command-symbols)))))

(define (test-and-print what fail success)
  (if (null? what)
      (display fail)
      (display (string-join
                (list success
                      (make-objects-string
                       what)) " "))))

(define display-exits-of
  (lambda (player rooms)
    (test-and-print (exits-of (get-location-of player rooms))
                    "You can't get out!"
                    "The exits are:")))

(define display-objects-of
  (lambda (player rooms)
    (let ((objects (objects-of (get-location-of player rooms))))
      (if (null? objects)
          (display "There's nothing here.")
          (display (string-join
                    (list "There is"
                          (make-objects-string
                           (map (lambda (obj) (obj 'name-with-article))
                                objects))) " "))))))

(define display-inventory-of
  (lambda (player)
    (let ((inventory (inventory-of player))) 
      (if (null? inventory) 
          (display "Your inventory is empty.")
          (display (string-join
                    (list "You have"
                          (make-objects-string 
                           (map (lambda (obj) (obj 'name-with-article))
                                inventory))) " "))))
    (newline)))

(define take
  (lambda (name player location)
    (let ((object (get-object-of-location location name)))
      (if (null? object)
	        (begin (display "That's impossible.")
                 player)
	        (begin (add-to-inventory-of player object)
                 (location 'delete-object object)
                 (display (string-join (list "You take" (object 'name-with-article)) " ")))))))

(define get-object-of-location
  (lambda (location name)
    (get-object-from (objects-of location) name)))

(define objects-of
  (lambda (location)
    (location 'objects)))

(define get-object-from
  (lambda (objects name)
    (get-from-list (lambda (obj) (member name (obj 'name)))
                   objects)))

(define get-object-from-inventory-of
  (lambda (player name)
    (get-object-from (inventory-of player) name)))

(define drop
  (lambda (name player location)
    (let ((object (get-object-from-inventory-of player name))
	        (inv (inventory-of player)))
      (if (null? object)
	        (begin (display "That's impossible.")
                 player)
	        (begin (add-to-objects-of location object)
		             (player 'remove-from-inventory object)
                 (display (string-join (list "You drop" (object 'name-with-article)) " ")))))))

(define add-to-objects-of
  (lambda (location object)
    (location 'add-object object)))

(define (object-exists? objects name)
  (not (null? (get-object-from objects name))))
