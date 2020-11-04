(define-module (simple-test)
  #:use-module (srfi srfi-64)
  #:use-module (mud-lab))

(module-define! (resolve-module '(srfi srfi-64))
		'test-log-to-file #f)

(test-begin "test-suite")

(define foo (make-player "foo"))

(define bar (make-room "bar" "A bar" (cons 0 0) (list "north")))

(define baz (make-room "baz" "Not a bar" (cons 1 0) (list "south")))

(define rooms (list bar baz))

(test-equal "Player"
  foo
  (make-player "foo"))

(go-to "north" foo rooms)

(test-equal "Move"
  (cdr (assoc "location" baz))
  (cdr (assoc "location" foo)))

(test-end "test-suite")

