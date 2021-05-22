(define-module (simple-test)
  #:use-module (srfi srfi-64)
  #:use-module (mud-lab))

(module-define! (resolve-module '(srfi srfi-64))
		'test-log-to-file #f)

(test-begin "test-suite")

(define foo (make-player "foo"))

(define bar (make-room "bar" "A bar" (list 0 0 0) (list "north")))

(test-equal "Move"
  (bar 'location)
  (foo 'location))

(test-end "test-suite")

