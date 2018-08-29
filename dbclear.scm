#!/usr/local/bin/vcsi

;; The database
(dynamic-load "gdbm.so")
(define fight-db (gdbm-open "fight.db"))

(define (dumper key)
  (if key
      (cons key (dumper (gdbm-nextkey fight-db key)))
      '()))

(define (clear)
  (let ((keys (dumper (gdbm-firstkey fight-db))))
    (map (lambda (x)
	   (display (format "~a> Clear? (y|n) " x))
	   (if (eq? (read) 'y)
	       (gdbm-delete fight-db x)))
	 keys)
    #t))

(clear)

