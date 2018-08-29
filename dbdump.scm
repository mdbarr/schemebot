#!/usr/local/bin/vcsi

;; The database
(dynamic-load "gdbm.so")
(define fight-db (gdbm-open "fight.db"))

(define (dumper key)
  (if key
      (let ((nkey (gdbm-nextkey fight-db key)))
	(display (format "~a: ~a~%" nkey (gdbm-fetch fight-db nkey)))
	(dumper nkey))
      #t))

(define (dump)
  (dumper (gdbm-firstkey fight-db)))

(dump)
