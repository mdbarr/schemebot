#!/usr/local/bin/vcsi

;; The database
(dynamic-load "gdbm.so")
(define fight-db (gdbm-open "fight.db"))

(define (dumper key)
  (if key
      (cons key (dumper (gdbm-nextkey fight-db key)))
      '()))

(define (auto-clear)
  (let ((keys (dumper (gdbm-firstkey fight-db))))
    (map (lambda (x)
	   (if (and 
		(not (regexp-match? "death-cry" x))
		(not (regexp-match? "gender" x))
		(not (regexp-match? "rank" x))
		(not (regexp-match? "super-move" x))
		(not (regexp-match? "alias" x)))
	       (begin
		 (display (format "Deleting ~a...~%" x))
		 (gdbm-delete fight-db x))))
	 keys)
    #t))

(auto-clear)

