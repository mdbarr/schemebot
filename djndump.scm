#!/usr/local/bin/vcsi

;; The database
(dynamic-load "gdbm.so")
(define scheme-fighter-db (gdbm-open "fight.db"))

(define (dumper key)
  (if key
      (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
        (if nkey
            (if (regexp-match? "ude" nkey)
		(irc-privmsg irc-conn "DSmooth" (format "~a: ~a~%" nkey (gdbm-fetch scheme-fighter-db nkey)))
	(dumper nkey))))
      #t))

(define (dump)
  (dumper (gdbm-firstkey scheme-fighter-db)))

(dump)
