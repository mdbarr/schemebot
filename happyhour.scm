#!/usr/local/bin/vcsi

(dynamic-load "gdbm.so")

(define happyhour-db (gdbm-open "/home/schemebot/schemebot/happyhour.db"))

(gdbm-store-replace happyhour-db "happyhour-hour" 
		    (format "~a" (+ 9 (random 7))))
(gdbm-store-replace happyhour-db "happyhour-day" 
		    (format "~a" (+ 1 (random 5))))
