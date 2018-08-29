;;
;; Scheme Knowbot
;;

;; Open the Databases

;; The Users Database
(define knowbot-users-db (gdbm-open "knowbot-users.db"))

;; The Seen Database
(define knowbot-seen-db (gdbm-open "knowbot-seen.db"))
;; The IS Database
(define knowbot-is-db (gdbm-open "knowbot-is.db"))
;; The ARE Database
(define knowbot-are-db (gdbm-open "knowbot-are.db"))


;; The Version
(define knowbot-version "Knowbot v0.2")
;; Change the schemebot version
(set! version (format "~a + ~a" version knowbot-version))

;;
;; User Functions
;;
;; User flags:
;; i   Ignored Flag
;; t   Teaching Allowed
;; r   Removing Allowed
;; m   Modifying Allowed
;;

(define (knowbot-users-flags-set nick hostmask flags)
  (gdbm-store-replace knowbot-users-db (format "~a~a" nick hostmask) flags))

(define (knowbot-users-flags nick hostmask)
  (let ((flags (gdbm-fetch knowbot-users-db (format "~a~a" nick hostmask))))
    (if flags
	flags
	(gdbm-fetch knowbot-users-db "*default*"))))

(define (knowbot-users-ignore? nick hostmask)
  (regexp-match? "i" (knowbot-users-flags nick hostmask)))

(define (knowbot-users-teach? nick hostmask)
  (regexp-match? "t" (knowbot-users-flags nick hostmask)))

(define (knowbot-users-remove? nick hostmask)
  (regexp-match? "r" (knowbot-users-flags nick hostmask)))

(define (knowbot-users-modify? nick hostmask)
  (regexp-match? "m" (knowbot-users-flags nick hostmask)))

;; Default flags
(define knowbot-users-default-flags "trm")

;; Set the default flags
(knowbot-users-flags-set "*default*" "" knowbot-users-default-flags)

;;
;; Seen support
;;

;; Update the seen database
(define (knowbot-seen-update nick channel text)
  (let ((lnick (string-downcase nick)))
	(gdbm-store-replace knowbot-seen-db 
			    (format "~a-seen" lnick) (format "~a" (time)))
	(gdbm-store-replace knowbot-seen-db (format "~a-channel" lnick) channel)
	(gdbm-store-replace knowbot-seen-db (format "~a-text" lnick) text)))

;; Check the seen database
(define (knowbot-seen-check requester nick) 
  (let* ((lnick (string-downcase nick))
	 (stime (gdbm-fetch knowbot-seen-db (format "~a-seen" lnick)))
	 (schan (gdbm-fetch knowbot-seen-db (format "~a-channel" lnick)))
	 (stext (gdbm-fetch knowbot-seen-db (format "~a-text" lnick))))

    (if (eq? schan "()")
	(set! schan "IRC"))

    (if (and stime stext)
	(format "~a was last seen on ~a at ~a, saying: ~a~%" nick schan
		(localtime (string->number stime)) stext)
	(format "I haven't seen '~a', ~a~%" nick requester))))

;;
;; IS factoids
;; 
(define (knowbot-is-lookup key)
  (set! key (string-downcase key))
  (let ((factoid (gdbm-fetch knowbot-is-db (string-downcase key))))
    (if factoid
	factoid
	#f)))


(define (knowbot-is-set nick hostmask key factoid)
  (set! key (string-downcase key))
  (if (knowbot-users-teach? nick hostmask)      
      (gdbm-store knowbot-is-db key factoid)
      #f))

(define (knowbot-is-add nick hostmask key factoid)
  (set! key (string-downcase key))
  (if (knowbot-users-modify? nick hostmask)
      (gdbm-store-replace knowbot-is-db key
			  (format "~a or ~a"
				  (knowbot-is-lookup key)
				  factoid))
      #f))

(define (knowbot-is-modify nick hostmask key factoid)
  (set! key (string-downcase key))
  (if (knowbot-users-change? nick hostmask)
      (gdbm-store-replace knowbot-is-db key factoid)
      #f))

(define (knowbot-is-remove nick hostmask key)
  (set! key (string-downcase key))
  (if (knowbot-users-remove? nick hostmask)
      (gdbm-delete knowbot-is-db key)
      #f))

;;
;; ARE factoids
;;


;;
;; Helper Functions
;;

(define (knowbot-rand-ok)
  (random "gotcha." "ok." "sure." "done."))

(define (knowbot-rand-is-factoid key factoid)
  (format (random "~a is ~a~%" "I think ~a is ~a.~%" "I guess ~a is ~a~%"
		  "~a is, like, ~a~%" "well, ~a is ~a~%" "I heard ~a is ~a~%"
		  "rumour has it ~a is ~a~%")
	  key factoid))	 

;; Clean our nick from the text, if we are being addressed
(define (knowbot-clean-nick text) 
  (if (regexp-match-ci? (format "^[ ]*~a[ ]*[:,]*[ ]*" irc-mynick) text)
      (set! text (list-ref (regexp-matches-ci
			    (format "^[ ]*~a[ ]*[:,]*[ ]*(.*)" irc-mynick)
			    text) 
			   1)))
  (if (regexp-match-ci? (format ",[ ]*~a[ ]*[\\\\?]*[ ]*$" irc-mynick) text)
      (set! text (list-ref (regexp-matches-ci
			    (format "(.*),[ ]*~a[ ]*[\\\\?]*[ ]*$" irc-mynick)
			    text) 
			   1)))
  text)


;; Clean stop words from the key
(define (knowbot-clean-key key)
  ;; no,
 (if (regexp-match-ci? "^no[ ]*,*[ ]+" key)
      (set! key (list-ref (regexp-matches-ci "^no[ ]*,*[ ]+(.*)" key) 1)))
 ;; a or an
 (if (regexp-match-ci? "^a[n]*[ ]+" key)
     (set! key (list-ref (regexp-matches-ci "^a[n]*[ ]+(.*)" key) 1)))
 ;; the
 (if (regexp-match-ci? "^the[ ]+" key)
     (set! key (list-ref (regexp-matches-ci "^the[ ]+(.*)" key) 1)))
 (if (regexp-match-ci? "^.*[\\?!]+$" key)
     (set! key (list-ref (regexp-matches-ci "^(.*)[\\?!]+$" key) 1)))
 key)

;;
;;The Knowbot parser - generates response strings
;; 
(define (knowbot-parser channel target source hostmask text)
  (define respond #f) ; should we respond?
  (define response '())

  ;; Private message?
  (if (null? channel)
      (set! respond #t))

  ;; Addressed to us?
  (if (regexp-match-ci? irc-mynick text)
      (begin 
	(set! text (knowbot-clean-nick text))
	(set! respond #t)))
     
  (set! response 
	(cond 
	 ;; Ignore people
	 ((knowbot-users-ignore? source hostmask) #f)
	 
	 ;;
	 ;; Queries
	 ;; 

	 ;; schemebot?
	 ((regexp-match-ci? "^[ ]*\\?*[ ]*$" text)
	  (random (format "yes, ~a?~%" source) 
		  (format "~a?~%" source)))


	 ;; x?!
	 ((regexp-match-ci? "^[ ]*.*[\\?!]+[ ]*$" text)
	  (let ((key (list-ref (regexp-matches-ci "^[ ]*(.*)[\\?!]+[ ]*$" 
						  text) 1)))
	    (set! key (knowbot-clean-key key))		
	    (cond
	     ((knowbot-is-lookup key)
	      (set! respond #t)
	      (knowbot-rand-is-factoid key (knowbot-is-lookup key)))
	     ;;((knowbot-are-lookup key)
	     ;;(knowbot-rand-are-factoid (knowbot-are-lookup key)))
	     (else #f))))	     	 
	 ;;
	 ;; Teaching
	 ;;
	 
	 ;; forget x
	 ((regexp-match-ci? "^[ ]*forget[ ]+.*$" text)
	  (let ((key (list-ref (regexp-matches-ci "^[ ]*forget[ ]+(.*)$" text) 1)))
	    (set! key (knowbot-clean-key key))
	    (if (and (knowbot-is-lookup key)
		     (knowbot-users-remove? source hostmask))
		(begin
		  (knowbot-is-remove source hostmask key)
		  (format "~a: I forgot ~a.~%" source key))
		#f)))		  

	 ;; x is also y || no, x is also y
	 ((regexp-match-ci? "^[ ]*(.*?)[ ]+is[ ]+also[ ]+(.*)$" text)
	  (let* ((matches (regexp-matches-ci "^[ ]*(.*?)[ ]+is[ ]+also[ ]+(.*)$" 
					     text))
		 (key (list-ref matches 1))
		 (factoid (list-ref matches 2)))
	    (set! key (knowbot-clean-key key))
	    (cond ((and (not (knowbot-is-lookup key))
			(knowbot-users-teach? source hostmask))
		   (begin
		     (display (format "<~a[~a]> FACTOID: ~a => ~a~%" source channel
				      key factoid))
		     (knowbot-is-set source hostmask key factoid)
		     (knowbot-rand-ok)))
		  ((and (knowbot-is-lookup key)
			(knowbot-users-modify? source hostmask))
		   (begin
		     (display (format "<~a[~a]> FACTOID: ~a => or ~a~%" source
				      channel key factoid))
		     (knowbot-is-add source hostmask key factoid)
		     (knowbot-rand-ok)))
		  (else #f))))

	 ;; no, x is y
	 ((regexp-match-ci? "^[ ]*no[,]*[ ]*(.*?)[ ]+is[ ]+(.*)$" text)
	  (let* ((matches (regexp-matches-ci "^[ ]*no[,]*[ ]*(.*?)[ ]+is[ ](.*)$" 
					     text))
		 (key (list-ref matches 1))
		 (factoid (list-ref matches 2)))
	    (set! key (knowbot-clean-key key))
	    (if (knowbot-users-modify? source hostmask)
		(begin
		  (display (format "~a => ~a~%" key factoid))
		  (knowbot-is-modify source hostmask key factoid)
		  (knowbot-rand-ok))
		#f)))
		   
	 ;; x is y
	 ((regexp-match-ci? "^[ ]*(.*?)[ ]+is[ ]+(.*)$" text)
	  (let* ((matches (regexp-matches-ci "^[ ]*(.*?)[ ]+is[ ](.*)$" text))
		 (key (list-ref matches 1))
		 (factoid (list-ref matches 2)))
	    (set! key (knowbot-clean-key key))
	    (if (and (not (knowbot-is-lookup key))
		     (knowbot-users-teach? source hostmask))
		(begin
		  (display (format "<~a[~a]> ~a => ~a~%" source channel
				   key factoid))
		  (knowbot-is-set source hostmask key factoid)
		  (knowbot-rand-ok))
		#f)))
	 
	 (else #f)))

  ;; Return a response only if we directly addressed
  (if respond
      (format "~a~%" response)))
;; done
#t