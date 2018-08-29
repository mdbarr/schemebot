;;
;; The Scheme Fighter 
;;


;;
;; To Do
;;
;; !fstattop by exp
;; !consider
;; banned lists
;; better channel/names support
;; auto aliasing for nick change if !exists new_nick
;;

;; Open the Database
(define scheme-fighter-db (gdbm-open "fight.db"))

;;Open the Happy Hour Database
(define happyhour-db (gdbm-open "happyhour.db"))

;;Set the happy hour fight count to 0
(gdbm-store-replace scheme-fighter-db "HappyHourSameFights" (format "~a" 0))

;; The Version
(define fighter-version "Scheme Fighter v0.99")
;; Change the schemebot version
(set! version (format "~a + ~a" version fighter-version))

;; Logger funtion - do something with victories and exp/hp gains
(define (scheme-fighter-logger text)
  (ctcp "Sasquatch" (format "SCHEME-FIGHTER ~a" text)))

;; List of Non-Combatants
(define scheme-fighter-non-combatants (list "jiffy"))
;; List of 'no-kicks'
(define scheme-fighter-no-kick (list "Sasquatch"))

;; Helper functions
(define seq? string-ci=?)

;; Sync the DB in Memory to Disk
(define (scheme-fighter-db-sync)
  (gdbm-sync scheme-fighter-db))

(define (scheme-fighter-set-alias x y) ; Set an alias for a nick
  (gdbm-store-replace scheme-fighter-db (format "~a-alias" x) (format "~a" y)))

(define (scheme-fighter-get-alias x) ; Get the real nick from an alias
  (let ((alias (gdbm-fetch scheme-fighter-db (format "~a-alias" x))))
    (if alias alias x)))

(define (scheme-fighter-check-alias x)
  (if (seq? (scheme-fighter-get-alias x) x)
      (format "No alias for ~a.~%" x)
      (format "Alias for ~a: ~a.~%" x (scheme-fighter-get-alias  x))))

(define (scheme-fighter-set-hp x y) ; Set the HP
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-hp" x) (format "~a" y)))

(define (scheme-fighter-get-hp x) ; Get the HP or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((hp (gdbm-fetch scheme-fighter-db (format "~a-hp" x))))
    (if hp (string->number hp)
        (let ((hp (+ 50 (random 5))))
          (scheme-fighter-set-hp x hp)
          hp))))

(define (scheme-fighter-set-exp x y) ; Set the EXP
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-exp" x) (format "~a" y)))

(define (scheme-fighter-get-exp x) ; Get the EXP or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((exp (gdbm-fetch scheme-fighter-db (format "~a-exp" x))))
    (if exp (string->number exp)
        (let ((exp 0))
          (scheme-fighter-set-exp x exp)
          exp))))

(define (scheme-fighter-set-wins x y) ; Set the # of Wins
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-wins" x) (format "~a" y)))

(define (scheme-fighter-get-wins x) ; Get the WINS or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((wins (gdbm-fetch scheme-fighter-db (format "~a-wins" x))))
    (if wins (string->number wins)
        (let ((wins 0))
          (scheme-fighter-set-wins x wins)
          wins))))

(define (scheme-fighter-set-losses x y) ; Set the # of losses
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-losses" x)
                      (format "~a" y)))

(define (scheme-fighter-get-losses x) ; Get the LOSSES or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((losses (gdbm-fetch scheme-fighter-db (format "~a-losses" x))))
    (if losses (string->number losses)
        (let ((losses 0))
          (scheme-fighter-set-losses x losses)
          losses))))

(define (scheme-fighter-set-vswins x y wins) ; Set the # of vs wins
  (set! x (scheme-fighter-get-alias x))
  (set! y (scheme-fighter-get-alias y))
  (gdbm-store-replace scheme-fighter-db (format "~a-~a-wins" x y)
                      (format "~a" wins)))

(define (scheme-fighter-get-vswins x y) ; Get the vs win stats between x and y set if not found
  (set! x (scheme-fighter-get-alias x))
  (set! y (scheme-fighter-get-alias y))
  (let ((wins-x (gdbm-fetch scheme-fighter-db (format "~a-~a-wins" x y))))
    (if wins-x (string->number wins-x)
        (let ((wins-x 0))
          (scheme-fighter-set-vswins x y wins-x)
          wins-x))))

(define (scheme-fighter-stats-last10-add x type) ; add a result to the last10
  (set! x (scheme-fighter-get-alias x))
  (let* ((last10 (scheme-fighter-stats-getlast10 x))
         (newlast10 (string-append last10 type))
         (last10len (string-length newlast10)))
    (if (> last10len 10)
        (set! newlast10 (substring newlast10 (- last10len 10) last10len))
        )
    (scheme-fighter-stats-setlast10 x newlast10)
    newlast10
    ))

(define (scheme-fighter-stats-setlast10 x last10) ; Set the last 10 for a user
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-last10" x) last10))

(define (scheme-fighter-stats-getlast10 x) ; return the last 10 fights --set to blank if none
  (set! x (scheme-fighter-get-alias x))
  (let ((last10 (gdbm-fetch scheme-fighter-db (format "~a-last10" x))))
    (if last10 last10
        (let ((last10 ""))
          (scheme-fighter-stats-setlast10 x last10)
          last10))))

(define (scheme-fighter-stats-parselast10 x) ; Parse the last 10 and return the wins and losses
  (let ((l (string->list (scheme-fighter-stats-getlast10 x)))
        (wins 0)
        (loses 0))
    (map (lambda (y) (if (char-ci=? y #\w)
                         (set! wins (+ wins 1))
                         (set! loses (+ loses 1)))) l)
    (cons wins loses)))

(define scheme-fighter-timeout 600) ; The default timeout

(define scheme-fighter-notify-sleep 1) ; The sleep between notifies

(define (scheme-fighter-set-time x y) ; Set the time of fight start
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-time" x) (format "~a" y)))

(define (scheme-fighter-get-time x) ; Get the start time or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((ltime (gdbm-fetch scheme-fighter-db (format "~a-time" x))))
    (if ltime (string->number ltime)
        (begin (gdbm-store-replace scheme-fighter-db
                                   (format "~a-time" x) "0")
          0))))

(define (scheme-fighter-penalize x y) ; Penalize X for Y minutes
  (set! x (scheme-fighter-get-alias x))
  (set! y (max (- (+ (time) (* y 60)) scheme-fighter-timeout)
               (+ (scheme-fighter-get-time x) (* y 60))))
  (scheme-fighter-set-time x y))

(define (scheme-fighter-set-death-cry x y) ; Set the Death Cry for a user
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-death-cry" x)
                      (format "~a" y)))

(define (scheme-fighter-get-death-cry x) ; Get the DEATH-CRY or set 
  (set! x (scheme-fighter-get-alias x))
  (let ((death-cry (gdbm-fetch scheme-fighter-db (format "~a-death-cry" x))))
    (if death-cry death-cry
        (let ((death-cry "I am too lame for a death-cry"))
          (scheme-fighter-set-death-cry x death-cry)
          death-cry))))


(define (scheme-fighter-set-victory-cry x y) ; Set the Victory Cry for a user
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-victory-cry" x)
                      (format "~a" y)))

(define (scheme-fighter-get-victory-cry x) ; Get the VICTORY-CRY or set 
  (set! x (scheme-fighter-get-alias x))
  (let ((victory-cry (gdbm-fetch scheme-fighter-db (format "~a-victory-cry" x))))
    (if victory-cry victory-cry
        (let ((victory-cry "UNSET"))
          (scheme-fighter-set-victory-cry x victory-cry)
          victory-cry))))


(define (scheme-fighter-set-gender x y) ; Set the gender 
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-gender" x) (format "~a" y)))

(define (scheme-fighter-get-gender x) ; Get the gender or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((gender (gdbm-fetch scheme-fighter-db (format "~a-gender" x))))
    (if gender gender
        (let ((gender "M"))
          (scheme-fighter-set-gender x gender)
          gender))))

(define (scheme-fighter-set-rank x y) ; Set the RANK
  (set! x (scheme-fighter-get-alias x))
  (gdbm-store-replace scheme-fighter-db (format "~a-rank" x) (format "~a" y)))

(define (scheme-fighter-get-rank x) ; Get the RANK or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((rank (gdbm-fetch scheme-fighter-db (format "~a-rank" x))))
    (if rank (string->number rank)
        (let ((rank 0))
          (scheme-fighter-set-rank x rank)
          rank))))

(define (scheme-fighter-check-add-hp newexp oldexp) ; See if we're add an HP breakpoint
  (if (scheme-fighter-check-add-hp-thresh newexp oldexp 20)
      #t
      (if (scheme-fighter-check-add-hp-thresh newexp oldexp 50)
          #t
          (if (scheme-fighter-check-add-hp-thresh newexp oldexp 100)
              #t
              (if (scheme-fighter-check-add-hp-thresh newexp oldexp 175)
                  #t
                  (if (scheme-fighter-check-add-hp-thresh newexp oldexp 250)
                      #t
                      #f))))))

(define (scheme-fighter-check-add-hp-thresh newexp oldexp thresh)
  (if (and (< oldexp thresh)
           (>= newexp thresh))
      #t
      #f))

(define (scheme-fighter-set-super-move x y) ; Set the super-move 
  (set! x (scheme-fighter-get-alias x))
  (if (and (regexp-match? "~a" y)
           (= (length (string-split "~a" y)) 3)) ; awkward, but works
      (gdbm-store-replace scheme-fighter-db (format "~a-super-move" x)
                          (format "~a" y))
      (begin
        (say "Your super-move must contains three '~a's, two for the nicks, and a third for the HP display.")
        (say "Such as: \"~a executes an indescribable super-move on ~a[~a].\"")
        )))

(define (scheme-fighter-get-super-move x) ; Get the super-move or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((super-move (gdbm-fetch scheme-fighter-db (format "~a-super-move" x))))
    (if super-move super-move
        (let ((super-move "~a executes an indescribable super-move on ~a[~a].~%"))
          (scheme-fighter-set-super-move x super-move)
          super-move))))

(define (scheme-fighter-set-finish-him x y) ; Set the finish him move 
  (set! x (scheme-fighter-get-alias x))
  (if (and (regexp-match? "~a" y)
           (= (length (string-split "~a" y)) 3)) ; awkward, but works
      (gdbm-store-replace scheme-fighter-db (format "~a-finish-him" x)
                          (format "~a" y))
      (begin
        (say "Your finish-him must contains three '~a's, two for the nicks, and a third for the HP display.")
        (say "Such as: \"~a executes an indescribable finishing move on ~a[~a].\"")
        )))

(define (scheme-fighter-get-finish-him x) ; Get the finsihing move or set if not found
  (set! x (scheme-fighter-get-alias x))
  (let ((the-finish-him (gdbm-fetch scheme-fighter-db (format "~a-finish-him" x))))
    (if the-finish-him the-finish-him
        (let ((the-finish-him "~a executes an indescribable finishing move on ~a[~a].~%"))
          (scheme-fighter-set-finish-him x the-finish-him)
          the-finish-him))))


(define (scheme-fighter-reset x) ; Reset a user
  (set! x (scheme-fighter-get-alias x))
  ; Delete the HP and EXP
  (gdbm-delete scheme-fighter-db (format "~a-hp" x))
  (gdbm-delete scheme-fighter-db (format "~a-exp" x))
  ; Set the to defaults
  (scheme-fighter-get-hp x)
  (scheme-fighter-get-exp x)
  #t)

(define (scheme-fighter-hp-bonus x) ; Bonus HP for levels with multiples of 50
  (if (>= x 50)
      (+ (div x 50) 1) (div x 50)))

;;
;; Additional attack
;;
(define (scheme-fighter-get-extra-hit rank1 rank2 startHP currHP)
  (let* ((startP 60)
         (rank1 (if (and (>= rank1 40) (<= (remainder rank1 40) 15)) 15 (+ rank1 0)))
         (rank2 (if (and (>= rank2 40) (<= (remainder rank2 40) 15)) 15 (+ rank2 0)))
         (diff (- (remainder rank1 40)
                  (remainder rank2 40)))
         (levelP 3))
    (if (< diff 0)
        #f
        (if (< startP (random 100))
            #f
            (__scheme-fighter-extra-hit-help diff levelP (/ (* currHP 110.) startHP))))))

(define (__scheme-fighter-extra-hit-help diff levelP prob)
  (if (= diff 0)
      #f
      (if (< (random 100) levelP)
          (if (< (random 100) prob)
              #t
              (__scheme-fighter-extra-hit-help (- diff 1) levelP prob))
          (__scheme-fighter-extra-hit-help (- diff 1) levelP prob))))
;;
;; Fight damage
;;
(define (__scheme-fighter-damage-repeat y)
  (let ((tmp (min y 1000000000)))
    (inexact->exact
     (round
      (if (< tmp 1000)
          (+ (* (expt tmp 2) .00003) (* (sqrt tmp) 0.4))
          (let ((x (+ (expt tmp 0.25) 37)))
            (- x
               (+ (* 0.00000030596 (expt x 4))
                  (+ (* -.00017912 (expt x 3))
                     (+ (* .038649 (expt x 2))
                        (+ (* -2.813 x)
                           57.7658)))))))))))


(define (__scheme-fighter-get-damage x y)
  (round (+ (* x y) (random (inexact->exact (round y))))))

(define (_scheme-fighter-max-damage exp)
  (__scheme-fighter-max-damage (__scheme-fighter-damage-repeat exp) 30))

(define (__scheme-fighter-max-damage x y)
  (let ((randPart (random 100)))
    (if (= x 0)
        y
        (cond ((< randPart 35) (__scheme-fighter-max-damage (- x 1) y))
              ((< randPart 80) (__scheme-fighter-max-damage (- x 1) (+ y 1)))
              ((< randPart 100) (__scheme-fighter-max-damage (- x 1) (+ y 2)))))))

(define (_scheme-fighter-get-damage newExp)
  (let* ((maxDam (_scheme-fighter-max-damage newExp))
         (partDam (/ maxDam 5.))
         (randPart (random 100)))
    (inexact->exact
     (cond ((< randPart 15) (__scheme-fighter-get-damage 0 partDam))
           ((< randPart 35) (__scheme-fighter-get-damage 1 partDam))
           ((< randPart 85) (__scheme-fighter-get-damage 2 partDam))
           ((< randPart 95) (__scheme-fighter-get-damage 3 partDam))
           ((< randPart 100) (__scheme-fighter-get-damage 4 partDam))))))

(define (_scheme-fighter-damage-block otherExp)
  (let* ((partBlockPerc (+ 35 (random 15)))
         (maxDam (_scheme-fighter-max-damage otherExp))
         (partBlock (/ (/ (* maxDam partBlockPerc) 100) 5.))
         (randPart (random 100)))
    (inexact->exact
     (cond ((< randPart 40) (__scheme-fighter-get-damage 0 partBlock))
           ((< randPart 70) (__scheme-fighter-get-damage 1 partBlock))
           ((< randPart 80) (__scheme-fighter-get-damage 2 partBlock))
           ((< randPart 85) (__scheme-fighter-get-damage 3 partBlock))
           ((< randPart 90) (__scheme-fighter-get-damage 4 partBlock))
           ((< randPart 100) -1)))))

(define (scheme-fighter-get-damage x y)
  (let* ((block (_scheme-fighter-damage-block y))
         (damage (_scheme-fighter-get-damage x))
         (totDam (max (- damage block) 0)))
    (if (< block 0)
        0 ; Blocked
        totDam)))

;; The main damage function
(define (scheme-fighter-compute-damage x y) ; Damage for x hitting y
  ;; (let* ((base (* 5 (+ 1 (quotient (scheme-fighter-get-exp x) 50))))
  ;; (offset (quotient (scheme-fighter-get-exp y) 50))
  ;; (damage (if (< (- base offset) 0) 0 (- base offset))))   	
  ;; (random (+ damage 10))))
  (scheme-fighter-get-damage (scheme-fighter-get-exp x)
                             (scheme-fighter-get-exp y)))
;;
;; Rewards after the battle
;;
(define scheme-fighter-max-exp 250)


;; x in the winner, y the loser
(define (scheme-fighter-finish x x-hp y y-hp) ; End of a fight
  (let* ((flawless-multiplier (if (= x-hp (scheme-fighter-get-hp x)) 2 1)) ;; increase multiplier to 2 if it's a flawless victory
         (exp (max
               (+ 1 (random 3))
               (+ 1 (inexact->exact (round (* .08
                                              (scheme-fighter-get-exp y)))))))
         (old-exp (scheme-fighter-get-exp x))
         (exp (* exp flawless-multiplier))
         (new-exp (+ exp old-exp))
         (new-vswins (+ (scheme-fighter-get-vswins x y) 1)))

    ;;Store off winner and loser and fight
    (gdbm-store-replace scheme-fighter-db "scheme-fighter-last-winner" (format "~a" x))
    (gdbm-store-replace scheme-fighter-db "scheme-fighter-last-loser" (format "~a" y))

    (scheme-fighter-stats-last10-add x "w")
    (scheme-fighter-stats-last10-add y "l")

    ;; Store off the vs winner
    (scheme-fighter-set-vswins x y new-vswins)

    ;; Check to see if it was a flawless victory
    (if (= x-hp (scheme-fighter-get-hp x))
        (begin
          (say "FLAWLESS VICTORY!")
          (scheme-fighter-set-time x 0)))
    ;; Give X stuff
    (scheme-fighter-set-wins x (+ 1 (scheme-fighter-get-wins x)))
    (scheme-fighter-award-exp x exp #t)

    ;; Y Losses
    (scheme-fighter-set-losses y (+ 1 (scheme-fighter-get-losses y)))

    ;; 1-exp for Y?
    (scheme-fighter-check-1exp y x-hp)

    ;; Sync the DB
    (scheme-fighter-db-sync)

    ;; CTCP updates
    (scheme-fighter-logger (format "END winner:~a" x))
    (scheme-fighter-logger (scheme-fighter-stats-ctcp-logger x))
    
    (scheme-fighter-stats-ctcp x)
    (scheme-fighter-stats-ctcp y)

    ))

;; Award the user some experience
(define (scheme-fighter-award-exp user exp win)
  (let* ((old-exp (scheme-fighter-get-exp user))
         (new-exp (+ old-exp exp)))
    (scheme-fighter-set-exp user new-exp)
    (if win
        (scheme-fighter-logger (format "~a gains ~a experience points." user exp)))
    ;; Notify the user
    (if (> exp 1)
        (notice user (format "You gain ~a experience points." exp))
        (notice user (format "You gain ~a experience point." exp))
        )

    ;; Check to see if we need to levelup
    (if (scheme-fighter-check-levelup user)
        #t
        ;; Check to see if we need to add hp
        (if (scheme-fighter-check-add-hp new-exp old-exp)
            (scheme-fighter-award-hp user))
        )))

(define (scheme-fighter-check-1exp loser winner-hp)
  (if (< winner-hp 10)
      (scheme-fighter-award-exp loser 1 #f)
      ))

(define (scheme-fighter-check-levelup user)
  (if (and
       (> (scheme-fighter-get-exp user) scheme-fighter-max-exp)
       (< (scheme-fighter-get-exp user) 1000))
      (begin
        (scheme-fighter-set-rank user (+ (scheme-fighter-get-rank user) 1))
        (scheme-fighter-reset user)
        (scheme-fighter-logger (format "~a achieves a new level" user))
        (notice user "You achieve a new level.")
        #t
        )
      #f
      ))

(define (scheme-fighter-award-hp user)
  (let ((hp (+ (+ 2 (random 4)) (scheme-fighter-hp-bonus (scheme-fighter-get-rank user)))))
    (scheme-fighter-logger (format "~a gains ~a hit points." user hp))
    (scheme-fighter-set-hp user (+ hp (scheme-fighter-get-hp user)))
    (notice user (format "You gain ~a hit points.~%" hp))))

(define (scheme-fighter-stats x) ; Get the stats for a user
  ; Remove trailing spaces
  (if (seq? (substring x (- (string-length x) 1) (string-length x)) " ")
      (set! x (substring x 0 (- (string-length x) 1))))
  (if (scheme-fighter-member x irc-primary-channel-names) ; Get the real name
      (set! x (scheme-fighter-member x irc-primary-channel-names)))

  (set! x (scheme-fighter-get-alias x))
  (let* ((last10 (scheme-fighter-stats-parselast10 x))
         (lastwins (car last10))
         (lastlosses (cdr last10)))
    (if (gdbm-fetch scheme-fighter-db (format "~a-hp" x))
        (format
         "~a: HP: ~a  EXP: ~a  Wins: ~a  Losses: ~a  Level: ~a (~a|~a)~%"
         x
         (scheme-fighter-get-hp x)
         (scheme-fighter-get-exp x)
         (scheme-fighter-get-wins x)
         (scheme-fighter-get-losses x)
         (scheme-fighter-get-rank x)
         lastwins
         lastlosses
         )
        (format "No fight statistics for ~a.~%" x))))

(define (scheme-fighter-stats-vs-user x) ; output all vs stats for the user
  (format "Not done yet")
  )

(define (scheme-fighter-stats-vs x y) ; output vs stats
  ; Remove trailing spaces
  (if (seq? (substring x (- (string-length x) 1) (string-length x)) " ")
      (set! x (substring x 0 (- (string-length x) 1))))
  (if (scheme-fighter-member x irc-primary-channel-names) ; Get the real name
      (set! x (scheme-fighter-member x irc-primary-channel-names)))

  (set! x (scheme-fighter-get-alias x))

  ; Remove trailing spaces
  (if (seq? (substring y (- (string-length y) 1) (string-length y)) " ")
      (set! y (substring y 0 (- (string-length y) 1))))
  (if (scheme-fighter-member y irc-primary-channel-names) ; Get the real name
      (set! y (scheme-fighter-member y irc-primary-channel-names)))

  (set! y (scheme-fighter-get-alias y))

  (let ((x-wins (scheme-fighter-get-vswins x y))
        (y-wins (scheme-fighter-get-vswins y x)))

    (format "VS Stats (~a vs ~a): Wins: ~a  Losses: ~a" x y x-wins y-wins)))

(define (scheme-fighter-stats-all l)
  (define stats '())

  (for-each (lambda (x)
              (if (and (not (seq? x (scheme-fighter-get-alias x)))
                       (member (scheme-fighter-get-alias x) l))
                  #f
                  (set! stats (cons (format "~a[~a/~a/~a/~a/~a]" x
                                            (scheme-fighter-get-hp x)
                                            (scheme-fighter-get-exp x)
                                            (scheme-fighter-get-wins x)
                                            (scheme-fighter-get-losses x)
                                            (scheme-fighter-get-rank x))
                                    stats))))
            l)

  (string-append (string-join ", " stats) "."))

;; ctcp stats
(define (scheme-fighter-stats-ctcp x) 
  (if (list-contains update-list x)
      (begin
	(ctcp x (format "FIGHT UPDATE_STATS ~a,~a,~a,~a,~a"
			(scheme-fighter-get-hp x)
			(scheme-fighter-get-exp x)
			(scheme-fighter-get-wins x)
			(scheme-fighter-get-losses x)
			(scheme-fighter-get-rank x))))))

(define (scheme-fighter-stats-ctcp-logger x)
  (format "STATS ~a: ~a,~a,~a,~a,~a"
	  x
	  (scheme-fighter-get-hp x)
	  (scheme-fighter-get-exp x)
	  (scheme-fighter-get-wins x)
	  (scheme-fighter-get-losses x)
	  (scheme-fighter-get-rank x)))


;; better member function
(define (scheme-fighter-member s l)
  (if (pair? l)
      (if (and (string? (car l)) (seq? (car l) s))
          (car l)
          (scheme-fighter-member s (cdr l)))
      #f))

;; list pop functions
(define (scheme-fighter-list-pop s l)
  (define (scheme-fighter-list-pop-i s l r)
    (if (pair? l)
        (if (seq? (scheme-fighter-get-alias s) 
		  (scheme-fighter-get-alias (car l)))
            (scheme-fighter-list-pop-i s (cdr l) r)
            (scheme-fighter-list-pop-i s (cdr l) (cons (car l) r)))
        r))
  (reverse (scheme-fighter-list-pop-i s l '())))

;; Find the weakest (hp) apart from the player and schemebot and fight aliases
(define (scheme-fighter-get-weakest y)
  (define y-orig (scheme-fighter-get-alias y))
  (define y-hp 999999)
  (for-each (lambda (n)
              (define n-hp (scheme-fighter-get-hp n))
              (if (and (not (or (seq? n "schemebot") 
				(seq? y-orig (scheme-fighter-get-alias n))))
                       (< n-hp y-hp))
                  (begin (set! y n) (set! y-hp n-hp) y)))
            irc-primary-channel-names)
  y)

;; Find the strongest (hp) apart from the player and schemebot and fight aliases
(define (scheme-fighter-get-strongest y)
  (define y-orig (scheme-fighter-get-alias y))
  (define y-hp 0)
  (for-each (lambda (n)
              (define n-hp (scheme-fighter-get-hp n))
              (if (and (not (or (seq? n "schemebot")
				(seq? y-orig (scheme-fighter-get-alias n))))
                       (> n-hp y-hp))
                  (begin (set! y n) (set! y-hp n-hp) y)))
            irc-primary-channel-names)
  y)

;; The real fight function
(define (scheme-fighter x y channel mode)
  (call/cc
   (lambda (exit)

     ;; State variables
     (define is-random #f)

     ;; The fighting channel?
     (if (not (seq? channel irc-primary-channel))
         (exit (irc-privmsg irc-conn channel (format
                                              "sorry, ~a isn't a fighting channel"
                                              channel))))

     ;; Special persons
     (cond
      ((seq? y "!random")
       (begin (set! is-random #t)
	      (set! y (apply random
			     (scheme-fighter-list-pop x
						      (scheme-fighter-list-pop
						       "schemebot"
						       irc-primary-channel-names))))))

      ((seq? y "!hipster")
       (begin (set! is-random #t) (set! y (scheme-fighter-get-weakest x))))
      ((seq? y "!weakest")
       (begin (set! is-random #t) (set! y (scheme-fighter-get-weakest x))))

      ((seq? y "!strongest")
       (begin (set! is-random #t) (set! y (scheme-fighter-get-strongest x))))

      ((seq? y "!winner")
       (set! y (gdbm-fetch scheme-fighter-db "scheme-fighter-last-winner")))
      ((seq? y "!hypocrite")
       (set! y (gdbm-fetch scheme-fighter-db "scheme-fighter-last-winner")))

      ((seq? y "!loser")
       (set! y (gdbm-fetch scheme-fighter-db "scheme-fighter-last-loser"))))

     ;; The person exists
     (if (not (scheme-fighter-member x irc-primary-channel-names))
         (exit (say (format "~a isn't here!~%" x)))
         (set! x (scheme-fighter-member x irc-primary-channel-names)))

     (if (not (scheme-fighter-member y irc-primary-channel-names))
         (exit (say (format "~a isn't here!~%" y)))
         (set! y (scheme-fighter-member y irc-primary-channel-names)))

     ;; Non Combatants
     (if (scheme-fighter-member y scheme-fighter-non-combatants)
         (begin
           (notice y (format "Attention ~a: To remain a non-combatant, please change your nick to wuss_~a or ~a_is_gay.~%" y y y))
           (say (format "Attention ~a: To remain a non-combatant, please change your nick to wuss_~a or ~a_is_gay.~%" y y y))))



     ;; Wuss check
     (if (regexp-match? "^wuss_" y)
         (exit (say (format "~a is too much of a wuss to fight.~%" y))))
     (if (regexp-match? "^wuss_" x)
         (exit (say (format "~a is too much of a wuss to fight.~%" x))))

     ;; is_gay check
     (if (and (regexp-match? "_is_gay" y)
              (not (regexp-match? "fighting_is_gay" y)))
         (exit (say (format "~a is too gay to fight.~%" y))))

     (if (and (regexp-match? "_is_gay" x)
              (not (regexp-match? "fighting_is_gay" x)))
         (exit (say (format "~a is too gay to fight.~%" x))))

     ;; Happy hour limit override
     (if (not (scheme-fighter-happyhour-check))

         ;; Limits
         (if (< (- (time) (scheme-fighter-get-time x)) scheme-fighter-timeout)
             (let ((minutes (inexact->exact
                             (round (/ (- scheme-fighter-timeout
                                          (-
                                           (time)
                                           (scheme-fighter-get-time x)))
                                       60.))))
                   (hours (inexact->exact
                           (floor (/ (- scheme-fighter-timeout
                                        (-
                                         (time)
                                         (scheme-fighter-get-time x)))
                                     3600.))))
                   (days (inexact->exact
                          (floor (/ (- scheme-fighter-timeout
                                       (-
                                        (time)
                                        (scheme-fighter-get-time x)))
                                    86400.)))))

               (if (> days 0)
                   (begin (set! hours (- hours (* days 24)))
                     (set! minutes (- minutes (* days 24 60)))))

               (if (> hours 0)
                   (set! minutes (- minutes (* hours 60))))

               (cond

                ((eq? minutes 0)
                 (exit
                  (notice x "You must wait about a minute before you can fight.")))


                ((eq? minutes 1)
                 (exit
                  (notice x
                          (format "You must wait ~a minute before you can fight."
                                  minutes))))
                ((> days 1)
                 (exit
                  (notice x
                          (format "You must wait ~a days, ~a hours and ~a minutes before you can fight."
                                  days hours minutes))))
                ((> hours 1)
                 (exit
                  (notice x
                          (format "You must wait ~a hours and ~a minutes before you can fight."
                                  hours minutes))))

                (else
                 (exit
                  (notice x
                          (format "You must wait ~a minutes before you can fight."
                                  minutes))))))
             (scheme-fighter-set-time x (time)))

         ;; end happy hour check

         (scheme-fighter-set-time x (time)))

     ;; start happy hour fairness limit
     (define curhapcount (gdbm-fetch scheme-fighter-db "HappyHourSameFights"))
     (if (not curhapcount) (set! curhapcount "0"))
     (if (scheme-fighter-happyhour-check)
         (cond
          ((and (seq? x (gdbm-fetch scheme-fighter-db "scheme-fighter-last-fighter")) (< (string->number curhapcount) 9))
           (gdbm-store-replace scheme-fighter-db "HappyHourSameFights" (format "~a" (+ (string->number curhapcount) 1))))
          ((and (seq? x (gdbm-fetch scheme-fighter-db "scheme-fighter-last-fighter")) (>= (string->number curhapcount) 9))
           (exit
            (say
             (format "~a: You must wait until someone else fights first before you can fight again during Happy Hour." x))))
          ((not (seq? x (gdbm-fetch scheme-fighter-db "scheme-fighter-last-fighter")))
           (gdbm-store-replace scheme-fighter-db "HappyHourSameFights" (format "~a" 1)))))
     ;; end happy hour fairness limit


     ;;Store off current
     (gdbm-store-replace scheme-fighter-db "scheme-fighter-last-fighter" (format "~a" x))


     ;; The kicker - unsecure it, kick, secure it
     (define (scheme-fighter-kick x y)
       (if #f ;;(not (scheme-fighter-member x scheme-fighter-no-kick))
	   (begin
	     (irc-unsecure irc-conn irc-secure-passwd)
	     (irc-kick irc-conn irc-primary-channel x y)
	     (irc-secure irc-conn irc-secure-passwd))))


     ;; Fighting yourself?
     (if (or (seq? x y)
             (seq? x (scheme-fighter-get-alias y))
             (seq? y (scheme-fighter-get-alias x))
             (seq? (scheme-fighter-get-alias x)
                          (scheme-fighter-get-alias y)))

         (begin
           (if (seq? (scheme-fighter-get-gender x) "M")
               (begin
                 (say (format "~a starts fighting himself...~%" x))
                 (say (format "~a punches himself in the throat.~%" x)))
               (begin
                 (say (format "~a starts fighting herself...~%" x))
                 (say (format "~a punches herself in the throat.~%" x))))
           (exit (scheme-fighter-kick x (format "You have killed yourself!~%")))))


     (define (scheme-fighter-milestone x)
       (if (= (remainder (+ (scheme-fighter-get-wins x) (scheme-fighter-get-losses x) 1) 500 ) 0) ;; Checks to see if this fight will make total fights a multiple of 500
           (begin
             (say (format "MILESTONE!!! ~a has now reached the ~a fight mark!!!" x (+ (scheme-fighter-get-wins x) (scheme-fighter-get-losses x) 1)))
             (if (= (remainder (+ (scheme-fighter-get-wins x) (scheme-fighter-get-losses x) 1) 5000 ) 0) ;; Checks to see if this fight will make total fights a multiple of 5000 
                 (begin
                   (say (format "~a gains 5 bonus levels!!!" x))
                   (scheme-fighter-set-rank x (+ (scheme-fighter-get-rank x) 5)))
                 )))
       )
     (scheme-fighter-milestone x)
     (scheme-fighter-milestone y)
     (say (format "~a[~a] starts a fight with ~a[~a]...~%"
                  x (scheme-fighter-get-hp x)
                  y (scheme-fighter-get-hp y)))
     (notice y (format "~a starts a fight with you.~%" x))

     (define (finish-him x y y-hp)
       (if (<= y-hp 0) ;; Finish him/her check
           (if (seq? (scheme-fighter-get-gender y) "M")
               (if (eq? mode 'public)
                   (begin
                     (say "FINISH HIM!!")
                     (sleep scheme-fighter-notify-sleep))
                   (begin
                     (notice x "FINISH HIM!!")
                     (sleep scheme-fighter-notify-sleep)))
               (if (eq? mode 'public)
                   (begin
                     (say "FINISH HER!!")
                     (sleep scheme-fighter-notify-sleep))
                   (begin
                     (notice x "FINISH HER!!")
                     (sleep scheme-fighter-notify-sleep))))))

     ;; Strike function
     (define (strike x y x-hit y-hp)
       (let
           ((smsg
             (case x-hit
               ((0)
                (format
                 (random
                  "~a jabs, but ~a[~a] parries.~%"
                  "~a swings, but misses ~a[~a].~%"
                  "~a dives, but ~a[~a] performs evasive maneuvers.~%"
                  "~a charges at ~a[~a], but trips.~%"
                  "~a throws a beer bottle at ~a[~a], but it hits a wall.~%"
                  "~a punches, ~a[~a] blocks the blow.~%"
                  "~a swings a baseball bat at ~a[~a], but strikes out.~%"
                  "~a goes throat punching, but ~a[~a] ducks.~%"
                  "~a stabs ~a[~a], but the blow is deflected~%"
                  "~a throws a ninja star at ~a[~a], but misses.~%"
                  "~a slaps, but ~a[~a] dodges.~%"
                  "~a attempts a super-move on ~a[~a], but is unsuccessful.~%"
                  "~a lunges at ~a[~a], but misses.~%")
                 x y y-hp))

               ((1 2 3 4 5 6 7 8 9 10)
                (format
                 (random
                  "~a clips ~a[~a].~%"
                  "~a delivers a glancing blow to ~a[~a].~%"
                  "~a lightly jabs ~a[~a].~%"
                  "~a steps on ~a[~a]'s toe.~%"
                  "~a bumps ~a[~a]'s funny bone.~%"
                  "~a gives ~a[~a] a splinter.~%"
                  "~a lightly kicks ~a[~a].~%"
                  "~a kicks ~a[~a] in the shins.~%"
                  "~a lightly punches ~a[~a].~%"
                  "~a nicks ~a[~a].~%"
                  "~a scrapes ~a[~a].~%"
                  "~a scratches ~a[~a].~%"
                  "~a pokes ~a[~a].~%"
                  "~a bumps ~a[~a].~%"
                  "~a swings a baseball bat at ~a[~a]'s head, but just catches a shoulder.~%"
                  "~a throws a mean roundhouse kick, but only catches ~a[~a]'s shoulder.~%"
                  "~a pulls out a shiv, but only nicks ~a[~a]'s arm.~%"
                  "~a attempts a super-move, but barely catches ~a[~a]'s arm.~%"
                  "~a tries to cause serious damage with a household object, but only nicks ~a[~a].~%"
                  "~a swings a broken bottle, but only gets ~a[~a]'s hand.~%"
                  "~a lightly strikes ~a[~a].~%")
                 x y y-hp))

               ((11 12 13 14 15 16 17 18 19 20)
                (format
                 (random
                  "~a jabs ~a[~a].~%"
                  "~a judo chops ~a[~a].~%"
                  "~a kicks ~a[~a].~%"
                  "~a knees ~a[~a].~%"
                  "~a knees ~a[~a] in the groin.~%"
                  "~a knees ~a[~a] in the junk.~%"
                  "~a picks up a chair and smashes it over ~a[~a]'s head.~%"
                  "~a punches ~a[~a].~%"
                  "~a punches ~a[~a] in the throat.~%"
                  "~a punches ~a[~a] in the junk.~%"
                  "~a shoves a forearm into ~a[~a]'s face.~%"
                  "~a smashes a bottle against ~a[~a]'s face.~%"
                  "~a stabs ~a[~a].~%"
                  "~a stabs ~a[~a] in the eye.~%"
                  "~a slaps ~a[~a].~%"
                  "~a pokes ~a[~a] with a pencil.~%"
                  "~a pokes ~a[~a] with a pen.~%"
                  "~a throws a small household object at ~a[~a].~%"
                  "~a strikes ~a[~a].~%"
                  "~a pulls out a Glock 21 and pistol whips ~a[~a].~%"
                  "~a lays a mean roundhouse kick onto ~a[~a]'s head.~%"
                  "~a sweeps ~a[~a] legs out.~%"
                  "~a throws a brick at ~a[~a].~%"
                  "~a drives a car over ~a[~a].~%"
                  "~a uses a pair of hardwood bokkens on ~a[~a].~%"
                  "~a hits ~a[~a] with a crowbar.~%"
                  "~a punches ~a[~a] in the kidney.~%"
                  "~a sprays pepper spray into ~a[~a]'s eyes.~%"
                  "~a pours battery acid onto ~a[~a].~%"
                  "~a beats ~a[~a] with a cane.~%"
                  "~a squirts lemon juice into ~a[~a]'s eye.~%"
                  "~a pokes ~a[~a] with a dirty hypodermic needle.~%"
                  "~a played knick-knack on ~a[~a]'s knee.~%"
                  "~a tosses a tarantula down ~a[~a]'s pants.~%"
                  "~a finds a belt and beats ~a[~a] raw --- Tony Soprano style.~%"
                  "~a gets ~a[~a] onto the ground, then kicks and kicks --- Goodfellas style.~%"
                  "~a opens up a can of whoop-ass on ~a[~a].~%"
                  "~a lays an uppercut into ~a[~a]'s chin.~%"
                  "~a throws a body blow and cracks two of ~a[~a]'s ribs.~%"
                  "~a whips out some bamboo and shoves it under ~a's toenails [~a].~%")
                 x y y-hp))

               (else
                (format
                 (random
                  (scheme-fighter-get-super-move x)
                  "~a punches ~a[~a] with tremendous power.~%"
                  "~a deals ~a[~a] a resounding blow.~%"
                  "~a lets loose a pack of wild dogs on ~a[~a] .~%"
                  "~a goes medieval on ~a[~a].~%"
                  "~a calls in an air strike on ~a[~a].~%"
                  "~a causes a nuclear meltdown in ~a[~a]'s vicinity.~%"
                  "~a deals ~a[~a] a bone-shattering blow.~%"
                  "~a deals ~a[~a] a massive blow.~%")
                 x y y-hp))
               )))
         (if (eq? mode 'public)
             (begin ;; Public fight mode
               (say smsg)
               (sleep scheme-fighter-notify-sleep))
             (begin ;; Private fight mode
               (notice x smsg)
               (notice y smsg)
               (sleep scheme-fighter-notify-sleep)))))

     ;; do-finish-him function
     (define (do-finish-him x y x-hit y-hp)
       (let
           ((smsg
             (case x-hit
               ((0)
                (format
                 (random
                  "~a attempts a finishing move on ~a[~a], but is unsuccessful.~%")
                 x y y-hp))

               ((1 2 3 4 5 6 7 8 9 10)
                (format
                 (random
                  (scheme-fighter-get-finish-him x))
                 x y y-hp))

               ((11 12 13 14 15 16 17 18 19 20)
                (format
                 (random
                  (scheme-fighter-get-finish-him x))
                 x y y-hp))

               (else
                (format
                 (random
                  (scheme-fighter-get-finish-him x))
                 x y y-hp))
               )))
         (if (eq? mode 'public)
             (begin ;; Public fight mode
               (say smsg)
               (sleep scheme-fighter-notify-sleep))
             (begin ;; Private fight mode
               (notice x smsg)
               (notice y smsg)
               (sleep scheme-fighter-notify-sleep)))))

     ;; Send Initial log of START
     (scheme-fighter-logger (format "START ~a,~a ~a" x y is-random))

     ;; Fight Looper
     (let loop  ((x-hp (scheme-fighter-get-hp x))
                 (y-hp (scheme-fighter-get-hp y))
                 (fround 0))

       (cond
        ;; Someone dead? kick the loser
        ((<= x-hp 0) (scheme-fighter-finish y y-hp x x-hp)
         (scheme-fighter-kick x (format "You have been killed by ~a." y)))
        ((<= y-hp 0) (scheme-fighter-finish x x-hp y y-hp)
         (scheme-fighter-kick y (format "You have been killed by ~a." x)))

        ;; Keep fighting
        (else
         (let ((x-hit (scheme-fighter-compute-damage x y))
               (y-hit (scheme-fighter-compute-damage y x))
               (x-hit2 (+ (scheme-fighter-compute-damage x y) 1))
               (y-hit2 (+ (scheme-fighter-compute-damage y x) 1))
               (x-hp2 x-hp)
               (y-hp2 y-hp))

           ;;(say (format "~a ~a / ~a ~a~%" x x-hit y y-hit))

           ;; Sneak attack for X on round 0
           (if (and (= fround 0) ; round 0
                    (< (scheme-fighter-get-exp x)
                       (* .5 (scheme-fighter-get-exp y))) ;exp very differnt
                    (< (random 100) 65)) ; 65% chance
               (set! x-hit  (round (* x-hit (+ 1 (/ (+ (random 100) 1)
                                                    100))))))

           ;; X strikes Y
           (set! y-hp2 (- y-hp x-hit))
           (strike x y x-hit y-hp2)
           ;; Multi-hit
           (if (scheme-fighter-get-extra-hit (scheme-fighter-get-rank x)
                                             (scheme-fighter-get-rank y)
                                             (scheme-fighter-get-hp x)
                                             x-hp2)
               (begin
                 (finish-him x y y-hp2)
                 (set! y-hp2 (- y-hp2 x-hit2))
                 (if (< y-hp2 1)
                     (do-finish-him x y x-hit2 y-hp2))
                 (if (>= y-hp2 1)
                     (strike x y x-hit2 y-hp2))
                 ))

           (if (< y-hp2 1) ; Y dead?
               (begin
                 (if (not (seq? (scheme-fighter-get-victory-cry x) "UNSET"))
                     (say (format "~a yells: ~a~%" x
                                  (scheme-fighter-get-victory-cry x))))
                 (say (format "~a cries out: ~a~%" y
                              (scheme-fighter-get-death-cry y)))
                 (scheme-fighter-logger (format "~a has been killed.~%" y)))
               (begin
                 ;; Y strikes X
                 (set! x-hp2 (- x-hp y-hit))
                 (strike y x y-hit x-hp2)
                 ;; Multi-hit
                 (if (scheme-fighter-get-extra-hit (scheme-fighter-get-rank y)
                                                   (scheme-fighter-get-rank x)
                                                   (scheme-fighter-get-hp y)
                                                   y-hp2)
                     (begin
                       (finish-him y x x-hp2)
                       (set! x-hp2 (- x-hp2 y-hit2))
                       (if (< x-hp2 1)
                           (do-finish-him y x y-hit2 x-hp2))
                       (if (>= x-hp2 1)
                           (strike y x y-hit2 x-hp2))
                       ))

                 (if (< x-hp2 1) ; X Dead?
                     (begin
                       (if (not (seq? (scheme-fighter-get-victory-cry y) "UNSET"))
                           (say (format "~a yells: ~a~%" y
                                        (scheme-fighter-get-victory-cry y))))
                       (say (format "~a cries out: ~a~%" x
                                    (scheme-fighter-get-death-cry x)))
                       (scheme-fighter-logger (format "~a has been killed.~%" x))))))

           (loop x-hp2 y-hp2 (+ fround 1))))
        )
       )
     )
   )
  )

;; Fvsstat macro
(define-syntax !scheme-fighter-vsstats (syntax-rules ()
                                         ((_ ?x ?y) (scheme-fighter-stats-vs
                                                     (format "~a" '?x)
                                                     (format "~a" '?y)))))

;; Fvstat macro (no args)
(define-syntax !scheme-fighter-vsstats-user (syntax-rules ()
                                              ((_ ?x) (scheme-fighter-stats-vs-user
                                                       (format "~a" '?x)))))

;; Fstat macro
(define-syntax !scheme-fighter-stats (syntax-rules ()
                                       ((_ ?x) (scheme-fighter-stats
                                                (format "~a" '?x)))))
;; Falias macro
(define-syntax !scheme-fighter-alias (syntax-rules (schemebot)
                                       ((_ ?x schemebot)
                                        (say "You cannot alias to schemebot!"))
                                       ((_ ?x ?y) (scheme-fighter-check-alias
                                                   (format "~a" '?y)))
                                       ((_ ?x) (scheme-fighter-check-alias
                                                (format "~a" '?x)))))
;; Fighter macro
(define-syntax !scheme-fighter (syntax-rules (schemebot)
                                 ((_ ?x schemebot ?c ?d)
                                  (say "You cannot fight schemebot!"))
                                 ((_ schemebot ?y ?c ?d)
                                  (say "You cannot fight schemebot!"))
                                 ((_ ?x ?y ?c ?d)
                                  (scheme-fighter (format "~a" '?x)
                                                  (format "~a" '?y)
                                                  ?c ?d))))

(define (jbaliasdumper key)
  (if key
      (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
        (if (regexp-match? "-alias" nkey)
            (irc-privmsg irc-conn "JackBlack" (format "~a: ~a~%" nkey (gdbm-fetch scheme-fighter-db nkey))))
        (jbaliasdumper nkey))
      #t))

(define (jbaliasdump)
  (jbaliasdumper (gdbm-firstkey scheme-fighter-db)))

(define (scheme-fighter-happyhour-hour)
  (let ((hour (gdbm-fetch happyhour-db "happyhour-hour")))
    (if hour hour "12")))

(define (scheme-fighter-happyhour-day)
  (let ((day (gdbm-fetch happyhour-db "happyhour-day")))
    (if day day "1")))

(define (!happyhour)
  (let ((hour (caddr (localtime->list (time))))
        (hhour (string->number (scheme-fighter-happyhour-hour)))
        (day (caddr (cddddr (localtime->list (time)))))
        (hday (string->number (scheme-fighter-happyhour-day)))
        (past (random "missed it..." "it's in the past, let it go."
                      "sorry" "maybe next week"))
        (future (random "soon... soon..." "patience...")))
    (cond
     ((< hday day) past)
     ((> hday day) future)
     ((= hday day)
      (cond
       ((< hhour hour) past)
       ((>= hhour hour) future))))))

(define (scheme-fighter-happyhour-checker a b c d e f g h i)
  (if (eq? g (string->number (scheme-fighter-happyhour-day)))
      (if (eq? c (string->number (scheme-fighter-happyhour-hour)))
          #t) #f))

(define (scheme-fighter-happyhour-check)
  (apply scheme-fighter-happyhour-checker (localtime->list (time))))

(define (scheme-fighter-super-move-dump key)
  (if key
      (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
        (if nkey
            (if (regexp-match? "-super-move" nkey)
                (if (not (regexp-match? "indescribable" (gdbm-fetch scheme-fighter-db nkey)))
                    (say (format "~a: ~a~%" (substring nkey 0 (- (string-length nkey) 11)) (gdbm-fetch scheme-fighter-db nkey))))))
        (scheme-fighter-super-move-dump nkey))
      #t))

(define (super-move-dump)
  (scheme-fighter-super-move-dump (gdbm-firstkey scheme-fighter-db)))

(define (scheme-fighter-death-cry-dump key)
  (if key
      (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
        (if nkey
            (if (regexp-match? "-death-cry" nkey)
                (if (not (regexp-match? "am too lame for a death" (gdbm-fetch scheme-fighter-db nkey)))
                    (say (format "~a: ~a~%" (substring nkey 0 (- (string-length nkey) 10)) (gdbm-fetch scheme-fighter-db nkey))))))
        (scheme-fighter-death-cry-dump nkey))
      #t))

(define (death-cry-dump)
  (scheme-fighter-death-cry-dump (gdbm-firstkey scheme-fighter-db)))

(define (scheme-fighter-victory-cry-dump key)
  (if key
      (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
        (if nkey
            (if (regexp-match? "-victory-cry" nkey)
                (if (not (regexp-match? "UNSET" (gdbm-fetch scheme-fighter-db nkey)))
                    (say (format "~a: ~a~%" (substring nkey 0 (- (string-length nkey) 12)) (gdbm-fetch scheme-fighter-db nkey))))))
        (scheme-fighter-victory-cry-dump nkey))
      #t))

(define (victory-cry-dump)
  (scheme-fighter-victory-cry-dump (gdbm-firstkey scheme-fighter-db)))

(define (backup-db)
  (define backup-db (gdbm-open "/tmp/backup.db"))

  (define (dumper key)
    (if key
        (let ((nkey (gdbm-nextkey scheme-fighter-db key)))
          (if nkey (gdbm-store-replace backup-db nkey (gdbm-fetch scheme-fighter-db nkey)))
          (dumper nkey))
        #t))

  (define (dump)
    (dumper (gdbm-firstkey scheme-fighter-db)))

  (dump))
