#lang racket

(define *deck* (for*/list ((suit '(♥ ♣ ♠ ♦))
                           (face '(2 3 4 5 6 7 8 9 10 J Q K A)))
                 (cons face suit)))

(define face-value (match-lambda ('A 14) ('K 13) ('Q 12) ('J 11) (x x)))

(define (card>? c1 c2)
  (> (face-value (car c1)) (face-value (car c2))))

(define render-card (match-lambda ((cons f s) (format "~a~a" f s))))

(define (redact-hidden-card c)
  (if (cdr c) (render-card (car c)) "??"))

(define (redact-deck deck)
  (string-join (map redact-hidden-card deck) " "))

(define ((win-lose-or-draw #:> (> card>?) won drawn (win void) (lose void)) v1 v2 (p1 #f) (p2 #f))
  (cond [(> v1 v2) (won 1 (win p1 p2) (lose p2 p1))]
        [(> v2 v1) (won 2 (lose p1 p2) (win p2 p1))]
        [else (drawn p1 p2)]))

(define phyrric-battle
  (win-lose-or-draw #:> >
                    (λ (p . _) (format "pyrrhic victory to player ~a" p))
                    (thunk "loss")))

(define overwhelming-battle
  (win-lose-or-draw #:> (λ (p _) (< p 4))
                    (λ (p . _) (format "player ~a can’t win" p))
                    (λ (x _) (x))))

(define (on-battle-won p deck-1′ deck-2′)
  (printf "battle won by player ~a~%" p)
  (turn deck-1′ deck-2′))

(define (on-battle-drawn battle-args-1 battle-args-2)
  (apply battle (append battle-args-1 battle-args-2)))

(define (win-battle winner loser)
  (match-define (list winner-deck (list (app car victorious-battle-cards) ...)) winner)
  (match-define (list _ (list (app car battle-spoils-cards) ...)) loser)
  (append winner-deck victorious-battle-cards battle-spoils-cards))

(define (lose-battle loser _) (first loser))

(define win-lose-or-draw-battle
  (win-lose-or-draw on-battle-won on-battle-drawn win-battle lose-battle))

(define (flop-onto-army army c1 c2 c3 c4)
  (cons (cons c4 #t) (append (map (curryr cons #f) (list c3 c2 c1)) army)))

(define (battle deck-1 army-1 deck-2 army-2)
  (match* (deck-1 deck-2)
    (((app length l1) (app length l2))
     (=> lengths-ok)
     (if (and (< l1 4) (< l2 4)) (phyrric-battle l1 l2) (overwhelming-battle l1 l2 lengths-ok null)))
    (((list a1 a2 a3 a4 deck-1′ ...) (list b1 b2 b3 b4 deck-2′ ...))
     (define army-1′ (flop-onto-army army-1 a1 a2 a3 a4))
     (define army-2′ (flop-onto-army army-2 b1 b2 b3 b4))
     (printf "P1 ~a vs~%P2 ~a~%" (redact-deck army-1′) (redact-deck army-2′))
     (win-lose-or-draw-battle (caar army-1′) (caar army-2′)
                              (list deck-1′ army-1′) (list deck-2′ army-2′)))))

(define (on-turn-won p deck-1 deck-2)
  (printf "player ~a wins turn~%" p)
  (turn deck-1 deck-2))

(define (on-turn-drawn deck-1 deck-2)
  (match* (deck-1 deck-2)
    ((`(,card-1 ,@deck-1′) `(,card-2 ,@deck-2′))
     (printf "~%BATTLE COMMENCES WITH: ~a vs ~a~%" (render-card card-1) (render-card card-2))
     (battle deck-1′ `((,card-1 . #t)) deck-2′ `((,card-2 . #t))))))

(define (win-turn winner loser)
  (match* (winner loser)
    ((`(,keeper ,@deck) `(,forfeit ,@_)) `(,@deck ,keeper ,forfeit))))

(define (lose-turn loser _)
  (match loser (`(,forfeit ,@remains) remains)))

(define win-lose-or-draw-on-turn (win-lose-or-draw on-turn-won on-turn-drawn win-turn lose-turn))

(define (turn deck-1 deck-2)
    (printf "turn: ~s vs ~s: " (length deck-1) (length deck-2))
    (match* (deck-1 deck-2)
      (('() '()) "a draw")
      (('() _) "player 2 wins")
      ((_ '()) "player 1 wins")
      ((`(,card-1 ,@_) `(,card-2 ,@_))
       (printf "~a vs ~a: " (render-card card-1) (render-card card-2))
       (win-lose-or-draw-on-turn card-1 card-2 deck-1 deck-2))))

(define (war) (call-with-values (λ () (split-at (shuffle *deck*) 26)) turn))

(war)