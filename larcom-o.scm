; assumed structure of the tic-tac-toe playing field:
; A1 A2 A3
; B1 B2 B3
; C1 C2 C3

; To force the system to begin first, enter:
; (() . A1)
; - i.e. an empty co-ordinate

(define (proto-terminate-or-reply coordlist checkedpart machinesym)
  (if (null? coordlist) '(NO WINNER) ; i.e. no free boxes left -
                                     ; the game terminates
    (if (null? (caar coordlist))
      ; if an empty position has been found, naively place the
      ; machine symbol there and reply:
      (append (reverse (cons (cons machinesym (cdar coordlist)) checkedpart))
              (cdr coordlist))
      ; else look for the next empty position
      (proto-terminate-or-reply (cdr coordlist)
        (cons (car coordlist) checkedpart) machinesym))))

(define (terminate-or-reply coordlist machinesym)
  (proto-terminate-or-reply coordlist '() machinesym))
; sample calls:
; (terminate-or-reply '((X . A1) (O . A2) (X . A3)
;   (O . B1) (() . B2) (() . B3) (() . C1) (() . C2) (() . C3)) 'X)
; --> ((X . A1) (O . A2) (X . A3)
;      (O . B1) (X . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
; (terminate-or-reply '((X . A1) (O . A2) (X . A3)
;   (X . B1) (O . B2) (O . B3) (O . C1) (X . C2) (X . C3)) 'X)
; --> (NO WINNER)

; check has anybody won the game:
(define (victory coordlist)
  (let ((puresyms (map car coordlist)))

    (if (and (equal? (list-ref puresyms 0) (list-ref puresyms 3))
             (equal? (list-ref puresyms 0) (list-ref puresyms 6))
             (not (null? (list-ref puresyms 0))))
      (append '(WINNER) (list (list-ref puresyms 0)))

    (if (and (equal? (list-ref puresyms 1) (list-ref puresyms 4))
             (equal? (list-ref puresyms 1) (list-ref puresyms 7))
             (not (null? (list-ref puresyms 1))))
      (append '(WINNER) (list (list-ref puresyms 1)))

    (if (and (equal? (list-ref puresyms 2) (list-ref puresyms 5))
             (equal? (list-ref puresyms 2) (list-ref puresyms 8))
             (not (null? (list-ref puresyms 2))))
      (append '(WINNER) (list (list-ref puresyms 2)))

    (if (and (equal? (list-ref puresyms 0) (list-ref puresyms 1))
             (equal? (list-ref puresyms 0) (list-ref puresyms 2))
             (not (null? (list-ref puresyms 0))))
      (append '(WINNER) (list (list-ref puresyms 0)))

    (if (and (equal? (list-ref puresyms 3) (list-ref puresyms 4))
             (equal? (list-ref puresyms 3) (list-ref puresyms 5))
             (not (null? (list-ref puresyms 3))))
      (append '(WINNER) (list (list-ref puresyms 3)))

    (if (and (equal? (list-ref puresyms 6) (list-ref puresyms 7))
             (equal? (list-ref puresyms 6) (list-ref puresyms 8))
             (not (null? (list-ref puresyms 6))))
      (append '(WINNER) (list (list-ref puresyms 6)))

    (if (and (equal? (list-ref puresyms 0) (list-ref puresyms 4))
             (equal? (list-ref puresyms 0) (list-ref puresyms 8))
             (not (null? (list-ref puresyms 0))))
      (append '(WINNER) (list (list-ref puresyms 0)))

    (if (and (equal? (list-ref puresyms 2) (list-ref puresyms 4))
             (equal? (list-ref puresyms 2) (list-ref puresyms 6))
             (not (null? (list-ref puresyms 2))))
      (append '(WINNER) (list (list-ref puresyms 2)))

    '()))))))))))

; sample calls:
; (victory '((X . A1) (O . A2) (X . A3) (O . B1) (X . B2) (O . B3)
;   (X . C1) (() . C2) (() . C3)))
; --> (WINNER X)
; (victory '((X . A1) (O . A2) (X . A3) (X . B1) (O . B2) (() . B3)
;   (() . C1) (O . C2) (() . C3)))
; --> (WINNER O)
; (victory '((X . A1) (O . A2) (X . A3) (O . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
; --> ()

(define (proto-xlatcoord input-coordinate mainlist checkedpart)
  (if (null? mainlist) (reverse checkedpart) ; omission of the the turn
                                             ; - should not happen
    (if (equal? (cdar mainlist) (cdr input-coordinate))
      (append (reverse (cons input-coordinate checkedpart)) (cdr mainlist))
      (proto-xlatcoord
        input-coordinate (cdr mainlist) (cons (car mainlist) checkedpart)))))

(define (xlatcoord input-coordinate mainlist)
  (proto-xlatcoord input-coordinate mainlist '()))

; update the knowledge or add new knowledge:
(define (proto-testknowledge newpair knowledgelist checkedpart)
  (if (null? knowledgelist)
    (cons newpair (reverse (cdr checkedpart)))
    (if (equal? (caar knowledgelist) (car newpair))
      (cons newpair (append (reverse checkedpart) (cdr knowledgelist)))
      (proto-testknowledge
        newpair (cdr knowledgelist) (cons (car knowledgelist) checkedpart)))))

(define (testknowledge newpair knowledgelist)
  (proto-testknowledge newpair knowledgelist '()))
; sample calls:
; (testknowledge '(X . Y) '((A . B) (C . D) (X . Z) (E . F)))
; --> ((X . Y) (A . B) (C . D) (E . F))
; (testknowledge '(X . Y) '((A . B) (C . D) (R . Z) (E . F)))
; --> ((X . Y) (A . B) (C . D) (R . Z))

(define (proto-findanswer challenge knowledgelist checkedpart)
  (if (null? knowledgelist) '()
    (if (equal? (caar knowledgelist) challenge)
      ; return as answer a re-cycled knowledgelist, with the answer
      ; in the first element:
      (cons (car knowledgelist)
        (append (reverse checkedpart) (cdr knowledgelist)))
      ; else, re-call:
      (proto-findanswer
        challenge (cdr knowledgelist)
        (cons (car knowledgelist) checkedpart)))))

(define (findanswer challenge knowledgelist)
  (proto-findanswer challenge knowledgelist '()))
; sample calls:
; (findanswer 'X '((A . B) (C . D) (X . Y) (E . F)))
; --> ((X . Y) (A . B) (C . D) (E . F))
; (findanswer 'Z '((A . B) (C . D) (X . Y) (E . F)))
; --> ()

(define (testvictnoans answerlist)
  (if (equal? (length (car answerlist)) 2) answerlist
  ; i.e. we got either (NO WINNER) or (WINNER X/O),
  ; then do not change anything.
    (let ((vict (victory (car answerlist))))
      (if (not (null? vict)) ; then cons the victory to the front
        (cons (cons vict (car answerlist)) (cdr answerlist))
        (if (equal? '(NO WINNER) (terminate-or-reply (car answerlist) 'Y))
          ; the symbol above is "Y" as it is really unimportant.
          (cons (cons '(NO WINNER) (car answerlist)) (cdr answerlist))
          ; if no such special case has occurred,
          ; simply reply as originally envisioned:
          answerlist)))))


; if lastreply is null, create it!
; THIS IS THE MAIN FUNCTION DRIVING THE REASONING:
(define
  (proto-interaction lastreply input-coordinate knowledgelist machinesym)
  ; first, let's gain a "civilised" version of the last reply
  ; - "()" is pretty useless, so:
  (let ((xlatc (if (null? lastreply)
                 (xlatcoord input-coordinate
                   '((() . A1) (() . A2) (() . A3)
                     (() . B1) (() . B2) (() . B3)
                     (() . C1) (() . C2) (() . C3)))
                 (xlatcoord input-coordinate lastreply))))
    ; now let us find out, does the system know the answer to
    ; the current case, or is the user victorious:
    (let ((findans (findanswer xlatc knowledgelist))
          (vict (victory xlatc)))

      ; if the victory is not null, save it as the last reply and
      ; return the victory, together with the updated knowledgelist;
      ; victory is always achieved "creatively", i.e. there is no
      ; hypothetic answer to victory in xlatc:
      (if (not (null? vict))
        (cons vict (testknowledge (cons lastreply xlatc) knowledgelist))

        ; else, if there was no victory of the user:
        (if (not (null? findans))
          ; i.e. there is a known answer return it, together with the changed
          ; knowledgelist - and test for victory or no answer:
          (testvictnoans
            (cons (cdar findans)
                  (testknowledge (cons lastreply xlatc) findans)))
          ; now (car findans) itself might be a victory - let that
          ; be checked by the superior function.

          ; else, if there is no known answer - still,
          ; do test for victory or no answer:
          (testvictnoans (cons
            (terminate-or-reply xlatc machinesym)
            (testknowledge (cons lastreply xlatc) knowledgelist))))))))

; sample calls:

; (proto-interaction '((() . A1) (() . A2) (() . A3)
;                      (() . B1) (X . B2) (() . B3)
;                      (() . C1) (() . C2) (() . C3))
;
;   '(O . A1)
;
;   '((((() . A1) (() . A2) (() . A3)
;       (() . B1) (() . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))))
;
;   'X)

; --> ((((O . A1) (X . A2) (() . A3)
;        () . B1) (X . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (X . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  (O . A1) (() . A2) (() . A3)
;  (() . B1) (X . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))))


; (proto-interaction '((() . A1) (() . A2) (() . A3)
;                      (() . B1) (() . B2) (() . B3)
;                      (() . C1) (() . C2) (() . C3))
;
;   '(X . A1)
;
;   '((((() . A1) (() . A2) (() . A3)
;       (() . B1) (() . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))))
;
;   'O)

; --> (((X . A1) (O . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  (X . A1) (() . A2) (() . A3)
; (() . B1) (() . B2) (() . B3)
; (() . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))))


; (proto-interaction '((() . A1) (X . A2) (X . A3)
;                       (O . B1) (O . B2) (() . B3)
;                       (() . C1) (() . C2) (() . C3))
;
;   '(X . A1)
;
;   '((((() . A1) (() . A2) (() . A3)
;       (() . B1) (() . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))))
;
;   'O)

; --> ((WINNER X)
;
; (((() . A1) (X . A2) (X . A3)
;    (O . B1) (O . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))
;  (X . A1) (X . A2) (X . A3)
;  (O . B1) (O . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))))


; (proto-interaction '((O . A1) (X . A2) (O . A3)
;                     (() . B1) (X . B2) (X . B3)
;                      (X . C1) (O . C2) (() . C3))
;
;   '(O . B1)
;
;   '((((() . A1) (() . A2) (() . A3)
;       (() . B1) (() . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))))
;
;   'X)

; --> (((NO WINNER) (O . A1) (X . A2) (O . A3)
;                   (O . B1) (X . B2) (X . B3)
;                   (X . C1) (O . C2) (X . C3))
;
; (((O . A1) (X . A2) (O . A3)
;  (() . B1) (X . B2) (X . B3)
;   (X . C1) (O . C2) (() . C3))
;  (O . A1) (X . A2) (O . A3)
;  (O . B1) (X . B2) (X . B3)
;  (X . C1) (O . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))))


; (proto-interaction '((O . A1) (() . A2) (O . A3)
;                      (X . B1) (X . B2) (O . B3)
;                      (() . C1) (() . C2) (() . C3))
;
;   '(X . C1)
;
;   '((((() . A1) (() . A2) (() . A3)
;       (() . B1) (() . B2) (() . B3)
;       (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3)))
;
;   (((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;   ((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))))
;
;   'O)

; --> (((WINNER O) (O . A1) (O . A2) (O . A3)
;                  (X . B1) (X . B2) (O . B3)
;                  (X . C1) (() . C2) (() . C3))
;
; (((O . A1) (() . A2) (O . A3)
;   (X . B1) (X . B2) (O . B3)
;   (() . C1) (() . C2) (() . C3))
;  (O . A1) (() . A2) (O . A3)
;  (X . B1) (X . B2) (O . B3)
;  (X . C1) (() . C2) (() . C3))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3)))
;
; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
;  ((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))))

; Now, cycle the actual interaction with the user:
(define (interaction answerlist machinesym)
  (if (or (equal? (caar answerlist) '(WINNER O))
          (equal? (caar answerlist) '(WINNER X))
          (equal? (caar answerlist) '(NO WINNER))
          (equal? (caar answerlist) 'WINNER)
          (equal? (caar answerlist) 'NO))
    (begin (display (car answerlist)) (newline)
           (display '(GAME OVER)) (newline)
      ; dump the changed knowledge file - this is a later addition,
      ; otherwise just close the "begin" above:
      (let ((outknowledge (open-output-file "tictacto.txt")))
        (begin (display (cdr answerlist) outknowledge)
          (close-output-port outknowledge))))
    (begin (display (append '(MACHINE REPLY IS) (car answerlist))) (newline)
      (interaction
        (proto-interaction (car answerlist)
          (begin (display '(INPUT YOUR TURN)) (newline) (read))
          (cdr answerlist) machinesym)
        machinesym))))

; sample interaction:
; (interaction (cons
;    '((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;
;    '((((() . A1) (() . A2) (() . A3)
;        (() . B1) (() . B2) (() . B3)
;        (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))
;
;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))
;
;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))
;
;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))))
;
;    'X)

; -->
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (() . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . A2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (O . A2) (() . A3)
;                   (() . B1) (() . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (O . A2) (X . A3)
;                   (() . B1) (O . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . C2) ; -- WRITTEN BY USER
; (WINNER O)
; (GAME OVER)

; (define knownpairs

; '((((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))

;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))

;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))

;    (((() . A1) (() . A2) (() . A3)
;      (() . B1) (() . B2) (() . B3)
;      (() . C1) (() . C2) (() . C3))
;    ((() . A1) (() . A2) (() . A3)
;     (() . B1) (() . B2) (() . B3)
;     (() . C1) (() . C2) (() . C3)))))

; (define (main)
;   (let ((machsym (begin
;                    (newline) (display '(WILL YOU PLAY WITH X OR O))
;                    (newline) (read))))
;     (if (equal? machsym 'X)
;       (interaction
;         (cons '((() . A1) (() . A2) (() . A3)
;                 (() . B1) (() . B2) (() . B3)
;                 (() . C1) (() . C2) (() . C3)) knownpairs)
;         'O)
;       (interaction
        
        ; This is so the machine does not always start at the same position
        ; - just "nicer", not "necessary":
;         (cons
;           (xlatcoord
;             (begin
;               (newline)
;               (display '(PLEASE INPUT THE MACHINE STARTING MOVE))
;               (newline) (read))
;             '((() . A1) (() . A2) (() . A3)
;               (() . B1) (() . B2) (() . B3)
;               (() . C1) (() . C2) (() . C3)))
;           knownpairs)
;         'X))))

; sample call:
; (main)
;
; (WILL YOU PLAY WITH X OR O)
; X ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (() . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . B1) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (O . A1) (() . A2) (() . A3)
;                   (X . B1) (() . B2) (() . B3)
;                  (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (O . A1) (O . A2) (() . A3)
;                   (X . B1) (X . B2) (() . B3)
;                  (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . C1) ; -- WRITTEN BY USER
; ((WINNER O) (O . A1) (O . A2) (O . A3)
;             (X . B1) (X . B2) (() . B3)
;             (X . C1) (() . C2) (() . C3))
; (GAME OVER)

(define knownpairs '())

(define (main)
  (let ((kchains (open-input-file "tictacto.txt")))
    (begin (set! knownpairs (read kchains)) (close-input-port kchains)
      (let ((machsym (begin
                       (newline) (display '(WILL YOU PLAY WITH X OR O))
                       (newline) (read))))
        (if (equal? machsym 'X)
          (interaction
            (cons '((() . A1) (() . A2) (() . A3)
                    (() . B1) (() . B2) (() . B3)
                    (() . C1) (() . C2) (() . C3)) knownpairs)
            'O)
          (interaction
        
            ; This is so the machine does not always start at
            ; the same position - just "nicer", not "necessary":
            (cons
              (xlatcoord
                (begin
                  (newline)
                  (display '(PLEASE INPUT THE MACHINE STARTING MOVE))
                  (newline) (read))
                '((() . A1) (() . A2) (() . A3)
                  (() . B1) (() . B2) (() . B3)
                  (() . C1) (() . C2) (() . C3)))
              knownpairs)
            'X))))))
; sample interaction:
; (main)
;
; (WILL YOU PLAY WITH X OR O)
; O ; -- WRITTEN BY USER
;
; (PLEASE INPUT THE MACHINE STARTING MOVE)
; (X . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . A3) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . C3)
; (MACHINE REPLY IS (X . A1) (X . A2) (O . A3)
;                  (() . B1) (X . B2) (() . B3)
;                  (() . C1) (() . C2) (O . C3))
; (INPUT YOUR TURN)
; (O . B3)
; (WINNER O)
; (GAME OVER)

; thereby, tictacto.txt has been amended from:

; ((((() . A1) (() . A2) (() . A3)
;    (() . B1) (() . B2) (() . B3)
;    (() . C1) (() . C2) (() . C3))
; ((() . A1) (() . A2) (() . A3)
;  (() . B1) (() . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3)))

; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; ((() . A1) (() . A2) (() . A3)
;  (() . B1) (() . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3)))

; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; ((() . A1) (() . A2) (() . A3)
;  (() . B1) (() . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3)))

; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; ((() . A1) (() . A2) (() . A3)
;  (() . B1) (() . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3)))))

; before the game to

; ((((X . A1) (X . A2) (O . A3)
;   (() . B1) (X . B2) (() . B3)
;   (() . C1) (() . C2) (O . C3))
; (X . A1) (X . A2) (O . A3)
; (() . B1) (X . B2) (O . B3)
; (() . C1) (() . C2) (O . C3))

; (((X . A1) (() . A2) (O . A3)
;   (() . B1) (X . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; (X . A1) (() . A2) (O . A3)
; (() . B1) (X . B2) (() . B3)
; (() . C1) (() . C2) (O . C3))

; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (X . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; (() . A1) (() . A2) (O . A3)
; (() . B1) (X . B2) (() . B3)
; (() . C1) (() . C2) (() . C3))

; (((() . A1) (() . A2) (() . A3)
;   (() . B1) (() . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; ((() . A1) (() . A2) (() . A3)
;  (() . B1) (() . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3))))

; after the game.

; It DID learn this and can use it, but having a too limited memory capacity,
; it starts to make mistakes - here is the above game in reverse:
; (main)
;
; (WILL YOU PLAY WITH X OR O)
; X ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (() . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . A1) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (O . C3))
; (INPUT YOUR TURN)
; (X . A2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (X . A2) (O . A3)
;                   (O . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (O . C3))
; (INPUT YOUR TURN)
; (X . C1) ; -- WRITTEN BY USER
; ((WINNER O) (X . A1) (X . A2) (O . A3)
;             (O . B1) (X . B2) (O . B3)
;             (X . C1) (() . C2) (O . C3))
; (GAME OVER)

; after this game, tictacto.txt looks like:
; ((((X . A1) (X . A2) (O . A3)
;    (O . B1) (X . B2) (() . B3)
;    (() . C1) (() . C2) (O . C3))
; (X . A1) (X . A2) (O . A3)
; (O . B1) (X . B2) (() . B3)
; (X . C1) (() . C2) (O . C3))

; (((X . A1) (() . A2) (O . A3)
;  (() . B1) (X . B2) (() . B3)
;  (() . C1) (() . C2) (O . C3))
; (X . A1) (X . A2) (O . A3)
; (() . B1) (X . B2) (() . B3)
; (() . C1) (() . C2) (O . C3))

; (((() . A1) (() . A2) (O . A3)
;   (() . B1) (X . B2) (() . B3)
;   (() . C1) (() . C2) (() . C3))
; (X . A1) (() . A2) (O . A3)
; (() . B1) (X . B2) (() . B3)
; (() . C1) (() . C2) (() . C3))

; (((X . A1) (() . A2) (O . A3)
;  (() . B1) (X . B2) (() . B3)
;  (() . C1) (() . C2) (() . C3))
; (X . A1) (() . A2) (O . A3)
; (() . B1) (X . B2) (() . B3)
; (() . C1) (() . C2) (O . C3)))

; increasing the challenge-reply-pairs in memory to 20, full mirroring
; of the game is possible which proves that the machine indeed learned
; "the proper replies":

; (main)
;
; (WILL YOU PLAY WITH X OR O)
; O ; -- WRITTEN BY USER

; (PLEASE INPUT THE MACHINE STARTING MOVE)
; (X . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . A3) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . B3) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (X . A2) (O . A3)
;                   (() . B1) (X . B2) (O . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (O . C3) ; -- WRITTEN BY USER
; (WINNER O)
; (GAME OVER)

; (main)
;
; (WILL YOU PLAY WITH X OR O)
; X ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (() . A3)
;                   (() . B1) (() . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . B2) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (() . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (() . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . A1) ; -- WRITTEN BY USER
; (MACHINE REPLY IS (X . A1) (() . A2) (O . A3)
;                   (() . B1) (X . B2) (O . B3)
;                   (() . C1) (() . C2) (() . C3))
; (INPUT YOUR TURN)
; (X . A2) ; -- WRITTEN BY USER
; ((WINNER O) (X . A1) (X . A2) (O . A3)
;             (() . B1) (X . B2) (O . B3)
;             (() . C1) (() . C2) (O . C3))
; (GAME OVER)

