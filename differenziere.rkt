

(load "inf-to-pre.rkt")

; Regeln
(define regeln
  '(   
    (differenziere
     (d (restrict ? (lambda (f)(and(not(atom? f))
                                   (begin
                                     (set! a-list1
                                           (match '(* (> e1)(> e2)) f '()))
                                     (if (or (null? a-list1)(not a-list1)) #f #t)))))
        (> v1))
     '(+ (* ,(match-val ’e2 a-list1) (d ,(match-val 'e1 a-list1)
                                        ,(match-val 'v1 a-list)))
         (* ,(match-val ’e1 a-list1) (d ,(match-val 'e2 a-list1)
                                        ,(match-val 'v1 a-list)))
         produkt-regel))

     
     ; weitere Differentiationsregeln
     
     (differenziere
      ((+ f))
      (let ()
        (set! aktuelles-ziel 'vereinfache)
        (match-val 'f a-list))
      veraendere-ziel-regel)
     
     ; Vereinfachungsregeln
     (vereinfache (expt ? 0) 1 expt-0-regel)
     
     (vereinfache (* ? 0) 0 mult-0-regel)
     ; weitere Vereinfachungsregeln
     )
  )
  

; Steuerung

(define a-list '())
(define a-list1 '())
(define temp '())

(define steuere (let((temp1 '()))
                  (lambda()
                    (do ()
                      ((begin (set! temp1 (versuche-regeln regeln))
                              (if (or (null? temp1)(not temp1)) #t #f))
                       aktueller-ausdruck)
                      (newline)(write 'aktueller-ausdruck:)
                      (newline)(write aktueller-ausdruck)(newline)
))))




(define versuche-regeln
  (lambda(rest-regeln)
    (cond ((null? rest-regeln) (write a-list) '())
          ((atom? aktueller-ausdruck) '())
          ((begin (set! temp (vers-regel (car rest-regeln) aktueller-ausdruck))
                  (if (or (not temp)(null? temp)) #f #t))
           (set! aktueller-ausdruck temp))
          (else (versuche-regeln (cdr rest-regeln))))))


(define vers-regel
  (lambda(regel ausdruck)
    (let ((regel-ziel (car regel))
          (muster (cadr regel))
          (aktion (caddr regel)))
      (cond ((eq? aktuelles-ziel regel-ziel)
             (vers-regel1 ausdruck muster aktion regel))
            (else #f)))))


(define vers-regel1
  (lambda(ausdruck muster aktion regel)
    ;(write ’---vers-regel1:) (newline)(write ausdruck)(newline) (write muster) (newline)
    ;(write aktion)(newline)(write regel)(newline)
    (cond((atom? ausdruck) #f)
         ((begin(set! a-list (match muster ausdruck '()))
                (if (or(null? a-list)(not a-list)) #f #t))
          (feuere aktion regel))
         (else (vers-regel-liste ausdruck muster aktion regel)))))


(define vers-regel-liste
  (let ((temp '()))
    (lambda(ausdruck muster aktion regel)
      (cond((null? ausdruck) #f)
           ((begin (set! temp (vers-regel1 (car ausdruck) muster aktion regel))
                   (if (or(null? temp)(not temp)) #f #t))
            (cons temp (cdr ausdruck)))
           ((begin(set! temp (vers-regel-liste (cdr ausdruck) muster aktion regel))
                  (if (or(null? temp)(not temp)) #f #t))
            (cons (car ausdruck) temp))
           (else #f)))))

(define feuere
  (lambda(aktion regel)
    (newline)
    (write (caddr(cdr regel)))
    (write " feuert")(newline)
    (eval aktion (interaction-environment ))))

; Datenbasis
  (define aktuelles-ziel 'differenziere)
  (define aktueller-ausdruck '(d  (expt x 0)  ))