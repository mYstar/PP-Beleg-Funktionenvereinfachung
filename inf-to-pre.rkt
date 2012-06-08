

; Transformation Infix -> Prefix  (c) H. Fritzsche, 2012

(load "match.rkt")
          

(define inf-to-pre
   (lambda(e)         ; Infix-Notation -> Präfix-Notation
      (let ((alist '()))
         (cond ((atom? e) e)
               ((begin (set! alist (match '((> v)) e '())) alist)
                (inf-to-pre (match-val 'v alist )) )
               ((begin (set! alist (match '((+ l) (restrict ? eq+) (+ r)) e  '())) alist)
                 `(+ ,(inf-to-pre (match-val 'l  alist ))
                     ,(inf-to-pre (match-val 'r  alist )) ))
               ((begin (set! alist (match '((+ l) - (+ r)) e  '())) alist)
                 `(- ,(inf-to-pre (reverse(match-val 'r alist )))
                     ,(inf-to-pre (reverse(match-val 'l   alist ))) ))
               ((begin (set! alist (match '((+ l) * (+ r)) e '())) alist)
                 `(* ,(inf-to-pre (match-val 'l  alist ))
                     ,(inf-to-pre (match-val 'r  alist )) ))
               ((begin (set! alist (match '((+ l) / (+ r)) e '())) alist)
                 `(/ ,(inf-to-pre (reverse(match-val 'r alist )))
                     ,(inf-to-pre (reverse(match-val 'l  alist ))) ))
               ((begin (set! alist (match '((+ l) ^ (+ r)) e '())) alist)
                 `(expt ,(inf-to-pre (reverse(match-val 'r alist)))
                        ,(inf-to-pre (reverse(match-val 'l  alist ))) ))
               ((begin (set! alist (match '(- (+ r)) e '())) alist)
                 `(- ,(inf-to-pre  (match-val 'r  alist )) ))
               (else e) ))))  ; andere Ausdrücke werden nicht transformiert 
  
(define plus 
   (lambda(x) 
      (eq? x '+)))

(define match-val 
   (lambda(key alist) 
      (cadr (assoc key alist)) ))



; (inf-to-pre '(a expt b / (c * (- d)) - e + f))


