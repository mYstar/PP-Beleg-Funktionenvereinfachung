

; Mustervergleicher (c) H. Fritzsche, 2012

         
(define atom? (lambda(o)(or(symbol? o)
                          (null? o)
			  (number? o))))

(define  match 
   (lambda(p d assignments)
       (cond ((and (null? p) (null? d))
                 ; Erfolg
              (cond ((null? assignments) #t)
                    (else assignments) ))
                 ; true, falls Muster und Datum "leer" sind
             ((or (null? p) (null? d))  #f)
                 ; Misserfolg, eine Liste ist kürzer
             ((or (eq? (car p) '?)
                 ; == Joker-Klausel ==
                  (equal? (car p)(car d)) )  
                 ; die betrachteten Elemente sind gleich
                (match (cdr p)(cdr d) assignments) ) 
             ((eq? (car p) '+)
                  (or (match (cdr p)(cdr d) assignments)
                      (match p (cdr d) assignments)))
             ((atom? (car p)) #f)            
                 ; Misserfolg
             ((eq? (pattern-indicator (car p)) '>)
	        (match (cdr p)(cdr d) 
                   (shove-gr (pattern-variable (car p))
                             (car d)
			      assignments )) ) 
             ((eq? (pattern-indicator (car p)) '<)
               (match (cons (pull-value
                               (pattern-variable (car p))
                               assignments)
                            (cdr p))
                      d assignments))
             ((eq? (pattern-indicator (car p)) '+)
                (let ((new-assignments (shove-pl (pattern-variable (car p))
                                                 (car d)
                                                 assignments)))
                   (or (match (cdr p)(cdr d) new-assignments)
                       (match p (cdr d) new-assignments) ))) 
             ((and (eq? (pattern-indicator (car p)) 'restrict)
                  (eq? (restriction-indicator (car p)) '?)
                  (test (restriction-predicates (car p))
                        (car d)) )
               (match (cdr p) (cdr d) assignments)) 
             (else #f) )))

(define shove-gr 
   (lambda(variable item a-list)
      (append a-list `((,variable ,item)))))

(define pattern-indicator 
   (lambda(l)
      (car l)))

(define pattern-variable 
   (lambda(l)
      (cadr l))) 

(define shove-pl 
   (lambda(variable item a-list)
      (cond((null? a-list) `((,variable ( ,item))) )
           ((eq? variable (caar a-list))
              (cons `(,variable ,(append (cadar a-list) `( ,item)))
                     (cdr a-list)) )
           (else (cons (car a-list)
                       (shove-pl variable item (cdr a-list)) )) )))
(define pull-value 
   (lambda(variable a-list)
      (cadr (assoc variable a-list)) ))

(define restriction-indicator
  (lambda(l)
    (cadr l)))

(define restriction-predicates
  (lambda(l)
    (cddr l)))

(define eq+ (lambda(z)(eq? z '+)))

(define test 
   (lambda(predicates  arg)
      (cond ((null? predicates) #t)
            (((eval(car predicates)(interaction-environment)) arg)
              (test  (cdr predicates) arg))
            (else #f))))

; (match '(a (restrict ? eq+) b) '(a + b) '())