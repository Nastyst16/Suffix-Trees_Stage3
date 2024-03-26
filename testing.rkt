#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

;(provide (all-defined-out))

(define input-list (string->list "xabxabxaaxbbxabxabxaaxbb"))

(define (repeated-substring-of-given-length text len)

  (let* ((st (text->cst text))
         )

    (helper-calling-by-every-branch st len '())
    
    )

    

  )


(define (helper-calling-by-every-branch st len rez)

  ;(display (list 'aici: st))
  ;(newline)

  (cond
    ((st-empty? st) '())
    ((not (list? (first-branch st))) (helper-calling-by-every-branch (other-branches st) len rez)) ; nodul este o frunza;
    ;((null? (other-branches st)) '())
      
    (else

     ;(display (list 'eticheta: (get-branch-label (first-branch st))))
     ;(newline)

     (let ((substring-found (helper-for-each-branch (first-branch st) len rez)))

       (if (= (length substring-found) len)
           (take substring len)
           (helper-calling-by-every-branch (other-branches st) len rez)
           )
       )
     )
     

    )
  )

(define (helper-for-each-branch st len substring)

  ;(display (list 'aici: st))
  ;(display (list 'st: st))

  (display (list 'substring: substring))
  (newline)

  
  (cond 
      ((null? st) '() )
      ((not (list? st)) (helper-calling-by-every-branch st len substring)) ; suntem pe frunza

      (else

       (let* ((label-root (get-branch-label st))
              (appended-substring (append substring label-root))
              )

         (display (list 'ap-substr: appended-substring))


         

         appended-substring
    
         )
       )
      )
  
  
  

  )