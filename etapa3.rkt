#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)

  (st-has-pattern? (text->ast text) pattern)

  )


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).
(define (longest-common-substring text1 text2)

  (let ((st-text1 (text->ast text1))
         (suffixes-text2 (get-suffixes text2))
         )

    (let iter ((st st-text1) (suffixes suffixes-text2) (rez '()))
    (cond
      ((null? suffixes) rez)
      ((< (length rez) (length(helper-builder st (car suffixes)))) (iter st (cdr suffixes) (helper-builder st (car suffixes))))
      (else (iter st (cdr suffixes) rez)))
      )
    )
  )

(define (helper-builder st suffix)

  (if (st-has-pattern? st suffix)
      suffix
      (helper-builder st (drop-right suffix 1))
      )
  )

(define input-list (string->list "xabxabxaaxbbxabxabxaaxbb"))
; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
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
    ((null? st) '())
    ((null? (first-branch st)) '()) ; nodul este o frunza;
    ((null? (other-branches st)) '())
      
    (else

     ;(display (list 'eticheta: (get-branch-label (first-branch st))))
     ;(newline)
     
     (helper-for-each-branch (first-branch st) len rez)
     (helper-calling-by-every-branch (other-branches st) len rez)
     
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
      ((pair? (first-branch st)) '()) ; suntem pe frunza

      (else

       (let* ((label-root (get-branch-label st))
              )


         (append substring label-root)

         (display substring) (newline)

         (if (>= (length substring) len)
             (take substring len)
             (helper-calling-by-every-branch st len substring)
             )
    

    
         )
       )
      )
  
  
  

  )

         

              

(define tree
  '(((#\$))
  ((#\a)
   ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
   ((#\b #\x #\a)
    ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
    ((#\b #\x #\a #\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$))))
   ((#\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$))))
  ((#\b)
   ((#\$))
   ((#\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
   ((#\x #\a)
    ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
    ((#\b #\x #\a)
     ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
     ((#\b #\x #\a #\a #\x #\b #\b #\$)))))
  ((#\x)
   ((#\a)
    ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
    ((#\b #\x #\a)
     ((#\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))
     ((#\b #\x #\a #\a #\x #\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))))
   ((#\b #\b) ((#\$)) ((#\x #\a #\b #\x #\a #\b #\x #\a #\a #\x #\b #\b #\$)))))
  )


#|
    (if (equal? label-root '(#\$))
        '()
        (let ((rest-of-len (let iter ((len len) (word label-root) (rez '()))
                             (if (null? word)
                                 rez
                                 (iter (- len 1) (cdr word) (append rez (car word)))
                                 )
                             )
                           )
              )
          (display (list 'r-o-l: rest-of-len))
          (newline)

          (helper-calling-by-every-branch (first-branch st) len rest-of-len)
          
          )
        
        
        )
    |#
         






#|
(let* ((suffix-tree (text->cst text))
         ;(rez
         )

    suffix-tree

    ;(display (list 'afisare: (other-branches suffix-tree)))
    ;(newline)
    

    (define rez
      (let preorder-transversal ((suffix-tree suffix-tree) (len len))
        (cond
          ((null? tree) '())
          ((null? (first-branch suffix-tree)) '()) ; e frunza: nu fac nimic
          (else
           (let ((root (first-branch suffix-tree))
                 (branches (other-branches suffix-tree))
                 (result '()))

             (append (list root)
                     (apply append (map (lambda (branch) (preorder-transversal branch len) branches)))
                     )
             )
           )
          )
        )
      )

    (display rez)
    rez
    
    )
    

    



    
    (let iter ((st suffix-tree) (len len) (rez '()))
      (cond
        ((null? (other-branches st)) 'aici)
        ((>= (length (get-branch-label (first-branch st))) len)
         (continueing-the-searching-after-substring-found (get-branch-label (first-branch))))


        ;daca mai are copii intram in copii
        ((not (null? (first-branch st))) (iter (first-branch st) len rez))
        
        (else


         (display (list 'oth-br (other-branches st))) (newline)

                  
         (iter (other-branches st) len rez))

        )
      )
    |#

      

;(repeated-substring-of-given-length input-list 10)


(define (continueing-the-searching-after-substring-found substring)

  '()



  )







  





