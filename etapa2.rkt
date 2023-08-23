#lang racket

(require racket/trace)
(provide (all-defined-out))

; ATENȚIE - Veți avea de reimplementat mai multe funcții
; din etapa 1, însă cu un alt mod de rezolvare (folosind
; funcționale sau alte funcții solicitate în enunț).
; Enunțul acestor exerciții va debuta prin "Ca în etapa 1,
; dar este interzisă recursivitatea explicită".


; TODO 1
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; bărbaților și calculează lista bărbaților din problemă.
; Folosiți orice funcțională exceptând foldl/foldr.
(define (get-men mpref)
  (map (lambda (linie) (car linie)) mpref))
; map ia fiecare lista de preferinte din lista mare
; si cu lambda iau prima persoana din fiecare lista


; TODO 2
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește lista preferințelor
; femeilor și calculează lista femeilor din problemă.
; Folosiți foldl sau foldr, astfel încât să nu fie necesare
; operații de tip append sau reverse.
(define (get-women wpref)
  (foldr (lambda (L lili) (cons (car L) lili)) null wpref))
; foldr ca sa nu imi intoarca lista
; iau fiecare femeie (prima pozitie din fiecare lista) si o adaug
; la o lista auxiliara (lili)


; TODO 3
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de liste
; de preferințe (ori lista preferințelor bărbaților, ori a
; femeilor) și o persoană ale cărei preferințe apar în listă,
; și întoarce lista preferințelor acelei persoane.
; Se garantează că persoana apare în listă.
; Exemplu: dacă în lista wpref apare linia (ana bobo adi cos),
; (get-pref-list wpref 'ana) => '(bobo adi cos)
; Folosiți minim o funcțională și minim o funcție anonimă.
(define (get-pref-list pref person)
  (foldl (lambda (L lili)
           (if (equal? (car L) person)
             (append (cdr L) lili)
             lili
           )) null pref))
; foldl ia fiecare lista din lista mare
; lambda verifica capul de lista si daca e persoana cautata
; adaug restul listei la lili


; TODO 4
; Ca în etapa 1, dar este interzisă recursivitatea explicită
; și sunt permiși operatorii condiționali:
; Implementați o funcție care primește o listă de tipul
; întors la exercițiul precedent (lista preferințelor unei persoane),
; respectiv două persoane x și y care apar în lista respectivă,
; și întoarce true dacă x este mai sus decât y în topul preferințelor
; și false în caz contrar.
; Folosiți funcția member.
(define (preferable? pref-list x y)
  (define xp (member x pref-list))
  (define yp (member y pref-list))
  (cond
    ((equal? xp #f) #t)
    ((equal? yp #f) #f)
    ((> (length xp) (length yp)) #t)
    (else #f)))
; member imi intoarce lista incepand de la elementul gasit
; daca x se afla inainte de y atunci lista lui e mai lunga


; TODO 5
; Implementați recursiv funcționala find-first, care primește
; un predicat și o listă și întoarce primul element al listei
; care satisface predicatul, sau false dacă un asemenea element
; nu există.
; Implementarea trebuie să fie eficientă în sensul că nu trebuie
; să continue explorarea listei odată ce s-a găsit elementul.
(define (find-first p L)
  (cond
    ((null? L) #f)
    ((p (car L)) (car L))
    (else (find-first p (cdr L)))))
; parcurg lista recursiv si daca elementul la care sunt respecta
; conditia atunci il intorc


; TODO 6
; Ca în etapa 1, dar este interzisă recursivitatea explicită:
; Implementați o funcție care primește o listă de logodne
; (în care fiecare logodnă este o pereche cu punct între parteneri)
; și o persoană, și, în cazul în care această persoană apare pe prima
; poziție într-o logodnă, este întors partenerul său, altfel se
; întoarce false.
; Folosiți find-first, fără să îl apelați de 2 ori (hint: define în define).
(define (get-partner engagements person)
  (define result (find-first (lambda (x) (equal? (car x) person)) engagements))
  (if (pair? result)
      (cdr result)
      #f))
; intoarce (cora . bobo) sau #f si vreau sa iar cdr doar in cazul in care nu intoarce #f
; si nu pot apela find-first de doua ori


; TODO 7
; Implementați recursiv funcționala change-first care primește
; un predicat p, o listă L și o valoare val, și întoarce o nouă 
; listă în care primul element din L care satisface predicatul p
; a fost înlocuit cu valoarea val, celelalte rămânând la fel.
; Dacă niciun element din L nu satisface predicatul, lista L
; rămâne neschimbată.
(define (change-first-helper p L val lili nr)
  (cond
    ((null? L) lili)
    ((and (p (car L)) (= nr 0)) (change-first-helper p (cdr L) val (append lili (list val)) (+ nr 1)))
    (else (change-first-helper p (cdr L) val (append lili (list (car L))) nr))
    ))

(define (change-first p L val)
  (change-first-helper p L val null 0))
; copiez toate elementele in lista auxiliara
; daca am gasit un element care respecta predicatul il schimb si
; incrementez nr ca sa stiu sa nu le mai schimb si pe celelalte


; TODO 8
; Implementați funcția update-engagements care primește o listă de
; logodne engagements și două persoane p1 și p2, și întoarce lista
; actualizată de logodne, în care vechiul partener al lui p1 este
; înlocuit cu p2.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - p1 era logodită în prealabil
; - fiecare cuplu din lista engagements are pe prima poziție
;   persoanele de același gen cu p1
; Folosiți change-first.
(define (update-engagements engagements p1 p2)
  (change-first (lambda (x) (equal? (car x) p1)) engagements (cons p1 p2)))
; caut persoana cu functia implementata mai devreme si schimb perechea din
; engagements cu noua pereche (p1 . p2)


; TODO
; Copiați implementarea funcției better-match-exists? din etapa 1.
; Funcția nu este repunctată de checker, dar este necesară pentru
; implementarea funcției stable-match? de mai jos.
; Dacă nu ați implementat better-match-exists? în etapa 1, solicitați 
; o rezolvare de la asistent, astfel încât să puteți continua.
(define (better-match-exists? p1 p2 p1-list pref2 engagements)
  (cond
    ((null? p1-list) #f)
    ((equal? (car p1-list) p2) #f)
    ((not (preferable? (get-pref-list pref2 (car p1-list)) p1 (get-partner engagements (car p1-list)))) (better-match-exists? p1 p2 (cdr p1-list) pref2 engagements))
    (else #t)))


; TODO 9
; Implementați funcția stable-match? care primește o listă 
; completă de logodne engagements, o listă de preferințe masculine 
; mpref și o listă de preferințe feminine wpref, și întoarce true 
; dacă toate cuplurile din engagements sunt stabile.
; Un cuplu este stabil dacă pentru niciunul din membrii cuplului
; nu există un alt partener mai potrivit (conform definiției de
; la funcția better-match-exists?).
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (stable-match?-helper engagements eng mpref wpref)
  (cond
    ((null? eng) #t)
    ((or (better-match-exists? (car (car eng)) (cdr (car eng)) (get-pref-list wpref (car (car eng))) mpref (map (lambda (x) (cons (cdr x) (car x))) engagements))
         (better-match-exists? (cdr (car eng)) (car (car eng)) (get-pref-list mpref (cdr (car eng))) wpref engagements)) #f)
    (else (stable-match?-helper engagements (cdr eng) mpref wpref))))

(define (stable-match? engagements mpref wpref)
  (stable-match?-helper engagements engagements mpref wpref))

;(trace stable-match?-helper)

; am o lista de engagements cu toate logodnele si o lista eng cu logodnele
; pe care o parcurg si o modific prin recursivitate
; daca am parcurs tot eng si n-am intors #f inseamna ca toti sunt compatibili
; daca p1 are un better-match sau p2 are un better-match intors #f si ma opresc
