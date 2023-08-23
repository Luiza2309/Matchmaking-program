#lang racket

(require "etapa2.rkt")
(require racket/trace)

(provide (all-defined-out))

; TODO 1
; După modelul funcției stable-match?, implementați funcția
; get-unstable-couples care primește o listă de logodne
; engagements, o listă de preferințe masculine mpref și o 
; listă de preferințe feminine wpref, și întoarce lista
; tuturor cuplurilor instabile din engagements.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; Nu este permisă recursivitatea pe stivă.
; Nu sunt permise alte funcții ajutătoare decât
; better-match-exists? și funcțiile de manipulare a listelor de
; preferințe definite în etapele anterioare.
; Nu este permisă apelarea multiplă a aceleiași funcții pe
; aceleași argumente.
; Folosiți una sau mai multe dintre expresiile let, let*, letrec,
; named let pentru a vă putea conforma acestor restricții.
(define (get-unstable-couples engagements mpref wpref)
  (let calcul ((L null) (eng engagements))
    (if (null? eng)
        L
        (let* ((p1 (car eng)) (p2 (cdr eng)) (p11 (car p1)) (p22 (cdr p1)))
          (cond
            ((or (better-match-exists? p11 p22 (get-pref-list wpref p11) mpref (map (lambda (x) (cons (cdr x) (car x))) engagements))
                 (better-match-exists? p22 p11 (get-pref-list mpref p22) wpref engagements))
             (calcul (cons p1 L) p2))
            (else (calcul L p2)))))))

; parcurg logodnele si vad daca exista o pereche mai buna pentru
; unul din parteneri si daca da, ii adaug in lista


; TODO 2
; Implementați funcția engage care primește o listă free-men
; de bărbați încă nelogodiți, o listă de logodne parțiale 
; engagements (unde fiecare cuplu are pe prima poziție o femeie),
; o listă de preferințe masculine mpref și o listă de preferințe 
; feminine wpref, și întoarce o listă completă de logodne stabile,
; obținută conform algoritmului Gale-Shapley:
; - cât timp există un bărbat m încă nelogodit
;   - w = prima femeie din preferințele lui m pe care m nu a cerut-o încă
;   - dacă w este nelogodită, adaugă perechea (w, m) la engagements
;   - dacă w este logodită cu m'
;     - dacă w îl preferă pe m lui m'
;       - m' devine liber
;       - actualizează partenerul lui w la m în engagements
;     - altfel, repetă procesul cu următoarea femeie din lista lui m
; Folosiți named let pentru orice proces recursiv ajutător (deci nu
; veți defini funcții ajutătoare recursive).
; Folosiți let și/sau let* pentru a evita calcule duplicate.
(define (engage free-men engagements mpref wpref)
  (let parcurge-burlaci ((burlaci free-men) (eng engagements))
    (if (null? burlaci)
        eng
        (let* ((primul_burlac (car burlaci)) (get_pref_burlac (get-pref-list mpref primul_burlac)))
          (let parcurge-femei ((pref-burlac get_pref_burlac) (femeie (get-partner eng (car get_pref_burlac))))
            (let* ((restul_burlacilor (cdr burlaci)) (prima_femeie (car pref-burlac)) (restul_femeilor (cdr pref-burlac)))
              (cond
                ((null? pref-burlac) (parcurge-burlaci restul_burlacilor eng))
                ((not femeie) (parcurge-burlaci restul_burlacilor (append (list (cons prima_femeie primul_burlac)) eng)))
                ((not (preferable? (get-pref-list wpref prima_femeie) primul_burlac femeie)) (parcurge-femei restul_femeilor (get-partner eng (car restul_femeilor))))
                (else (parcurge-burlaci (cons femeie restul_burlacilor) (update-engagements eng prima_femeie primul_burlac)))
                )))))))

; parcurg burlacii si parcurg lista fiecaruia de preferinte
; si daca gasesc vreo fata din lista lui de preferinte care
; este singura, ii logodesc
; daca o el vrea o fata si fata il prefera mai mult pe el
; decat pe logodnic, ii adaug si fostul fetei il adaug
; la burlaci
; daca am parcurs toate fetele din preferintele lui
; si nimeni nu-l vrea, trec la urmatorul burlac


; TODO 3
; Implementați funcția gale-shapley care este un wrapper pentru
; algoritmul implementat de funcția engage. Funcția gale-shapley
; primește doar o listă de preferințe masculine mpref și o listă
; de preferințe feminine wpref și calculează o listă completă de
; logodne stabile conform acestor preferințe.
(define (gale-shapley mpref wpref)
  (engage (get-men  mpref) null mpref wpref))

; apelez functia cu toti barbatii ca fiind burlaci


; TODO 4
; Implementați funcția get-couple-members care primește o listă
; de perechi cu punct și întoarce o listă simplă cu toate elementele 
; care apar în perechi.
; Folosiți funcționale, fără recursivitate explicită.
(define (get-couple-members pair-list)
  (foldl (lambda (x lili) (append (list (car x)) (append (list (cdr x)) lili))) null pair-list))

; iau fiecare element dintr-o pereche si il adaug in lista

