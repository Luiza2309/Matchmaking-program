#lang racket

(require "etapa2.rkt")
(require "etapa3.rkt")

(require racket/trace)
(provide (all-defined-out))

;; Preferințele bărbaților și femeilor din problemă se pot schimba
;; în timp, dar de obicei ele nu se schimbă radical de la un moment
;; la altul. De aceea, în loc să rulăm de la zero algoritmul
;; Gale-Shapley de fiecare dată când se schimbă ceva, preferăm să
;; pornim de la lista de logodne stabile obținută în pasul anterior
;; și să o actualizăm, conform algoritmului următor:
;; - eliminăm din engagements cuplurile care au devenit instabile
;;   în urma modificărilor de preferințe
;;   - cuplurile rămase sunt stabile între ele și considerăm că
;;     se găsesc împreună într-o cameră, în timp ce membrii cuplurilor
;;     destrămate stau la coadă la intrarea în cameră
;; - cât timp coada nu este goală
;;   - prima persoană p din coadă intră în cameră și încearcă să se
;;     cupleze cu cineva care este deja acolo, astfel:
;;     - p-list = lista de preferințe a lui p
;;     - determină prima persoană p' din p-list care este în cameră
;;     - dacă p' nu e logodită, logodește p' cu p
;;     - dacă p' e logodită
;;       - dacă p' îl preferă pe p partenerului actual p''
;;         - logodește p' cu p
;;         - încearcă să îl cuplezi pe p'' cu altcineva din cameră
;;           (folosind același algoritm)
;;       - altfel, treci la următoarea persoană din p-list (dacă
;;         aceasta există, altfel p rămâne temporar fără partener)


; TODO 1
; Implementați funcția match care primește o persoană person care
; intră în cameră, lista engagements a cuplurilor din cameră
; (cuplurile având pe prima poziție persoanele de gen opus lui 
; person), o listă pref1 care conține preferințele celor de același 
; gen cu person, o listă pref2 cu preferințele celor de gen diferit, 
; respectiv o coadă queue a persoanelor din afara camerei,
; și întoarce lista de cupluri actualizată astfel încât noile
; cupluri să fie stabile între ele.
; Această listă se obține ca rezultat al încercării de a cupla pe
; person cu cineva din cameră (person va încerca în ordine persoanele 
; din lista sa de preferințe), care poate duce la destrămarea
; unui cuplu și necesitatea de a cupla noua persoană rămasă singură
; cu altcineva din cameră, etc. Procesul continuă până când:
; - ori avem numai cupluri stabile între ele în cameră, nimeni
;   nefiind singur
; - ori toate persoanele rămase singure nu ar fi preferate de nimeni
;   altcineva din cameră, și în acest caz convenim să "logodim"
;   aceste persoane cu valoarea #f, astfel încât funcția să
;   întoarcă în aceeași listă atât informația despre cine din
;   cameră este logodit, cât și despre cine este singur
(define (match person engagements pref1 pref2 queue)
  (let combina ((preferinte-persoana (get-pref-list pref1 person)))
    (if (null? preferinte-persoana)
        (append (list (cons #f person)) engagements)
        (let* ((persoana-dorita (car preferinte-persoana)) (logodnic (get-partner engagements persoana-dorita)))
          (cond
            ((null? engagements) (list (cons #f person)))
            ((member persoana-dorita queue) (combina (cdr preferinte-persoana)))
            ((false? logodnic) (update-engagements engagements persoana-dorita person))
            ((preferable? (get-pref-list pref2 persoana-dorita) person logodnic)
             (match logodnic (update-engagements engagements persoana-dorita person) pref1 pref2 queue))
            (else (combina (cdr preferinte-persoana)))
            )))))

; vad daca am terminat preferintele persoanei si o cuplez cu #f
; iau prima persoana din preferinte si vad daca e in camera
; daca nu e, trec la urmatoarea persoana
; daca e, vad daca il prefera mai mult pe logodnic decat pe cel singur
; daca nu, ii despart si apelez algoritmul pentru fostul logodnic

;(trace match)
; TODO 2
; Implementați funcția path-to-stability care primește lista
; engagements a cuplurilor din cameră, o listă de preferințe 
; masculine mpref, o listă de preferințe feminine wpref, respectiv
; coada queue a persoanelor din afara camerei, și întoarce lista
; completă de logodne stabile, obținută după ce fiecare persoană
; din queue este introdusă pe rând în cameră și supusă procesului
; descris de funcția match.
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
; - persoanele nelogodite din cameră apar în engagements sub forma
;   (#f . nume-bărbat) sau (nume-femeie . #f)
(define (path-to-stability engagements mpref wpref queue)
  (cond
    ((null? engagements) (path-to-stability (append (list (cons #f (car queue))) engagements) mpref wpref (cdr queue)))
    ((null? queue)
          (if (or (member (car (car engagements)) (get-men mpref)) (member (cdr (car engagements)) (get-women wpref)))
              (map (lambda (x) (cons (cdr x) (car x))) engagements)
              engagements))
    (else (if (member (car queue) (get-men mpref)) ; e barbat
              (if (or (member (car (car engagements)) (get-men mpref))
                      (member (cdr (car engagements)) (get-women wpref))) ; primul din engagements e barbat => rasuceste engagements
                  (path-to-stability (match (car queue) (map (lambda (x) (cons (cdr x) (car x))) engagements)
                                       mpref wpref (cdr queue)) mpref wpref (cdr queue))
                  (path-to-stability (match (car queue) engagements mpref wpref (cdr queue)) mpref wpref (cdr queue)))
              (if (or (member (car (car engagements)) (get-men mpref))
                      (member (cdr (car engagements)) (get-women wpref))) ; primul din engagements e barbat => engagements ramane la fel
                  (path-to-stability (match (car queue) engagements wpref mpref (cdr queue)) mpref wpref (cdr queue))
                  (path-to-stability (match (car queue) (map (lambda (x) (cons (cdr x) (car x))) engagements) wpref mpref (cdr queue))
                                     mpref wpref (cdr queue)))))))

; am apelat pentru fiecare persoana din queue functia de mai sus
; fac verificari ca sa stiu ce sex are persoana curenta
; ca sa stiu daca rasucesc engagements sau nu
; la final vreau sa fie femeile pe prima pozitie si
; rasucesc corespunzator

;(trace path-to-stability)
; TODO 3
; Implementați funcția update-stable-match care primește o listă 
; completă de logodne engagements (soluția anterioară), o listă de 
; preferințe masculine mpref și o listă de preferințe feminine wpref 
; (adică preferințele modificate față de cele pe baza cărora s-a 
; obținut soluția engagements), și calculează o nouă listă de logodne 
; stabile - conform cu noile preferințe, astfel:
; - unstable = cuplurile instabile din engagements
; - room-engagements = engagements - unstable
; - queue = persoanele din unstable
; - aplică algoritmul path-to-stability
; Precizări (aspecte care se garantează, nu trebuie verificate):
; - fiecare cuplu din lista engagements are pe prima poziție
;   o femeie
(define (update-stable-match engagements mpref wpref)
  (define unstable (get-unstable-couples engagements mpref wpref))
  (define room-engagements (filter (lambda (x) (not (member x unstable))) engagements))
  (path-to-stability room-engagements mpref wpref (get-couple-members unstable)))

; iau persoanele instabile si le pun intr-o lista
; le pun inapoi dupa noile preferinte

; TODO 4
; Implementați funcția build-stable-matches-stream care primește
; un flux pref-stream de instanțe SMP și întoarce fluxul de 
; soluții SMP corespunzător acestor instanțe.
; O instanță SMP este o pereche cu punct între o listă de preferințe
; masculine și o listă de preferințe feminine.
; Fluxul rezultat se va obține în felul următor:
; - primul element se calculează prin aplicarea algoritmului
;   Gale-Shapley asupra primei instanțe
; - următoarele elemente se obțin prin actualizarea soluției
;   anterioare conform algoritmului implementat în etapa 4 a temei
; Trebuie să lucrați cu interfața pentru fluxuri. Dacă rezolvați
; problema folosind liste și doar convertiți în/din fluxuri,
; punctajul pe acest exercițiu se anulează în totalitate.
(define (update-stream pref-stream my_stream)
  (cond
    ((not (stream-empty? pref-stream))
      (stream-cons
       (update-stable-match (stream-first my_stream) (stream-first (stream-first pref-stream)) (stream-rest (stream-first pref-stream)))
       (update-stream (stream-rest pref-stream) my_stream)))
    (else empty-stream)))

(define (build-stable-matches-stream pref-stream)
  (cond
    ((stream-empty? pref-stream) empty-stream)
    (else (stream-cons (gale-shapley (stream-first (stream-first pref-stream)) (stream-rest (stream-first pref-stream)))
                   (update-stream (stream-rest pref-stream) (build-stable-matches-stream pref-stream))))))

; build-stable-match imi creeaza primul element din stream cu gale-shapley
; functia auxiliara imi adauga in stream bazandu-se pe elementul anterior
; din stream
