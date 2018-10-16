#lang racket
;; ------------------------------
;; DISCLAIMER; The following method are made by Kurt Nørmark, Aalborg University
;; ------------------------------
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

;; ------------------------------
;; Base methods
;; ------------------------------

; Define standard units
(define units-per-sec 960)
(define bpm 60)

; Makes it possible to get attributes from an element (fx duration)
(define (get-value key element)
  (cdr (assq key element)))

; Define 
(define base-tones
  '((C . 24) (C# . 25) (D . 26) (D# . 27) (E . 28) (F . 29) (F# . 30) (G . 31) (G# . 32) (A . 33) (A# . 34) (B . 35)))

(define channels
  '((piano . 1) (organ . 2) (guitar . 3) (violin . 4) (flute . 5) (trumpet . 6) (helicopter . 7) (telephone . 8)))

; Following procedures check if the element contain certain keys and a specific value under type key.
(define (note? element)
    (and (music-element? element)
         (and (assq 'tone element)
              (and (assq 'octave element)
                   (and (assq'instrument element)
                        (eq? (get-value 'type element) 'note))))))

(define (pause? element)
  (and (music-element? element)
       (eq? (get-value 'type element) 'pause)))

(define (sequence? element)
  (and (music-element? element)
       (and (assq 'elements element)
            (eq? (get-value 'type element) 'sequence))))

(define (parallel? element)
  (and (music-element? element)
       (and (assq 'elements element)
            (eq? (get-value 'type element) 'parallel))))

; Is a music element if it contains duration, starttime, and type
(define (music-element? element)
  (and (assq 'duration element)
       (and (assq 'starttime element)
            (assq 'type element))))


;; ------------------------------
;; Element definitions
;; ------------------------------
; Each element return an alist, with a different number of keys.
(define (pause duration)
  (lambda (instrument starttime)
    (list (cons 'starttime starttime)
          (cons 'type 'pause)
          (cons 'duration duration))))

(define (note tone duration octave)
  (lambda (instrument starttime)
    (list (cons 'starttime starttime)
          (cons 'type 'note)
          (cons 'tone tone)
          (cons 'duration duration)
          (cons 'octave octave)
          (cons 'instrument instrument))))

; The sequence also adds all elements to its list of elements. It therefore calculates the starttimes (absolute time)
; and the length of it self during this.
(define (sequence . elements)
  (if (null? elements)
      (error "Sequence must contain at least one element")
      (lambda (instrument starttime)
        (letrec ((calculate-subtree (lambda (startt total-duration processed head tail)
                                      (let* ((e (head instrument startt)) ; set starttime of head element as music-element
                                             (e-len (get-value 'duration e))   ; get duration of element
                                             (td (+ e-len total-duration))     ; calculate total duration of sequence so far
                                             (proc (cons e processed)))      ; add element to processed
                                        
                                        (if (null? tail)
                                            (list (cons 'elements (reverse proc))
                                                  (cons 'duration td))        ; Return alist with elements and duration
                                            (calculate-subtree (+ startt e-len) td proc (car tail) (cdr tail)))))))  ; Calculate next element
          
                 (append (calculate-subtree starttime 0 '() (car elements) (cdr elements))
                                (list (cons 'type 'sequence) (cons 'starttime starttime)))))))

; Parellel sets each subelement's starttime to the same starttime.
(define (parallel . elements)
  (if (or (null? elements)
          (null? (cdr elements)))
      (error "Parallel must contain at least two elements")
      (lambda (instrument starttime)
        (let* ((calc-elements (map (lambda (x) (x instrument starttime)) elements))
               (max-d (get-value 'duration (apply max-duration calc-elements))))
          
          
          (list (cons 'elements calc-elements)
                (cons 'duration max-d)
                (cons 'type 'parallel)
                (cons 'starttime starttime))))))


;; ------------------------------
;; MIDI functions
;; ------------------------------
; Calls correct subfunction form by checking type
(define (get-as-midi element)
  (let ((type (get-value 'type element)))
    (cond ((sequence? element) (parse-sequence element))
          ((parallel? element) (parse-parallel element))
          ((note? element) (parse-note element))
          ((pause? element) '())))) ; skips pauses - uses cons-helper

; 
(define (parse-sequence element)
  (let ((elements (get-value 'elements element)))
    (map get-as-midi elements)))
                               
(define (parse-parallel element)
  (letrec ((elements (get-value 'elements element))
           (parse-helper (lambda (rest)
                           (if (null? rest)
                               '()
                               (cons-helper (get-as-midi (car rest)) (parse-helper (cdr rest)))))))
    (map get-as-midi elements)));(parse-helper elements)))

(define (parse-note element)
  (cons 'note-abs-time-with-duration (list (calc-time-unit (get-value 'starttime element))
                                           (get-value (get-value 'instrument element) channels)
                                           (calc-pitch (get-value 'tone element) (get-value 'octave element))
                                           80
                                           (calc-time-unit (get-value 'duration element)))))

;; ------------------------------
;; Transform functions
;; ------------------------------
(define (re-instrument instrument element)
  (cond ((or (sequence? element)
             (parallel? element))
         (let ((re-i ((curry2 re-instrument) instrument)))
           (update-element-multi (list 'instrument 'elements)
                                 (list instrument (map re-i (get-value 'elements element)))
                                 element))) ; Element to update (for both)
        ; Update if note
        ((note? element) (update-element 'instrument
                                          instrument
                                          element))
        ;Do nothing if pause
        ((pause? element) element)
        (else (error "Invalid element, was:" element))))


(define (transpose value element)
  (cond ((note? element) (let* ((pitch (+ (calc-pitch (get-value 'tone element) (get-value 'octave element)) value))
                                (octave (- (quotient pitch 12) 2)) 
                                (val (+ (modulo pitch 12) 24))
                                (tone (get-tone-from-value val base-tones)))
                           (update-element-multi (list 'tone 'octave) (list tone octave) element)))
        ((or (sequence? element)
             (parallel? element))
         (let ((trans ((curry2 transpose) value)))
           (update-element 'elements
                           (map transpose (get-value 'elements element))
                           element)))
        ((pause? element) '())
        (else (error "Invalid element, was:" element))))

(define (scale scaler element)
  (scale-helper scaler (get-value 'starttime element) element))


(define (scale-helper scaler starttime element)
  (cond ((or (note? element)
             (pause? element)) (update-element-multi (list 'duration 'starttime)
                                                     (list (* (get-value 'duration element) scaler) starttime)
                                                     element))
        ((sequence? element) (scale-sequence scaler starttime element))
        ((parallel? element) (scale-parallel scaler starttime element))
        (else (error "not implemented yet"))))

                                                                            
(define (scale-sequence scaler starttime element)
  (letrec ((scale-sequence-helper (lambda (elements starttime)
                                    (if (null? elements)
                                        '()
                                        ; Else we update the next element, calculate next starttime and move on
                                        (let* ((e (scale-helper scaler starttime (car elements)))
                                               (st (+ starttime (get-value 'duration e))))
                                          (cons e (scale-sequence-helper (cdr elements) st)))))))
    ; Update multiple attributes of element
    (update-element-multi (list 'duration 'starttime 'elements)
                          (list (* (get-value 'duration element) scaler)
                                starttime
                                (scale-sequence-helper (get-value 'elements element)
                                                       starttime))
                          element)))

(define (scale-parallel scaler starttime element)
  (let* ((scal ((curry2 scale-helper) scaler)))
    ; Update multiple attributes of element
    (update-element-multi (list 'duration 'starttime 'elements)
                          (list (* (get-value 'duration element) scaler)
                                starttime
                                (map scal (get-value 'elements element)))
                          element)))

(define (update-element-multi keys values element)
  (cond ((or (null? keys)
             (null? values))
         (if (and (null? keys)
                  (null? values))
             element
             (error "Must equally many keys and values, only:" keys values)))
        (else (let ((key (car keys))
                    (value (car values)))
                (update-element-multi (cdr keys)
                                      (cdr values)
                                      (update-element key value element))))))

(define (update-element attribute attribute-value element)
  (cons (cons attribute attribute-value) element))
           
;; ------------------------------
;; Utility functions
;; ------------------------------

(define (get-duration element)
  (let ((e (if (procedure? element) (element 't 0) element)))
    (if (music-element? e)
        (get-value 'duration e)
        (error "Must be a music element to get duration"))))
              

(define (max-duration x y)
  (let ((dx (get-value 'duration x))
        (dy (get-value 'duration y)))
    (if (> dx dy) x y)))

(define (get-poly-degree element)
  (letrec ((get-timespan (lambda (e)
                                (cond ((or (parallel? e)
                                           (sequence? e))
                                       (map get-timespan (get-value 'elements e)))
                                      ((note? e)
                                       (cons (get-value 'starttime e)
                                             (get-value 'duration e)))
                                      ((pause? e) '())
                                      (else "Error, unknown element:" e))))
           
           (calculated (sort (flatten-alist (get-timespan element))
                             (lambda (x y) (< (+ (car x) (cdr x))
                                              (+ (car y) (cdr y))))))
           
           (count (lambda (lst)
                    (if (null? lst)
                        0
                        (let ((this-c (counter (car lst) lst))
                              (rest-c (count (cdr lst))))
                          (if (>= this-c rest-c)
                              this-c
                              rest-c))))))
                          
    (+(count calculated))))


(define (counter e lst)
  (let ((st1 (car e))
        (et1 (cdr e)))
    (if (null? lst)
        0
        (let* ((e2 (car lst))
               (st2 (car e2))
               (et2 (cdr e2)))
          (if (and (>= st1 st2)
                   (<= et1 et2))
              (+ (counter e (cdr lst)) 1)
              (counter e (cdr lst)))))))


; Calls functions with no parameters, more descriptive than parenthesis.
(define (uncurry f)
  (f))

(define (get-tone-from-value value alist)
  (cond ((null? alist) (error "Did not match a base-value. Must be between 24 and 35, was:" value))
        ((eq? value (cdar alist)) (caar alist))
        ((get-tone-from-value value (cdr alist)))))

; Ignores element1 if it is an empty list.  
(define (cons-helper element1 element2)
  (if (null? element1)
      element2
      (cons element1 element2)))

(define (flatten-alist alist)
  (cond ((null? alist) '())
        ((list? alist) (append (flatten-alist (car alist)) (flatten-alist (cdr alist))))
        (else (list alist))))

; flattens a midi song, by flattening everything except if the lists element is not a list it self.
(define (flatten-midi midi)
  (cond ((null? midi) '())
        ((list? (car midi)) (append (flatten-midi (car midi)) (flatten-midi (cdr midi))))
        (else (list midi))))

; Assumes 4/4 to calculate the time in midi time units.
(define (calc-time-unit duration)
  (let ((sec-per-beat (* 60 (/ 4 bpm))))
    (* (* duration sec-per-beat) units-per-sec)))

; Calculates the pitch as an integer.
(define (calc-pitch tone octave)
  (let ((pitch (+ (get-value tone base-tones) (* 12 octave))))
    (if (and (<= pitch 127)
            (>= pitch 0))
        pitch
        (error "Must be between note C -2 and G 8, was:" tone octave))))

;; ------------------------------
;; test melodies
;; ------------------------------

(define testf (sequence (note 'D 1/4 2) (parallel (note 'C 1/8 2) (note 'C# 1/4 3)) (note 'D 1/4 2)))
(define test (testf 'guitar 0))
(define parse-test (sequence (note 'C 1/8 2) (pause 1/8) (note 'C# 1/4 3)
                              (parallel (sequence (note 'G 1/4 2) (note 'D 1/4 2))
                                        (sequence (note 'G 1/4 2) (note 'D 1/4 2)))))
(define some ((parallel (sequence (pause 1/8) (note 'C 1/8 2) (pause 1/8))
                        (sequence (note 'C 1/8 2) (pause 1/8) (note 'C 1/8 2))) 'guitar 0)) 
(define test-note ((note 'C 1/2 2) 'guitar 0))

; Mester Jakob
; C D E C  C D E C  E F G  E F G  G A G F E C
(define mester-jakob (sequence (note 'G 1/4 2) (note 'A 1/4 2) (note 'B 1/4 2) (note 'G 1/4 2)
                                (note 'G 1/4 1) (note 'A 1/4 1) (note 'B 1/4 1) (note 'G 1/4 1)
                                (note 'B 1/4 2) (note 'C 1/4 3) (note 'D 1/2 3)
                                (note 'B 1/4 1) (note 'C 1/4 2) (note 'D 1/2 2)
                                (note 'D 1/8 3) (note 'E 1/8 3) (note 'D 1/8 3) (note 'C 1/8 3) (note 'B 1/4 2) (note 'G 1/4 2)
                                (note 'D 1/8 2) (note 'E 1/8 2) (note 'D 1/8 2) (note 'C 1/8 2) (note 'B 1/4 1) (note 'G 1/4 1)
                                (note 'G 1/4 2) (note 'D 1/4 2) (note 'G 1/2 2)
                                (parallel (sequence (note 'G 1/4 2) (note 'D 1/4 2) (note 'G 1/2 2))
                                          (sequence (note 'G 1/4 1) (note 'D 1/4 1) (note 'G 1/2 1)))))

(define d-dur (sequence (note 'D 1/4 1) (note 'E 1/4 1) (note 'F# 1/4 1) (note 'G 1/4 1)
                        (note 'A 1/4 1) (note 'B 1/4 1) (note 'C# 1/4 2) (note 'D 1/4 2)))

(define in-canon ((parallel d-dur
                           (sequence (pause 1) d-dur)) 'piano 0))