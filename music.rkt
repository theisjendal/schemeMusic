#lang racket
;; ------------------------------
;; DISCLAIMER; The following method are made by Kurt Nørmark, Aalborg University
;; ------------------------------

; Assumes lists, and makes associate list.
(define (pair-up key-list value-list)
  (if (or (null? key-list) (null? value-list))
      '()
      (cons
       (cons (car key-list) (car value-list))
       (pair-up (cdr key-list) (cdr value-list)))))

;; ------------------------------
;; Base methods
;; ------------------------------
(define units-per-sec 960)
(define bpm 60)

(define (get-value key element)
  (cdr (assq key element)))

(define (get-value-prod key procedure)
  (get-value key (uncurry procedure)))

(define base-notes
  '((C . 24) (C# . 25) (D . 26) (D# . 27) (E . 28) (F . 29) (F# . 30) (G . 31) (G# . 32) (A . 33) (A# . 34) (B . 35)))

(define channels
  '((piano . 1) (organ . 2) (guitar . 3) (violin . 4) (flute . 5) (trumpet . 6) (helicopter . 7) (telephone . 8)))

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

;(define (music-element element instrument)
;  (lambda () (element instrument)))

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
          
                 (append-alists (calculate-subtree starttime 0 '() (car elements) (cdr elements))
                                (list (cons 'type 'sequence) (cons 'starttime starttime)))))))


(define (parallel . elements)
  (if (and (not (list? elements))
           (or (null? elements) (null? (cdr elements))))
      (error "Parallel must contain at least two elements")
      (lambda (instrument starttime)
        (letrec ((calculate-subtree (lambda (max-len processed head tail)
                                     (let* ((e (head instrument starttime)) ; set starttime of head element as music-element
                                            (e-len (get-value 'duration e))   ; get duration of element
                                            (len (if (> e-len max-len) e-len max-len))
                                            (proc (cons e processed)))      ; add element to processed
                                       
                                       (cond ((null? tail) (list (cons 'elements (reverse proc))
                                                                 (cons 'duration len)))
                                             (else (calculate-subtree len proc (car tail) (cdr tail))))))))
          
          (append-alists (calculate-subtree 0 '() (car elements) (cdr elements))
                         (list (cons 'type 'parallel)
                               (cons 'starttime starttime)))))))

;; ------------------------------
;; MIDI functions
;; ------------------------------
(define (get-as-midi element)
  (let ((type (get-value 'type element)))
    (cond ((eq? type 'sequence) (parse-sequence element))
          ((eq? type 'parallel) (parse-parallel element))
          ((eq? type 'note) (parse-note element))
          ((eq? type 'pause) '())))) ; skips pauses - uses cons-helper

                
(define (parse-sequence element)
  (letrec ((elements (get-value 'elements element))
           (parse-helper (lambda (head tail)
                           (let ((parsed (get-as-midi head)))
                             (if (null? tail)
                                 (cons-helper parsed '())
                                 (cons-helper parsed
                                       (parse-helper (car tail) (cdr tail))))))))
    (parse-helper (car elements) (cdr elements))))
                               
(define (parse-parallel element)
  (letrec ((elements (get-value 'elements element))
           (parse-helper (lambda (rest)
                           (if (null? rest)
                               '()
                               (cons-helper (get-as-midi (car rest)) (parse-helper (cdr rest)))))))
    (parse-helper elements)))

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
         (update-element-multi (list 'instrument 'elements)
                               (list instrument (update-elements instrument (get-value 'elements element) re-instrument))
                               element)) ; Element to update (for both)
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
                                (tone (get-tone-from-value val base-notes)))
                           (update-element-multi (list 'tone 'octave) (list tone octave) element)))
        ((or (sequence? element)
             (parallel? element))
         (update-element 'elements
                         (update-elements value (get-value 'elements element) transpose)
                         element))
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
  (letrec ((scale-parallel-helper (lambda (elements)
                                 (if (null? elements)
                                     '()
                                     (cons (scale-helper scaler starttime (car elements))
                                           (scale-parallel-helper (cdr elements)))))))
    ; Update multiple attributes of element
    (update-element-multi (list 'duration 'starttime 'elements)
                          (list (* (get-value 'duration element) scaler)
                                starttime
                                (scale-parallel-helper (get-value 'elements element)))
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
              
        
(define (update-elements attribute-value elements function)
  (if (null? elements)
      '()
      (cons-helper (function attribute-value (car elements))
                   (update-elements attribute-value (cdr elements) function))))

(define (update-element attribute attribute-value element)
  (cons (cons attribute attribute-value) element))
           
;; ------------------------------
;; Utility functions
;; ------------------------------

(define (get-poly-degree element)
  (letrec ((get-timespand (lamda (e)
                                 (if (or (parallel? e)
                                         (sequence? e))
                                     (

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

; flattens a midi song
(define (flatten midi)
  (cond ((null? midi) '())
        ((eq? (car midi) 'note-abs-time-with-duration) (list midi))
        (else (append (flatten (car midi)) (flatten (cdr midi))))))

(define (calc-time-unit duration)
  (let ((sec-per-beat (* 60 (/ 4 bpm))))
    (* (* duration sec-per-beat) units-per-sec)))

(define (calc-pitch tone octave)
  (let ((pitch (+ (get-value tone base-notes) (* 12 octave))))
    (if (and (<= pitch 127)
            (>= pitch 0))
        pitch
        (error "Must be between note C -2 and G 8, was:" tone octave))))
    
(define (append-alists alist1 alist2)
  (cond ((null? alist2) '())
        ((null? alist1) (cons (car alist2)
                              (append-alists alist1 (cdr alist2))))
        (else (cons (car alist1)
                    (append-alists (cdr alist1) alist2)))))



;; ------------------------------
;; test melodies
;; ------------------------------

(define testf (sequence (parallel (note 'C 1/8 2) (note 'C# 1/4 3)) (note 'D 1/4 2)))
(define test (testf 'guitar 0))
(define parse-test ((sequence (note 'C 1/8 2) (pause 1/8) (note 'C# 1/4 3)) 'guitar 0))
(define test-note ((note 'C 1/2 2) 'guitar 0))

; Mester Jakob
; C D E C  C D E C  E F G  E F G  G A G F E C
(define mester-jakob ((sequence (note 'G 1/4 2) (note 'A 1/4 2) (note 'B 1/4 2) (note 'G 1/4 2)
                                (note 'G 1/4 1) (note 'A 1/4 1) (note 'B 1/4 1) (note 'G 1/4 1)
                                (note 'B 1/4 2) (note 'C 1/4 3) (note 'D 1/2 3)
                                (note 'B 1/4 1) (note 'C 1/4 2) (note 'D 1/2 2)
                                (note 'D 1/8 3) (note 'E 1/8 3) (note 'D 1/8 3) (note 'C 1/8 3) (note 'B 1/4 2) (note 'G 1/4 2)
                                (note 'D 1/8 2) (note 'E 1/8 2) (note 'D 1/8 2) (note 'C 1/8 2) (note 'B 1/4 1) (note 'G 1/4 1)
                                (note 'G 1/4 2) (note 'D 1/4 2) (note 'G 1/2 2)
                                (parallel (sequence (note 'G 1/4 2) (note 'D 1/4 2) (note 'G 1/2 2))
                                          (sequence (note 'G 1/4 1) (note 'D 1/4 1) (note 'G 1/2 1)))) 'piano 0))