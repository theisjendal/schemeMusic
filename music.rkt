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
  (cdr (assoc key element)))

(define (get-value-prod key procedure)
  (get-value key (uncurry procedure)))

(define base-notes
  '((C . 24) (C# . 25) (D . 26) (D# . 27) (E . 28) (F . 29) (F# . 30) (G . 31) (G# . 32) (A . 33) (A# . 34) (B . 35)))

(define channels
  '((piano . 1) (organ . 2) (guitar . 3) (violin . 4) (flute . 5) (trumpet . 6) (helicopter . 7) (telephone . 8)))

(define (note? element)
    (eq? (get-value 'type element) 'note))

(define (pause? element)
  (eq? (assq 'type element) 'pause))

(define (sequence? element)
  (eq? (get-value 'type element) 'sequence))

(define (parallel? element)
  (eq? (get-value 'type element) 'parallel))


;; ------------------------------
;; Element definitions
;; ------------------------------

(define (music-element element instrument  starttime)
  (lambda () (element instrument starttime)))

(define (pause length)
  (lambda (instrument starttime)
    (list (cons 'starttime starttime)
          (cons 'type 'pause)
          (cons 'length length))))

(define (note tone length octave)
  (lambda (instrument starttime)
    (list (cons 'starttime starttime)
          (cons 'type 'note)
          (cons 'tone tone)
          (cons 'length length)
          (cons 'octave octave)
          (cons 'instrument instrument))))

(define (sequence . elements)
  (if (null? elements)
      (error "Sequence must contain at least one element")
      (lambda (instrument starttime)
        (letrec ((calculate-subtree (lambda (startt total-length processed head tail)
                                      (let* ((e (music-element head instrument startt)) ; set starttime of head element as music-element
                                             (e-len (get-value-prod 'length e))   ; get length of element
                                             (tl (+ e-len total-length))     ; calculate total length of sequence so far
                                             (proc (cons e processed)))      ; add element to processed
                                        
                                        (if (null? tail)
                                            (list (cons 'elements (reverse proc))
                                                  (cons 'length tl))        ; Return alist with elements and length
                                            (calculate-subtree (+ startt e-len) tl proc (car tail) (cdr tail)))))))  ; Calculate next element
          
                 (append-alists (calculate-subtree starttime 0 '() (car elements) (cdr elements))
                                (list (cons 'type 'sequence) (cons 'starttime starttime)))))))


(define (parallel . elements)
  (if (or (null? elements) (null? (cdr elements)))
      (error "Parallel must contain at least two elements")
      (lambda (instrument starttime)
        (letrec ((calculate-subtree (lambda (max-len processed head tail)
                                     (let* ((e (music-element head instrument starttime)) ; set starttime of head element as music-element
                                            (e-len (get-value-prod 'length e))   ; get length of element
                                            (len (if (> e-len max-len) e-len max-len))
                                            (proc (cons e processed)))      ; add element to processed
                                       
                                       (cond ((null? tail) (list (cons 'elements (reverse proc))
                                                                 (cons 'length len)))
                                             (else (calculate-subtree len proc (car tail) (cdr tail))))))))
          
          (append-alists (calculate-subtree 0 '() (car elements) (cdr elements))
                         (list (cons 'type 'parallel)
                               (cons 'starttime starttime)))))))

;; ------------------------------
;; Element manipulation functions
;; ------------------------------



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
                           (let ((parsed (get-as-midi (uncurry head))))
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
                               (cons-helper (get-as-midi (uncurry (car rest))) (parse-helper (cdr rest)))))))
    (parse-helper elements)))

(define (parse-note element)
  (cons 'note-abs-time-with-duration (list (calc-time-unit (get-value 'starttime element))
                                           (get-value (get-value 'instrument element) channels)
                                           (calc-pitch (get-value 'tone element) (get-value 'octave element))
                                           80
                                           (calc-time-unit (get-value 'length element)))))

;; ------------------------------
;; Transform functions
;; ------------------------------
(define (re-instrument instrument element)
  (cond ((or (sequence? element)
             (parallel? element))
           
         ; Update instrument value
         (update-element 'instrument
                         instrument
                         ; Update elements key
                         (update-element 'elements
                                         ; Update all sub elements
                                         (update-elements instrument (get-value 'elements element) re-instrument)
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
                                (tone (get-tone-from-value val base-notes)))
                           (update-element 'tone
                                           tone
                                           (update-element 'octave
                                                           octave
                                                           element))))
        ((or (sequence? element)
             (parallel? element))
         (update-element 'elements
                         (update-elements value (get-value 'elements element) transpose)
                         element))
        ((pause? element) '())
        (else (error "Invalid element, was:" element))))

        
(define (update-elements  attribute-value elements function)
  (if (null? elements)
      '()
      (cons-helper (lambda () (function attribute-value (uncurry (car elements))))
                   (update-elements attribute-value (cdr elements) function))))

(define (update-element attribute attribute-value element)
  (cons (cons attribute attribute-value) element))
           
;; ------------------------------
;; Utility functions
;; ------------------------------
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

(define (flatten midi)
  (cond ((null? midi) '())
        ((eq? (car midi) 'note-abs-time-with-duration) (list midi))
        (else (append (flatten (car midi)) (flatten (cdr midi))))))

(define (calc-time-unit length)
  (let ((sec-per-beat (* 60 (/ 4 bpm))))
    (* (* length sec-per-beat) units-per-sec)))

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