#lang rosette/safe

; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
; packages
(require rosette/lib/destruct) ; Value destructuring library.
(require rosette/lib/synthax)  ; Synthesis library.

; General purpose register encoding.
; These aren't used?
(define RAX  0)  ; A eXtended
(define RBP  1)  ; Base Pointer
(define RBX  2)  ; B eXtended
(define RCX  3)  ; C eXtended
(define RDI  4)  ; Destination Index
(define RDX  5)  ; D eXtended
(define RSI  6)  ; Source Index
(define RSP  7)  ; Stack Pointer
(define R8   8)  ; R8
(define R9   9)  ; R9
(define R10 10)  ; R10
(define R11 11)  ; R11
(define R12 12)  ; R12
(define R13 13)  ; R13
(define R14 14)  ; R14
(define R15 15)  ; R15
(define PC  16)  ; R16

; struct for our contract language
(struct IF (pred expr) #:transparent)
(struct OPCODE ())
(struct INSTR ())
(struct SLIDE (i1 i2 bs) #:transparent)
(struct RS1 ())
(struct RS2 ())
(struct NOT (b))
(struct AND (b1 b2))
(struct OR (b1 b2))
(struct EQ (b1 b2))
(struct BOOL (b))
(struct BS (bs))
(struct REG (r) #:transparent)

; Grammar for the actual contract.
; (IF (BOOL #f) (REG 12))

; The expr object everywhere is just this cexpr.
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (REG (?? integer?))
              INSTR
              )]
  )

(define EMPTY (list '()))

; Debug note: e: c-expression (grammar), x: state (bv).
;   returns the observation.
(define (eval e x)
  (destruct e
            [(IF pred bs) (if (eval-pred pred x) (list (eval-bs bs x))
                              EMPTY)]))

; Debug note: p: pred (e.g., BOOL); x: state (bv).
(define (eval-pred p x)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p x))]
            [(AND p1 p2) (and (eval-pred p1 x) (eval-pred p2 x))]
            [(OR p1 p2) (or (eval-pred p1 x) (eval-pred p2 x))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 x) (eval-bs bs2 x))]))

; Debug note: bs: bitstring; x: state (bv).
(define (eval-bs bs x)
  (destruct bs
     [(BS b) b]
     [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b x))]
     [(REG reg) (eval-reg reg x)]
     [INSTR (eval-reg PC x)]
     ))

; Debug note: reg: integer val; x: state (bv).
(define (eval-reg reg x)
  ; Debug stuff.
  ; (print "reg-val: ")
  ; (println reg)
  ; (print "x: ")
  ; (println x)
  ; (println (list-ref x reg))
  (list-ref x reg))

; Auxiliary functions
; obs() takes an expression and a xstate 
;       returns its observation

; state contains all bitvectors for each run object.
(define (obs expr state)
  ; (println (eval expr state))
  ; (print "Getting observations for state:")
  ; (println state)
  (eval expr state))

; obs() takes an expression and a xstate 
;       returns true if there is some observations produced
;               false otherwise
(define (empty-obs expr state)
  (empty? (eval expr state)))

; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
(define (obs-equal expr state1 state2)
  (listbv-equal (obs expr state1)
                (obs expr state2)))

; listbv-equal() takes two observations
;                returns true if they are the same
;                        false otherwise
(define (listbv-equal bvs1 bvs2)
  (if (empty? bvs1)
      (if (empty? bvs2) #t
          #f)
      (if (empty? bvs2) #f
          (and (bveq (first bvs1) (first bvs2))
               (listbv-equal (rest bvs1) (rest bvs2))))))

; diff() takes the following arguments:
;              i,j,i_,j_: natural numbers such that i <= j and i_ <= j_
;              r, r_: two runs
;              expr: an expression
;        returns true if the trace produced by r[i]->r[j] and r_[i_]->r_[j_] are distinguishable 
;                false otherwise

; TODO: check the remaining diffs here - RPW.
(define (diff i j r i_ j_ r_ expr)
  (if (equal? i j)
      (if (equal? i_ j_) #f
                         (or (not (empty-obs expr (list-ref r_ i_)))
                             (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_) (or (not (empty-obs expr (list-ref r i)))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (list-ref r i))
                                  (diff (+ i 1) j r i_ j_ r_ expr))
                             (and (empty-obs expr (list-ref r_ i_))
                                  (diff i j r (+ i_ 1) j_ r_ expr))
                             (and (not (empty-obs expr (list-ref r i)))
                                  (not (empty-obs expr (list-ref r_ i_)))
                                  (not (obs-equal expr (list-ref r i) (list-ref r_ i_))))))))


; Debug testing function for playing with grammar directly.
(define (debug i j r i_ j_ r_ expr)
  (obs-equal expr (list-ref r i ) (list-ref r_ i_)))

; ------------- END-CORE ------------------ ;

; Run 1 archstates.
(define r0_0 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996224 (bitvector 64))))

(define r0_1 (list (bv 382252089433 (bitvector 64))
                    (bv 133143986207 (bitvector 64))
                    (bv 382252089433 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list r0_0 r0_1))

; Run 2 archstates.
(define r1_0 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996224 (bitvector 64))))

(define r1_1 (list (bv 549755814016 (bitvector 64))
                    (bv 605590388877 (bitvector 64))
                    (bv 549755814016 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list r1_0 r1_1))

(define myexpr (cexpr #:depth 1))

; Synthesis mode.
; (define sol 
;   (synthesize
;     #:forall (list r0 r1)
;     #:guarantee (assert (diff 0 1 r0 0 1 r1 myexpr))))

; Angelic execution mode.
; (define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
;                                (diff 1 2 r0 1 2 r1 myexpr)
;                               ;  (diff 2 2 r0 2 2 r1 myexpr)
; ))))

; TODO: test out finishing this part later.
(define sol
  (solve
    (assert (debug 0 1 r0 0 1 r1 myexpr))))

(print-forms sol)

; Returns: '(define myexpr (IF (BOOL #f) INSTR))'

; (define sol (solve (assert (or (diff 0 0 r1 0 0 r1 myexpr)
;                                (diff 0 1 r1 0 1 r1 myexpr)
;                                (diff 1 1 r1 1 1 r1 myexpr)
; ))))

; (print-forms sol)