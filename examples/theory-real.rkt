#lang rosette/safe

; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
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

(define (eval e x)
  (destruct e
            [(IF pred bs) (if (eval-pred pred x) (list (eval-bs bs x))
                              EMPTY)]))

(define (eval-pred p x)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p x))]
            [(AND p1 p2) (and (eval-pred p1 x) (eval-pred p2 x))]
            [(OR p1 p2) (or (eval-pred p1 x) (eval-pred p2 x))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 x) (eval-bs bs2 x))]))

(define (eval-bs bs x)
  (destruct bs
     [(BS b) b]
     [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b x))]
     [(REG reg) (eval-reg reg x)]
     [INSTR (eval-reg PC x)]
     ))

(define (eval-reg reg x)
  (list-ref x reg))

; Auxiliary functions
; obs() takes an expression and a xstate 
;       returns its observation
(define (obs expr state)
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
;        returns true if the trace produce r[i]->r[j] and r_[i_]->r_[j_] are distinguishable 
;                false otherwise
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

(define (diff_new i j r i_ j_ r_ expr)
  (println "Checking if r and r_ are distinguishable...")
  (if (equal? i j)            ; Are we looking at the same states? E.g., i=j=0.
      (if (equal? i_ j_) #f   ; If i=j=i_=j_ traces are identical, return false.
        (println "[-] No distinguishable observable from traces"))
      (if (equal? i_ j_) (or (not (empty-obs expr (list-ref r i)))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (list-ref r i))
                                  (diff (+ i 1) j r i_ j_ r_ expr)))))
    (assert #t) ; Should be true at this point, traces are distinguishable.
    (println "End of diff_new"))
  
; ------------- END-CORE ------------------ ;
(define r0_0 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996224 (bitvector 64))))

(define r0_1 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996227 (bitvector 64))))

(define r0_2 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996229 (bitvector 64))))

(define r0_3 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446621768290996236 (bitvector 64))))

(define r0_4 (list (bv 760209211569 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996242 (bitvector 64))))

(define r0_5 (list (bv 382252089433 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996246 (bitvector 64))))

(define r0_6 (list (bv 382252089433 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996250 (bitvector 64))))

(define r0_7 (list (bv 382252089433 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446621768290996257 (bitvector 64))))

(define r0_8 (list (bv 382252089433 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521319 (bitvector 64))
                   (bv 2194 (bitvector 64))
                   (bv 18446621768290996261 (bitvector 64))))

(define r0_9 (list (bv 382252089433 (bitvector 64))
                   (bv 133143986207 (bitvector 64))
                   (bv 382252089433 (bitvector 64))
                   (bv 1610612736269 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521319 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446621768290996264 (bitvector 64))))

(define r0_10 (list (bv 382252089433 (bitvector 64))
                    (bv 133143986207 (bitvector 64))
                    (bv 382252089433 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996267 (bitvector 64))))

(define r0_11 (list (bv 382252089433 (bitvector 64))
                    (bv 133143986207 (bitvector 64))
                    (bv 382252089433 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996269 (bitvector 64))))

(define r0_12 (list (bv 382252089433 (bitvector 64))
                    (bv 133143986207 (bitvector 64))
                    (bv 382252089433 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996349 (bitvector 64))))

(define r0_13 (list (bv 382252089433 (bitvector 64))
                    (bv 133143986207 (bitvector 64))
                    (bv 382252089433 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9 r0_10 r0_11 r0_12 r0_13))

(define r1_0 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996224 (bitvector 64))))

(define r1_1 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996227 (bitvector 64))))

(define r1_2 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 1997159793105 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 71 (bitvector 64))
                   (bv 18446621768290996229 (bitvector 64))))

(define r1_3 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446621768290996236 (bitvector 64))))

(define r1_4 (list (bv 1503238553950 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996242 (bitvector 64))))

(define r1_5 (list (bv 549755814016 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996246 (bitvector 64))))

(define r1_6 (list (bv 549755814016 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446621768290996250 (bitvector 64))))

(define r1_7 (list (bv 549755814016 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521247 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446621768290996257 (bitvector 64))))

(define r1_8 (list (bv 549755814016 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736375 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521319 (bitvector 64))
                   (bv 2194 (bitvector 64))
                   (bv 18446621768290996261 (bitvector 64))))

(define r1_9 (list (bv 549755814016 (bitvector 64))
                   (bv 605590388877 (bitvector 64))
                   (bv 549755814016 (bitvector 64))
                   (bv 1610612736269 (bitvector 64))
                   (bv 465 (bitvector 64))
                   (bv 1507533521319 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446621768290996264 (bitvector 64))))

(define r1_10 (list (bv 549755814016 (bitvector 64))
                    (bv 605590388877 (bitvector 64))
                    (bv 549755814016 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996267 (bitvector 64))))

(define r1_11 (list (bv 549755814016 (bitvector 64))
                    (bv 605590388877 (bitvector 64))
                    (bv 549755814016 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996269 (bitvector 64))))

(define r1_12 (list (bv 549755814016 (bitvector 64))
                    (bv 605590388877 (bitvector 64))
                    (bv 549755814016 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446621768290996349 (bitvector 64))))

(define r1_13 (list (bv 549755814016 (bitvector 64))
                    (bv 605590388877 (bitvector 64))
                    (bv 549755814016 (bitvector 64))
                    (bv 1610612736269 (bitvector 64))
                    (bv 465 (bitvector 64))
                    (bv 1507533521319 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9 r1_10 r1_11 r1_12 r1_13))

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert (diff_new 0 0 r0 0 1 r1 myexpr))))

; (define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
;                                (diff 1 2 r0 1 2 r1 myexpr)
;                                (diff 2 2 r0 2 2 r1 myexpr)
;                                (diff 2 3 r0 2 3 r1 myexpr)
;                                (diff 3 3 r0 3 3 r1 myexpr)
;                                (diff 3 4 r0 3 4 r1 myexpr)
;                                (diff 4 4 r0 4 4 r1 myexpr)
;                                (diff 4 5 r0 4 5 r1 myexpr)
;                                (diff 5 5 r0 5 5 r1 myexpr)
;                                (diff 5 6 r0 5 6 r1 myexpr)
;                                (diff 6 6 r0 6 6 r1 myexpr)
;                                (diff 6 7 r0 6 7 r1 myexpr)
;                                (diff 7 7 r0 7 7 r1 myexpr)
;                                (diff 7 8 r0 7 8 r1 myexpr)
;                                (diff 8 8 r0 8 8 r1 myexpr)
;                                (diff 8 9 r0 8 9 r1 myexpr)
;                                (diff 9 9 r0 9 9 r1 myexpr)
;                                (diff 9 10 r0 9 10 r1 myexpr)
;                                (diff 10 10 r0 10 10 r1 myexpr)
;                                (diff 10 11 r0 10 11 r1 myexpr)
;                                (diff 11 11 r0 11 11 r1 myexpr)
;                                (diff 11 12 r0 11 12 r1 myexpr)
;                                (diff 12 12 r0 12 12 r1 myexpr)
;                                (diff 12 13 r0 12 13 r1 myexpr)
;                                (diff 13 13 r0 13 13 r1 myexpr)
;                                (diff 13 14 r0 13 14 r1 myexpr)
;                                (diff 14 13 r0 14 13 r1 myexpr)
; ))))

(print-forms sol)
