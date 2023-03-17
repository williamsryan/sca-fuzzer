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

; ------------- END-CORE ------------------ ;

(define r1_0 (list (bv 1997159792943 (bitvector 64))
                   (bv 347892351057 (bitvector 64))
                   (bv 1086626726141 (bitvector 64))
                   (bv 1163936137487 (bitvector 64))
                   (bv 38654705673 (bitvector 64))
                   (bv 1301375090991 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_1 (list (bv 1056561955084 (bitvector 64))
                   (bv 880468295885 (bitvector 64))
                   (bv 1447403979089 (bitvector 64))
                   (bv 343597383760 (bitvector 64))
                   (bv 2134598746609 (bitvector 64))
                   (bv 1151051235596 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_2 (list (bv 2560 (bitvector 64))
                   (bv 1997159792764 (bitvector 64))
                   (bv 36 (bitvector 64))
                   (bv 387 (bitvector 64))
                   (bv 1705102016909 (bitvector 64))
                   (bv 1997159792908 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_3 (list (bv 2560 (bitvector 64))
                   (bv 1748051689634 (bitvector 64))
                   (bv 36 (bitvector 64))
                   (bv 387 (bitvector 64))
                   (bv 429496729700 (bitvector 64))
                   (bv 1748051689938 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_4 (list (bv 498216206416 (bitvector 64))
                   (bv 1550483194052 (bitvector 64))
                   (bv 2160368550399 (bitvector 64))
                   (bv 5 (bitvector 64))
                   (bv 151 (bitvector 64))
                   (bv 103 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_5 (list (bv 498216206416 (bitvector 64))
                   (bv 2044404433092 (bitvector 64))
                   (bv 2160368550399 (bitvector 64))
                   (bv 5 (bitvector 64))
                   (bv 151 (bitvector 64))
                   (bv 103 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_6 (list (bv 370 (bitvector 64))
                   (bv 2147483648500 (bitvector 64))
                   (bv 438086664194 (bitvector 64))
                   (bv 1589137899890 (bitvector 64))
                   (bv 1 (bitvector 64))
                   (bv 1692217114988 (bitvector 64))
                   (bv 2055 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1_7 (list (bv 246 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 1816771166512 (bitvector 64))
                   (bv 1056561955062 (bitvector 64))
                   (bv 1 (bitvector 64))
                   (bv 1730871820661 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv -1 (bitvector 64))))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7))

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert (or (diff 0 0 r1 0 0 r1 myexpr)
                               (diff 0 1 r1 0 1 r1 myexpr)
                               (diff 1 7 r1 1 7 r1 myexpr)
))))

(print-forms sol)