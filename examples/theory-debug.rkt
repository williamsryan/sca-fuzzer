#lang rosette/safe
; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
(require rosette/lib/destruct) ; Value destructuring library.
(require rosette/lib/synthax) ; Synthesis library.

; General purpose register encoding.
(define RAX 0)  ; A eXtended
(define RBP 1)  ; Base Pointer
(define RBX 2)  ; B eXtended
(define RCX 3)  ; C eXtended
(define RDI 4)  ; Destination Index
(define RDX 5)  ; D eXtended
(define RSI 6)  ; Source Index
(define RSP 7)  ; Stack Pointer
(define R8 8)   ; R8
(define R9 9)   ; R9
(define R10 10) ; R10
(define R11 11) ; R11
(define R12 12) ; R12
(define R13 13) ; R13
(define R14 14) ; R14
(define R15 15) ; R15
(define PC 16)  ; R16

; Struct definitions for our contract language.
(struct IF (pred expr) #:transparent)   ; Represents an if-expression.
(struct OPCODE ())                      ; An opcode.
(struct INSTR ())                       ; An instruction.
(struct SLIDE (i1 i2 bs) #:transparent) ; A sliding window operation.
(struct RS1 ())                         ; Register RS1.
(struct RS2 ())                         ; Register RS2.
(struct NOT (b))
(struct AND (b1 b2))
(struct OR (b1 b2))
(struct EQ (b1 b2))
(struct BOOL (b))
(struct BS (bs))                        ; Bitstring value.
(struct REG (r) #:transparent)          ; Register value.
; (struct ADDR (a) #:transparent)         ; Address value.
(struct MEM-LOAD (a) #:transparent)
(struct MEM-STORE (a bs) #:transparent)
(struct ADDR-CONST (a))
(struct ADDR-REG (r))

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))     <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))         <-- Supported (leaked program counter).
; (if (BOOL #t) (...))        <-- In progress (leaked address of loads/stores).
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
              (INSTR)
              ; (ADDR (?? integer?))
              (MEM-LOAD (a))
              (MEM-STORE (a) (bs))
              )]
  [a (choose (ADDR-CONST (?? integer?))
             (ADDR-REG (?? integer?))
        )]
  )

(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval e x)
  (destruct e [(IF pred bs) (if (eval-pred pred x) (list (eval-bs bs x)) EMPTY)]))

; Evaluation function for predicates.
(define (eval-pred p x)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p x))]
            [(AND p1 p2) (and (eval-pred p1 x) (eval-pred p2 x))]
            [(OR p1 p2) (or (eval-pred p1 x) (eval-pred p2 x))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 x) (eval-bs bs2 x))]))

; Evaluation function for bit sequences.
(define (eval-bs bs x)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b x))]
            [(REG reg) (eval-reg reg x)]
            [(MEM-LOAD addr) (eval-addr addr x)]
            [(MEM-STORE addr b) (eval-addr addr x) (eval-bs b x)]
            ; [(ADDR addr) (eval-addr addr x)]
            [INSTR (eval-reg PC x)]))

; Evaluation function for addresses.
(define (eval-addr addr x)
  (list-ref x addr))

; Evaluation function for registers.
(define (eval-reg reg x)
  (list-ref x reg))

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
  (listbv-equal (obs expr state1) (obs expr state2)))

; listbv-equal() takes two observations
;                returns true if they are the same
;                        false otherwise
(define (listbv-equal bvs1 bvs2)
  (if (empty? bvs1)
      (if (empty? bvs2) #t #f)
      (if (empty? bvs2)
          #f
          (and (bveq (first bvs1) (first bvs2)) (listbv-equal (rest bvs1) (rest bvs2))))))

; diff() takes the following arguments:
;              i,j,i_,j_  : natural numbers such that i <= j and i_ <= j_
;              r, r_      : two run objects
;              expr       : our grammar expression
;
;        returns true if the trace produced by r[i]->r[j] and r_[i_]->r_[j_] are distinguishable
;                false otherwise
(define (diff i j r i_ j_ r_ expr)
  (if (equal? i j)
      (if (equal? i_ j_)
          #f
          (or (not (empty-obs expr (list-ref r_ i_))) (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_)
          (or (not (empty-obs expr (list-ref r i))) (diff (+ i 1) j r j_ j_ r_ expr))
          (or (and (empty-obs expr (list-ref r i)) (diff (+ i 1) j r i_ j_ r_ expr))
              (and (empty-obs expr (list-ref r_ i_)) (diff i j r (+ i_ 1) j_ r_ expr))
              (and (not (empty-obs expr (list-ref r i)))
                   (not (empty-obs expr (list-ref r_ i_)))
                   (not (obs-equal expr (list-ref r i) (list-ref r_ i_))))))))

; ------------- END-CORE ------------------ ;

(define r0_0 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758080 (bitvector 64))))

(define r0_1 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758083 (bitvector 64))))

(define r0_2 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758085 (bitvector 64))))

(define r0_3 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610568 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 23 (bitvector 64))
                   (bv 18446624712099758087 (bitvector 64))))

(define r0_4 (list (bv 2057734405 (bitvector 64))
                   (bv 1889785610568 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 7 (bitvector 64))
                   (bv 18446624712099758092 (bitvector 64))))

(define r0_5 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758099 (bitvector 64))))

(define r0_6 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446624712099758103 (bitvector 64))))

(define r0_7 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758110 (bitvector 64))))

(define r0_8 (list (bv 4851 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758115 (bitvector 64))))

(define r0_9 (list (bv 19440 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 785979015351 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 2074469204451 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 2183 (bitvector 64))
                   (bv 18446624712099758118 (bitvector 64))))

(define r0_10 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758125 (bitvector 64))))

(define r0_11 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758129 (bitvector 64))))

(define r0_12 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758136 (bitvector 64))))

(define r0_13 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758142 (bitvector 64))))

(define r0_14 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758144 (bitvector 64))))

(define r0_15 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758203 (bitvector 64))))

(define r0_16 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 785979015351 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 2074469204451 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9 r0_10 r0_11 r0_12 r0_13 r0_14 r0_15 r0_16))

(define r1_0 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758080 (bitvector 64))))

(define r1_1 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758083 (bitvector 64))))

(define r1_2 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610680 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 19 (bitvector 64))
                   (bv 18446624712099758085 (bitvector 64))))

(define r1_3 (list (bv 2031519531481 (bitvector 64))
                   (bv 1889785610568 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 23 (bitvector 64))
                   (bv 18446624712099758087 (bitvector 64))))

(define r1_4 (list (bv 2057734405 (bitvector 64))
                   (bv 1889785610568 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 7 (bitvector 64))
                   (bv 18446624712099758092 (bitvector 64))))

(define r1_5 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 330712481869 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758099 (bitvector 64))))

(define r1_6 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446624712099758103 (bitvector 64))))

(define r1_7 (list (bv 2057734405 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758110 (bitvector 64))))

(define r1_8 (list (bv 4851 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758115 (bitvector 64))))

(define r1_9 (list (bv 19440 (bitvector 64))
                   (bv 328 (bitvector 64))
                   (bv 2052994367966 (bitvector 64))
                   (bv 560 (bitvector 64))
                   (bv 137438953504 (bitvector 64))
                   (bv 1443109011792 (bitvector 64))
                   (bv 2183 (bitvector 64))
                   (bv 18446624712099758118 (bitvector 64))))

(define r1_10 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758125 (bitvector 64))))

(define r1_11 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758129 (bitvector 64))))

(define r1_12 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758136 (bitvector 64))))

(define r1_13 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758142 (bitvector 64))))

(define r1_14 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758144 (bitvector 64))))

(define r1_15 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758203 (bitvector 64))))

(define r1_16 (list (bv 19440 (bitvector 64))
                    (bv 328 (bitvector 64))
                    (bv 2052994367966 (bitvector 64))
                    (bv 560 (bitvector 64))
                    (bv 137438953504 (bitvector 64))
                    (bv 1443109011792 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9 r1_10 r1_11 r1_12 r1_13 r1_14 r1_15 r1_16))

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                               (diff 1 2 r0 1 2 r1 myexpr)
))))

(print-forms sol)