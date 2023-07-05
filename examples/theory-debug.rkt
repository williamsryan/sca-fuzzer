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
(struct MEM-LOAD (a) #:transparent)
(struct MEM-STORE (a bs) #:transparent)
(struct ADDR-CONST (a))
(struct ADDR-REG (r))
; (struct ADDR (a) #:transparent)         ; Address value.

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
              (MEM-LOAD (?? integer?))
              (MEM-STORE (?? integer?) (bs))
              )]
  ; [a (choose (ADDR-CONST (?? integer?))
  ;            (ADDR-REG (?? integer?))
  ;       )]
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
(define r0_0 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758080 (bitvector 64))))

(define r0_1 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758083 (bitvector 64))))

(define r0_2 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758085 (bitvector 64))))

(define r0_3 (list (bv 1803886264741 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 135 (bitvector 64))
                   (bv 18446624712099758087 (bitvector 64))))

(define r0_4 (list (bv 1803886264741 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 2050 (bitvector 64))
                   (bv 18446624712099758089 (bitvector 64))))

(define r0_5 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446624712099758096 (bitvector 64))))

(define r0_6 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446624712099758100 (bitvector 64))))

(define r0_7 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758103 (bitvector 64))))

(define r0_8 (list (bv 1803886324523 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323328 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 2183 (bitvector 64))
                   (bv 18446624712099758106 (bitvector 64))))

(define r0_9 (list (bv 1803886324485 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 734439407787 (bitvector 64))
                   (bv 614180323328 (bitvector 64))
                   (bv 1241245548833 (bitvector 64))
                   (bv 23 (bitvector 64))
                   (bv 18446624712099758108 (bitvector 64))))

(define r0_10 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548833 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758110 (bitvector 64))))

(define r0_11 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548833 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758130 (bitvector 64))))

(define r0_12 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758134 (bitvector 64))))

(define r0_13 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758140 (bitvector 64))))

(define r0_14 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758144 (bitvector 64))))

(define r0_15 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 734439407787 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446624712099758146 (bitvector 64))))

(define r0_16 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758149 (bitvector 64))))

(define r0_17 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548809 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758151 (bitvector 64))))

(define r0_18 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548806 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758155 (bitvector 64))))

(define r0_19 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548806 (bitvector 64))
                    (bv 3 (bitvector 64))
                    (bv 18446624712099758157 (bitvector 64))))

(define r0_20 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 147 (bitvector 64))
                    (bv 18446624712099758161 (bitvector 64))))

(define r0_21 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 734439407701 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758167 (bitvector 64))))

(define r0_22 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 2183 (bitvector 64))
                    (bv 18446624712099758172 (bitvector 64))))

(define r0_23 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758179 (bitvector 64))))

(define r0_24 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758184 (bitvector 64))))

(define r0_25 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548986 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758186 (bitvector 64))))

(define r0_26 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548834 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758190 (bitvector 64))))

(define r0_27 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548837 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv 18446624712099758194 (bitvector 64))))

(define r0_28 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10808639481867927552 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1241245548837 (bitvector 64))
                    (bv 19 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9 r0_10 r0_11 r0_12 r0_13 r0_14 r0_15 r0_16 r0_17 r0_18 r0_19 r0_20 r0_21 r0_22 r0_23 r0_24 r0_25 r0_26 r0_27 r0_28))

(define r1_0 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758080 (bitvector 64))))

(define r1_1 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758083 (bitvector 64))))

(define r1_2 (list (bv 1803886264740 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 211 (bitvector 64))
                   (bv 18446624712099758085 (bitvector 64))))

(define r1_3 (list (bv 1803886264741 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 135 (bitvector 64))
                   (bv 18446624712099758087 (bitvector 64))))

(define r1_4 (list (bv 1803886264741 (bitvector 64))
                   (bv 120259084316 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 2050 (bitvector 64))
                   (bv 18446624712099758089 (bitvector 64))))

(define r1_5 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446624712099758096 (bitvector 64))))

(define r1_6 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446624712099758100 (bitvector 64))))

(define r1_7 (list (bv 1803886264741 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323471 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446624712099758103 (bitvector 64))))

(define r1_8 (list (bv 1803886324523 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323328 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 2183 (bitvector 64))
                   (bv 18446624712099758106 (bitvector 64))))

(define r1_9 (list (bv 1803886324485 (bitvector 64))
                   (bv 28 (bitvector 64))
                   (bv 167503724601 (bitvector 64))
                   (bv 1919850381759 (bitvector 64))
                   (bv 614180323328 (bitvector 64))
                   (bv 1279900254506 (bitvector 64))
                   (bv 23 (bitvector 64))
                   (bv 18446624712099758108 (bitvector 64))))

(define r1_10 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254506 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758110 (bitvector 64))))

(define r1_11 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254506 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758130 (bitvector 64))))

(define r1_12 (list (bv 1803886324485 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758134 (bitvector 64))))

(define r1_13 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758140 (bitvector 64))))

(define r1_14 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 167503724601 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758144 (bitvector 64))))

(define r1_15 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 1919850381759 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446624712099758146 (bitvector 64))))

(define r1_16 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758149 (bitvector 64))))

(define r1_17 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254482 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758151 (bitvector 64))))

(define r1_18 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 114 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254479 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758155 (bitvector 64))))

(define r1_19 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254479 (bitvector 64))
                    (bv 3 (bitvector 64))
                    (bv 18446624712099758157 (bitvector 64))))

(define r1_20 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 135 (bitvector 64))
                    (bv 18446624712099758161 (bitvector 64))))

(define r1_21 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 1919850381633 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446624712099758167 (bitvector 64))))

(define r1_22 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 2183 (bitvector 64))
                    (bv 18446624712099758172 (bitvector 64))))

(define r1_23 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446624712099758179 (bitvector 64))))

(define r1_24 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758184 (bitvector 64))))

(define r1_25 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254659 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758186 (bitvector 64))))

(define r1_26 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254507 (bitvector 64))
                    (bv 7 (bitvector 64))
                    (bv 18446624712099758190 (bitvector 64))))

(define r1_27 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254510 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv 18446624712099758194 (bitvector 64))))

(define r1_28 (list (bv 2821 (bitvector 64))
                    (bv 28 (bitvector 64))
                    (bv 115 (bitvector 64))
                    (bv 10664523924391329792 (bitvector 64))
                    (bv 614180323328 (bitvector 64))
                    (bv 1279900254510 (bitvector 64))
                    (bv 23 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9 r1_10 r1_11 r1_12 r1_13 r1_14 r1_15 r1_16 r1_17 r1_18 r1_19 r1_20 r1_21 r1_22 r1_23 r1_24 r1_25 r1_26 r1_27 r1_28))

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                               (diff 1 2 r0 1 2 r1 myexpr)
                               (diff 2 2 r0 2 2 r1 myexpr)
                               (diff 2 3 r0 2 3 r1 myexpr)
                               (diff 3 3 r0 3 3 r1 myexpr)
                               (diff 3 4 r0 3 4 r1 myexpr)
                               (diff 4 4 r0 4 4 r1 myexpr)
                               (diff 4 5 r0 4 5 r1 myexpr)
                               (diff 5 5 r0 5 5 r1 myexpr)
                               (diff 5 6 r0 5 6 r1 myexpr)
                               (diff 6 6 r0 6 6 r1 myexpr)
                               (diff 6 7 r0 6 7 r1 myexpr)
                               (diff 7 7 r0 7 7 r1 myexpr)
                               (diff 7 8 r0 7 8 r1 myexpr)
                               (diff 8 8 r0 8 8 r1 myexpr)
                               (diff 8 9 r0 8 9 r1 myexpr)
                               (diff 9 9 r0 9 9 r1 myexpr)
                               (diff 9 10 r0 9 10 r1 myexpr)
                               (diff 10 10 r0 10 10 r1 myexpr)
                               (diff 10 11 r0 10 11 r1 myexpr)
                               (diff 11 11 r0 11 11 r1 myexpr)
                               (diff 11 12 r0 11 12 r1 myexpr)
                               (diff 12 12 r0 12 12 r1 myexpr)
                               (diff 12 13 r0 12 13 r1 myexpr)
                               (diff 13 13 r0 13 13 r1 myexpr)
                               (diff 13 14 r0 13 14 r1 myexpr)
                               (diff 14 14 r0 14 14 r1 myexpr)
                               (diff 14 15 r0 14 15 r1 myexpr)
                               (diff 15 15 r0 15 15 r1 myexpr)
                               (diff 15 16 r0 15 16 r1 myexpr)
                               (diff 16 16 r0 16 16 r1 myexpr)
                               (diff 16 17 r0 16 17 r1 myexpr)
                               (diff 17 17 r0 17 17 r1 myexpr)
                               (diff 17 18 r0 17 18 r1 myexpr)
                               (diff 18 18 r0 18 18 r1 myexpr)
                               (diff 18 19 r0 18 19 r1 myexpr)
                               (diff 19 19 r0 19 19 r1 myexpr)
                               (diff 19 20 r0 19 20 r1 myexpr)
                               (diff 20 20 r0 20 20 r1 myexpr)
                               (diff 20 21 r0 20 21 r1 myexpr)
                               (diff 21 21 r0 21 21 r1 myexpr)
                               (diff 21 22 r0 21 22 r1 myexpr)
                               (diff 22 22 r0 22 22 r1 myexpr)
                               (diff 22 23 r0 22 23 r1 myexpr)
                               (diff 23 23 r0 23 23 r1 myexpr)
                               (diff 23 24 r0 23 24 r1 myexpr)
                               (diff 24 24 r0 24 24 r1 myexpr)
                               (diff 24 25 r0 24 25 r1 myexpr)
                               (diff 25 25 r0 25 25 r1 myexpr)
                               (diff 25 26 r0 25 26 r1 myexpr)
                               (diff 26 26 r0 26 26 r1 myexpr)
                               (diff 26 27 r0 26 27 r1 myexpr)
                               (diff 27 27 r0 27 27 r1 myexpr)
                               (diff 27 28 r0 27 28 r1 myexpr)
                               (diff 28 28 r0 28 28 r1 myexpr)
                               (diff 28 29 r0 28 29 r1 myexpr)
                               (diff 29 28 r0 29 28 r1 myexpr)
))))

(print-forms sol)