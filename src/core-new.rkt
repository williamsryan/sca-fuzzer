#lang rosette/safe
; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
(require rosette/lib/destruct) ; Value destructuring library.
(require rosette/lib/synthax) ; Synthesis library.
(require racket/match)

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
(struct INSTR (instr) #:transparent)    ; An instruction.
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
; Testing new instruction type for more precision.
(struct instruction (opcode operands) #:transparent)
; (struct ADDR (a) #:transparent)         ; Address value.

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))     <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))         <-- Supported (leaked program counter).
; (if (BOOL #t) (...))        <-- In progress (leaked address from loads/stores).
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
              ; (INSTR)
              (INSTR (?? instruction?)) ; New instruction.
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
  (destruct e [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]))

; Evaluation function for predicates.
(define (eval-pred p x)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p xstate))]
            [(AND p1 p2) (and (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(OR p1 p2) (or (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]))

; Evaluation function for bit sequences.
(define (eval-bs bs x)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            [(MEM-LOAD addr) (eval-addr addr xstate)]
            [(MEM-STORE addr b) (eval-addr addr xstate) (eval-bs b xstate)]
            [(INSTR instr) (eval-reg PC xstate)] ; New instruction handling test.
            ; [INSTR (eval-reg PC x)]
            ))

; Evaluation function for instructions.
(define (eval-instr instr)
  (match instr
    [(instruction opcode _) opcode])) ; Ignoring operands for now.

; Evaluation function for addresses.
(define (eval-addr addr xstate)
  (list-ref xstate addr))

; Evaluation function for registers.
(define (eval-reg reg xstate)
  (list-ref xstate reg))

; obs() takes an expression and a xstate
;       returns its observation
(define (obs expr xstate)
  (eval expr xstate))

; obs() takes an expression and a xstate
;       returns true if there is some observations produced
;               false otherwise
(define (empty-obs expr xstate)
  (empty? (eval expr xstate)))

; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
(define (obs-equal expr xstate1 xstate2)
  (listbv-equal (obs expr xstate1) (obs expr xstate2)))

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
