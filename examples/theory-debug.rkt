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
(struct INSTR (name operand) #:transparent)
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
; (struct MEM-LOAD (a) #:transparent)
; (struct MEM-STORE (a bs) #:transparent)
(struct ADDR (a) #:transparent)         ; Address value.

; New object that holds register values and instruction together.
(struct run-step (regs instr)
  #:constructor-name make-run-step
  #:transparent)

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))               <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))                   <-- Supported (leaked program counter).
; (IF (INSTR == `LOAD) REG[OPERAND2])   <-- In progress (leaked address from loads/stores).
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                (INSTR (name) (operand))  ; Testing still.
                )]
  [name (choose 'LOAD
                'STORE)]
  [operand (?? integer?)]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (REG (?? integer?))
              (ADDR (?? integer?))
              ; (MEM-LOAD (bs))
              ; (MEM-STORE (bs) (bs))
              )]
  )


(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval e xstate)
  (destruct e [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]))

; Evaluation function for predicates.
(define (eval-pred p xstate)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p xstate))]
            [(AND p1 p2) (and (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(OR p1 p2) (or (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]
            [(INSTR name operand) (eval-instr name operand xstate)]))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            ))

; Evaluation function for instructions.
(define (eval-instr name operand xstate)
  (match name
    ['LOAD
      (println "Got a 'LOAD; just testing for now.")
      (eval-reg operand xstate)]
    ['STORE
      (println "Got a 'STORE; not yet implemented.")]))

; Evaluation function for addresses.
(define (eval-addr addr xstate)
  (list-ref xstate addr))
  ; (eval-bs addr xstate))
  ; (destruct addr
  ;   [(MEM-LOAD a) (eval-bs a xstate)]
  ;   [(MEM-STORE a _) (eval-bs a xstate)]))

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
          (or (not (empty-obs expr (run-step-regs (list-ref r_ i_))))
              (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_)
          (or (not (empty-obs expr (run-step-regs (list-ref r i))))
              (diff (+ i 1) j r j_ j_ r_ expr))
          (or (and (empty-obs expr (run-step-regs (list-ref r i)))
                  (diff (+ i 1) j r i_ j_ r_ expr))
              (and (empty-obs expr (run-step-regs (list-ref r_ i_)))
                  (diff i j r (+ i_ 1) j_ r_ expr))
              (and (not (empty-obs expr (run-step-regs (list-ref r i))))
                   (not (empty-obs expr (run-step-regs (list-ref r_ i_))))
                   (equal? (run-step-instr (list-ref r i))
                           (run-step-instr (list-ref r_ i_)))
                   (not (obs-equal expr (run-step-regs (list-ref r i))
                                        (run-step-regs (list-ref r_ i_)))))))))

; ------------- END-CORE ------------------ ;
; Register state @ instruction: PLACEHOLDER
(define r0_0 (list (bv 176093659177 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439808 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_1 (list (bv 176093659177 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439811 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_2 (list (bv 176093659177 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439813 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_3 (list (bv 176093659151 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446630612648439817 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_4 (list (bv 176093659151 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446630612648439820 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_5 (list (bv 176093659151 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446630612648439827 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_6 (list (bv 176093659151 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446630612648439832 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_7 (list (bv 2930653972 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 135 (bitvector 64))
                   (bv 18446630612648439837 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_8 (list (bv 2930653972 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 42 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 3 (bitvector 64))
                   (bv 18446630612648439839 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_9 (list (bv 2930653972 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 42 (bitvector 64))
                   (bv 1984274891676 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446630612648439842 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_10 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 1984274891676 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446630612648439849 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_11 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 1984274891676 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 18 (bitvector 64))
                    (bv 18446630612648439854 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_12 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446630612648439856 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_13 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446630612648439859 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_14 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405579 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2182 (bitvector 64))
                    (bv 18446630612648439861 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_15 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439863 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_16 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439865 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_17 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439900 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r0_18 (list (bv 2930653972 (bitvector 64))
                    (bv 1662152343939 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list (make-run-step r0_0 'LOAD) (make-run-step r0_1 'LOAD) (make-run-step r0_2 'LOAD) (make-run-step r0_3 'LOAD) (make-run-step r0_4 'LOAD) (make-run-step r0_5 'LOAD) (make-run-step r0_6 'LOAD) (make-run-step r0_7 'LOAD) (make-run-step r0_8 'LOAD) (make-run-step r0_9 'LOAD) (make-run-step r0_10 'LOAD) (make-run-step r0_11 'LOAD) (make-run-step r0_12 'LOAD) (make-run-step r0_13 'LOAD) (make-run-step r0_14 'LOAD) (make-run-step r0_15 'LOAD) (make-run-step r0_16 'LOAD) (make-run-step r0_17 'LOAD) (make-run-step r0_18 'LOAD)))

;(define r0 (list (make-run-step r0_0 (OPCODE (bv #b00000011000000 (bitvector 8)))) ; Example memory store opcode

; Register state @ instruction: PLACEHOLDER
(define r1_0 (list (bv 176093659177 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439808 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_1 (list (bv 176093659177 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439811 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_2 (list (bv 176093659177 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439813 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_3 (list (bv 176093659151 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446630612648439817 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_4 (list (bv 176093659151 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 18 (bitvector 64))
                   (bv 18446630612648439820 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_5 (list (bv 176093659151 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446630612648439827 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_6 (list (bv 176093659151 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446630612648439832 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_7 (list (bv 2930653972 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 135 (bitvector 64))
                   (bv 18446630612648439837 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_8 (list (bv 2930653972 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 42 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 3 (bitvector 64))
                   (bv 18446630612648439839 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_9 (list (bv 2930653972 (bitvector 64))
                   (bv 1142461301002 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 42 (bitvector 64))
                   (bv 1984274891676 (bitvector 64))
                   (bv 78 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446630612648439842 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_10 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 1984274891676 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446630612648439849 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_11 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 1984274891676 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 18 (bitvector 64))
                    (bv 18446630612648439854 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_12 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713390435 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2 (bitvector 64))
                    (bv 18446630612648439856 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_13 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446630612648439859 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_14 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405579 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2182 (bitvector 64))
                    (bv 18446630612648439861 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_15 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439863 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_16 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439865 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_17 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv 18446630612648439900 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_18 (list (bv 2930653972 (bitvector 64))
                    (bv 1142461301002 (bitvector 64))
                    (bv 1524713405559 (bitvector 64))
                    (bv 42 (bitvector 64))
                    (bv 925 (bitvector 64))
                    (bv 78 (bitvector 64))
                    (bv 2054 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list (make-run-step r1_0 'LOAD) (make-run-step r1_1 'LOAD) (make-run-step r1_2 'LOAD) (make-run-step r1_3 'LOAD) (make-run-step r1_4 'LOAD) (make-run-step r1_5 'LOAD) (make-run-step r1_6 'LOAD) (make-run-step r1_7 'LOAD) (make-run-step r1_8 'LOAD) (make-run-step r1_9 'LOAD) (make-run-step r1_10 'LOAD) (make-run-step r1_11 'LOAD) (make-run-step r1_12 'LOAD) (make-run-step r1_13 'LOAD) (make-run-step r1_14 'LOAD) (make-run-step r1_15 'LOAD) (make-run-step r1_16 'LOAD) (make-run-step r1_17 'LOAD) (make-run-step r1_18 'LOAD)))

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
                               (diff 19 18 r0 19 18 r1 myexpr)
))))

(print-forms sol)