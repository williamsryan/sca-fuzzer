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
(struct OPCODE (bs) #:transparent)
(struct OPERAND (bs) #:transparent)
; (struct OPERANDS (op1 op2) #:transparent)
(struct INSTR ())
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

; Some logging stuff.
(define (log-info . messages)
  (apply printf "[INFO]: ~a\n" messages))

(define (log-debug . messages)
  (apply printf "[DEBUG]: ~a\n" messages))

(define (log-error . messages)
  (apply printf "[ERROR]: ~a\n" messages))

;; Helper function to get the register name from the register index.
(define (get-register-name reg-index registers)
  (case reg-index
    ((0) RAX)
    ((1) RBP)
    ((2) RBX)
    ((3) RCX)
    ((4) RDI)
    ((5) RDX)
    ((6) RSI)
    ((7) RSP)
    ((8) R8)
    ((9) R9)
    ((10) R10)
    ((11) R11)
    ((12) R12)
    ((13) R13)
    ((14) R14)
    ((15) R15)
    ((16) PC)
    (else #f)))

; Grammar for the actual contract.
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                (OPCODE (bv (?? integer?) (bitvector (16))))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (REG (?? integer?))
              )]
  )

(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval expr xstate)
  ; (log-debug "[eval]")
  (destruct expr
    [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]
    [_ EMPTY]))

; Evaluation function for predicates.
(define (eval-pred pred xstate)
  ; (log-debug "[eval-pred]")
  ; (log-debug pred)
  (destruct pred
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p xstate))]
            [(AND p1 p2) (and (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(OR p1 p2) (or (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]
            [(OPCODE op) op]
            [bs (log-error "Got an unknown pred") (log-error bs) #f]))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            [INSTR (eval-reg PC xstate)]
            ))

(define (eval-opcode opcode xstate)
  ; (log-debug "[eval-opcode]")
  ; (log-debug opcode)
  (match opcode
    [bv bv]
    [_ (log-error "Invalid opcode")]))

; Evaluation function for registers.
(define (eval-reg reg xstate)
  ; (log-debug "[eval-reg]")
  ; (log-debug (list-ref xstate reg))
  (list-ref xstate reg))

; obs() takes an expression and a xstate
;       returns its observation
(define (obs expr xstate)
  ; (log-debug "[obs]")
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
  ; (log-debug "[obs-equal] ...")
  (listbv-equal (obs expr xstate1) (obs expr xstate2)))

; listbv-equal() takes two observations
;                returns true if they are the same
;                        false otherwise
(define (listbv-equal bvs1 bvs2)
  ; (log-debug "[listbv-equal] ...")
  (if (empty? bvs1)
      (if (empty? bvs2) #t #f)
      (if (empty? bvs2)
          #f
          (and (bveq (first bvs1) (first bvs2)) (listbv-equal (rest bvs1) (rest bvs2))))))

; Extract the list of register values from the xstate.
(define (get-regs xstate)
  ; (log-debug "[get-regs]")
  (define (process-item item)
    (match item
      [(REG reg) reg]
      [_ #f]))        ; Ignore non-register values.

  (map process-item xstate))

(define (get-opcode xstate)
  ; (log-debug "[get-opcode]")
  (define (process-item item)
    (match item
      [(OPCODE opcode) (eval-opcode opcode xstate)]
      [_ #f]))        ; Ignore non-opcode values.

  (map process-item xstate))

(define (opcode-equal xstate1 xstate2)
  ; (log-debug "[opcode-equal]")
  (eq? (get-opcode xstate1) (get-opcode xstate2)))

; diff() takes the following arguments:
;              i,j,i_,j_  : natural numbers such that i <= j and i_ <= j_
;              r, r_      : two run objects
;              expr       : our grammar expression
;
;        returns true if the trace produced by r[i]->r[j] and r_[i_]->r_[j_] are distinguishable
;                false otherwise
(define (diff i j r i_ j_ r_ expr)
  (if (equal? i j)
      (if (equal? i_ j_) #f
                         (or (not (empty-obs expr (get-regs (list-ref r_ i_))))
                             (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_) (or (not (empty-obs expr (get-regs (list-ref r i))))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (get-regs (list-ref r i)))
                                  (diff (+ i 1) j r i_ j_ r_ expr))
                             (and (empty-obs expr (get-regs (list-ref r_ i_)))
                                  (diff i j r (+ i_ 1) j_ r_ expr))
                             (and (opcode-equal (get-opcode (list-ref r i)) (get-opcode (list-ref r_ i_)))
                                  (not (empty-obs expr (get-regs (list-ref r i))))
                                  (not (empty-obs expr (get-regs (list-ref r_ i_))))
                                  (not (obs-equal expr (get-regs (list-ref r i)) (get-regs (list-ref r_ i_)))))))))

; ------------- END-CORE ------------------ ;
