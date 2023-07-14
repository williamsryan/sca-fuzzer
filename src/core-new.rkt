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
(struct OPCODE (bs op1 op2) #:transparent)
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

; New object that holds register values and instruction together.
(struct run-step (regs instr)
  #:constructor-name make-run-step
  #:transparent)

(define (run-step-reg-index step)
  (cdr step))

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))               <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))                   <-- Supported (leaked program counter).
; (IF (INSTR == `LOAD) REG[OPERAND2])   <-- In progress (leaked address from loads/stores).
; (IF (OPCODE (bs?)) REG[OPERAND2])     <-- Instead of INSTR, get the opcode and map back to INSTR later.
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                (OPCODE (bs) (bs) (bs))
                ; (INSTR (name))
                )]
  ; [name (choose 'LOAD
  ;               'STORE)]
  ; [operand (?? integer?)]
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
            [(OPCODE bs op1 op2) (eval-opcode bs op1 op2 xstate)]))
            ; [(INSTR name operand) (eval-instr name operand xstate)]))

(define (eval-opcode bs op1 op2 xstate)
  (let ((opcode (get-opcode bs)))  ; Get the specific opcode based on the bitvector bs
    (cond
      ((eq? opcode 'LOAD)
       ; Retrieve the values of registers or operands based on op1 and op2.
       ; Perform the evaluation or comparison using the retrieved values.
       ; Return the result of the evaluation.
       ...)
      ((eq? opcode 'STORE)
       ; Handle the STORE opcode case similarly.
       ...)
      (else
       ; Handle other opcode cases, if any.
       ...))))

; Get opcode value; assuming it is first element in xstate list.
(define (get-opcode xstate)
  (car xstate))

; (define (get-opcode bs)
;   (destruct bs
;     [(bv 0 4) 'LOAD]
;     [(bv 1 4) 'STORE]
;     [(bv 2 4) 'ADD]
;     ; Add more patterns later.
;     [else (error "Unknown opcode")]))

(define (get-instr xstate)
  (cdr xstate))

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

; extract-observation() takes a run object
; This is just a test function for now.
; (define (extract-observations run)
;   (let loop ((steps run) (observations '()))
;     (if (null? steps)
;         observations
;         (let* ((step (car steps))
;                (instruction (run-step-instr step))
;                (registers (run-step-regs step))
;                (register-index (run-step-reg-index step))
;                (register-name (get-register-name register-index)))
;           (if (eq? instruction '(LOAD))
;               (let ((reg-value (list-ref registers register-index)))
;                 (loop (cdr steps) (cons `(IF (INSTR == 'LOAD) (REG ${register-name})) ,reg-value) observations)))
;               (loop (cdr steps) observations)))))

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
