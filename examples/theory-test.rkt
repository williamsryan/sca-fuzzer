#lang rosette/safe
; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
(require rosette/lib/destruct) ; Value destructuring library.
(require rosette/lib/synthax) ; Synthesis library.
(require racket/match)
; (require racket/base)

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
(struct OPERANDS (op1 op2) #:transparent)
(struct INSTR (opcode operands) #:transparent) ; TODO: use INSTR for opcode + operands (pred + bs).
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
(struct ADDR (a) #:transparent)         ; Address value.

; Aux structs.
(struct INTEGER (value) #:transparent)

; Some logging stuff.
(define (log-info message)
  (printf "[INFO]: ~a\n" message))

(define (log-debug message)
  (printf "[DEBUG]: ~a\n" message))

(define (log-error message)
  (printf "[ERROR]: ~a\n" message))

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
(struct run-step (regs instruction)
  #:constructor-name make-run-step
  #:transparent)

(define (run-step-reg-index step)
  (cdr step))

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))               <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))                   <-- Supported (leaked program counter).
; (IF (INSTR == `LOAD) REG[OPERAND2])   <-- In progress (leaked address from loads/stores).
; (IF (OPCODE (bs?)) REG[OPERAND2])     <-- Instead of INSTR, get the opcode and map back to INSTR later.
; (IF (OPCODE #b0000010011 (REG[op2])))
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                (OPCODE (bs))
                (INSTR (bs) (OPERANDS))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (ADDR (?? integer?))
              ; (OPERANDS (?? integer?) (?? integer?)) ; Moving OPERANDS to part of INSTR.
              (REG (choose (?? integer?)
                           (OPERAND))) ; Make this option an OPERAND?
              )]
  )

(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval expr xstate)
  ; (println "[eval]")
  (destruct expr
    [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]
    [_ EMPTY]))

; Evaluation function for predicates.
(define (eval-pred pred xstate)
  ; (println "[eval-pred]")
  (destruct pred
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p xstate))]
            [(AND p1 p2) (and (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(OR p1 p2) (or (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]
            [(OPCODE bs) (eval-opcode bs xstate)]
            [(INSTR opcode ops) (eval-instr opcode ops xstate)]))

; Format notes:
; (IF (OPCODE #b0000001010) (REG[operand2])) : 
; This clause represents the condition where a memory load opcode (#b0000001010) is encountered,
; and the value stored in the register specified by operand2 is leaked.
;
; (IF (OPCODE #b0000010101) ADDR[operand1]) : 
; This clause represents the condition where a memory store opcode (#b0000010101) is encountered,
; and the address referenced by operand1 is considered leaked based on the leakage-expression function.
(define (eval-opcode opcode xstate)
  (log-debug "[eval-opcode] TODO"))

(define (extract-integer value)
  (match value
    [(bv i _) i]
    [_ (log-error "Invalid integer value from bitvector")]))

(define (extract-bits bs start-bit end-bit)
  (bvextract start-bit (+ 1 (- end-bit start-bit)) bs))

(define (bvextract start-bit width bs)
  (bvlshr (bvand bs (bvneg (bvshl (bvneg (bvlshr (bvneg #b0) start-bit)) width))) start-bit))

(define (bv-eq? bv1 bv2)
  (bveq bv1 bv2))

; (define (opcode-equal opcode1 opcode2)
;   (bv-eq? opcode1 opcode2))

; (define (get-instr xstate)
;   (cdr xstate))

; TODO: update this with our desired constraints for instruction operands.
(define (eval-operands op1 op2 xstate)
  (log-debug "[eval-operands] TODO"))
  ; (list-ref xstate op2)
  ; (list-ref xstate op1))
  ; (list ((list-ref xstate op2)
  ;        (list-ref xstate op1))))

(define (eval-operand op xstate)
  (log-info "[eval-operand]")
  (match op
    [(INTEGER value) value]
    [(REG reg) (extract-integer (list-ref xstate reg))]
    ))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            ; [(OPERANDS op1 op2) (eval-operands op1 op2 xstate)]
            [(OPERAND op) (eval-operand op xstate)]
            ; [_ (log-error "Invalid expression for bitstring observation")]
            ))

; Evaluation function for instructions.
(define (eval-instr opcode op1 op2 xstate)
  (log-info "[eval-instr]")
  (let* ((opcode-value (eval-bs opcode xstate))
         (op1-value (eval-bs op1 xstate))
         (op2-value (eval-bs op2 xstate)))
    (cond
      ((eq? opcode-value 'memory-store) op1-value)  ; Update instruction opcode later.
      ((eq? opcode-value 'memory-load) op2-value)   ; Update instruction opcode later.
      (else
        (log-error "Invalid opcode value")
  ))))

; Evaluation function for addresses.
(define (eval-addr addr xstate)
  (list-ref xstate addr))
  ; (eval-bs addr xstate))
  ; (destruct addr
  ;   [(MEM-LOAD a) (eval-bs a xstate)]
  ;   [(MEM-STORE a _) (eval-bs a xstate)]))

; Evaluation function for registers.
(define (eval-reg reg xstate)
  ; (log-debug "[eval-reg]")
  (match reg
    [(? integer? reg-idx) (list-ref xstate reg-idx)]
    [(OPERAND op) (eval-operand op xstate)]
    [_ (log-error "Invalid register value in eval-reg: ~s" reg)]))

; obs() takes an expression and a xstate
;       returns its observation
(define (obs expr xstate)
  (eval expr xstate))

; obs() takes an expression and a xstate
;       returns true if there is some observations produced
;               false otherwise
(define (empty-obs expr xstate)
  (empty? (eval expr xstate)))

(define (instr-equal xstate1 xstate2)
  ; TODO: do we care about equality, or value presence in regs?
  (and (equal? (INSTR-opcode xstate1) (INSTR-opcode xstate2))
       (equal? (OPERANDS-op1 (INSTR-operands xstate1)) (OPERANDS-op1 (INSTR-operands xstate2)))
       (equal? (OPERANDS-op2 (INSTR-operands xstate1)) (OPERANDS-op2 (INSTR-operands xstate2)))))
    
; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
(define (obs-equal expr xstate1 xstate2)
  ; (log-debug "[obs-equal]")
  ; (log-debug (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  ; (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  ; (match-define (INSTR _ _) xstate1)
  (if (and (INSTR? xstate1) (INSTR? xstate2))
      (instr-equal xstate1 xstate2)
      (listbv-equal (obs expr xstate1) (obs expr xstate2))))

; listbv-equal() takes two observations
;                returns true if they are the same
;                        false otherwise
(define (listbv-equal bvs1 bvs2)
  (if (empty? bvs1)
      (if (empty? bvs2) #t #f)
      (if (empty? bvs2)
          #f
          (and (bveq (first bvs1) (first bvs2)) (listbv-equal (rest bvs1) (rest bvs2))))))

; Take our grammar expression and archstate as input.
; Return a list of the operands for the run step.
(define (obs-opcode xstate)
  (log-debug "[obs-opcode] TODO")
  (match xstate
    [(INSTR opcode ops)
     `(INSTR ,(obs opcode) ,(obs ops))]
    [_ (log-error "Invalid expression for opcode observation")]))
  ; (match xstate
  ;   [(OPCODE opcode operands)
  ;    (list opcode operands)]
  ;   [_
  ;    (println "Invalid expression for opcode observation")]))

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
          (or (not (empty-obs expr (run-step-regs (list-ref r_ i_)))
              (diff j j r (+ i_ 1) j_ r_ expr))))
      (if (equal? i_ j_)
          (or (not (empty-obs expr (run-step-regs (list-ref r i)))
              (diff (+ i 1) j r j_ j_ r_ expr)))
          (or (and (empty-obs expr (run-step-regs (list-ref r i)))
                  (diff (+ i 1) j r i_ j_ r_ expr))
              (and (empty-obs expr (run-step-regs (list-ref r_ i_)))
                  (diff i j r (+ i_ 1) j_ r_ expr))
              (not (obs-equal expr (run-step-instruction (list-ref r i)) (run-step-instruction (list-ref r_ i_))))
              (and (not (empty-obs expr (run-step-regs (list-ref r i))))
                   (not (empty-obs expr (run-step-regs (list-ref r_ i_))))
                   (not (obs-equal expr (run-step-regs (list-ref r i))
                                        (run-step-regs (list-ref r_ i_)))))))))

; ------------- END-CORE ------------------ ;
; Instruction: UNMAPPED 
(define r0_0 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035008 (bitvector 64))))	; PC

; Instruction: UNMAPPED 
(define r0_1 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035011 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.0
(define r0_2 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035013 (bitvector 64))))	; PC

; Instruction: ADD RAX, -274095631
(define r0_3 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 3 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035019 (bitvector 64))))	; PC

; Instruction: CMP AL, CL
(define r0_4 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 2179 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035021 (bitvector 64))))	; PC

; Instruction: SBB SI, DI
(define r0_5 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 151 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035024 (bitvector 64))))	; PC

; Instruction: INC AL
(define r0_6 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035026 (bitvector 64))))	; PC

; Instruction: JS .bb_main.1
(define r0_7 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035028 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.2
(define r0_8 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035053 (bitvector 64))))	; PC

; Instruction: ADD SIL, -5
(define r0_9 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1344324763961 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 730144440490 (bitvector 64))	; Register: RDX
                   (bv 1241245613961 (bitvector 64))	; Register: RSI
                   (bv 147 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035057 (bitvector 64))))	; PC

; Instruction: AND RSI, 0b1111111111111
(define r0_10 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576853394 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035064 (bitvector 64))))	; PC

; Instruction: SBB [R14 + RSI], AX
(define r0_11 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576853394 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 19 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035069 (bitvector 64))))	; PC

; Instruction: SUB DI, AX
(define r0_12 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 19 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035072 (bitvector 64))))	; PC

; Instruction: SBB SI, -33
(define r0_13 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 23 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035076 (bitvector 64))))	; PC

; Instruction: AND RAX, 0b1111111111111
(define r0_14 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035082 (bitvector 64))))	; PC

; Instruction: ADC [R14 + RAX], 99
(define r0_15 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035088 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.3
(define r0_16 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035090 (bitvector 64))))	; PC

; Instruction: MUL AL
(define r0_17 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440490 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 2055 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035092 (bitvector 64))))	; PC

; Instruction: ADD DL, DIL
(define r0_18 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 130 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035095 (bitvector 64))))	; PC

; Instruction: AND RSI, 0b1111111111111
(define r0_19 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035102 (bitvector 64))))	; PC

; Instruction: LOCK ADD [R14 + RSI], RSI
(define r0_20 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 134 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035107 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.4
(define r0_21 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 134 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035109 (bitvector 64))))	; PC

; Instruction: ADD SIL, -46
(define r0_22 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2055 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035113 (bitvector 64))))	; PC

; Instruction: AND RDI, 0b1111111111111
(define r0_23 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035120 (bitvector 64))))	; PC

; Instruction: SBB CX, [R14 + RDI]
(define r0_24 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763961 (bitvector 64))	; Register: RBX
                    (bv 1812476199333 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035125 (bitvector 64))))	; PC

; Instruction: DEC RBX
(define r0_25 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763960 (bitvector 64))	; Register: RBX
                    (bv 1812476199333 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035128 (bitvector 64))))	; PC

; Instruction: UNMAPPED 
(define r0_26 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1344324763960 (bitvector 64))	; Register: RBX
                    (bv 1812476199333 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 730144440511 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv -1 (bitvector 64))))	; Final state

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9 r0_10 r0_11 r0_12 r0_13 r0_14 r0_15 r0_16 r0_17 r0_18 r0_19 r0_20 r0_21 r0_22 r0_23 r0_24 r0_25 r0_26))

; Instruction: UNMAPPED 
(define r1_0 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035008 (bitvector 64))))	; PC

; Instruction: UNMAPPED 
(define r1_1 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035011 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.0
(define r1_2 (list	 ;Registers.
		  (bv 597000454283 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 210 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035013 (bitvector 64))))	; PC

; Instruction: ADD RAX, -274095631
(define r1_3 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 3 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035019 (bitvector 64))))	; PC

; Instruction: CMP AL, CL
(define r1_4 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245548833 (bitvector 64))	; Register: RSI
                   (bv 2179 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035021 (bitvector 64))))	; PC

; Instruction: SBB SI, DI
(define r1_5 (list	 ;Registers.
		  (bv 596726358652 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 151 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035024 (bitvector 64))))	; PC

; Instruction: INC AL
(define r1_6 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035026 (bitvector 64))))	; PC

; Instruction: JS .bb_main.1
(define r1_7 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035028 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.2
(define r1_8 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245613966 (bitvector 64))	; Register: RSI
                   (bv 7 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035053 (bitvector 64))))	; PC

; Instruction: ADD SIL, -5
(define r1_9 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                   (bv 1988569858511 (bitvector 64))	; Register: RBX
                   (bv 1812476199334 (bitvector 64))	; Register: RCX
                   (bv 1726576853394 (bitvector 64))	; Register: RDI
                   (bv 459561500779 (bitvector 64))	; Register: RDX
                   (bv 1241245613961 (bitvector 64))	; Register: RSI
                   (bv 147 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035057 (bitvector 64))))	; PC

; Instruction: AND RSI, 0b1111111111111
(define r1_10 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576853394 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035064 (bitvector 64))))	; PC

; Instruction: SBB [R14 + RSI], AX
(define r1_11 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576853394 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 19 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035069 (bitvector 64))))	; PC

; Instruction: SUB DI, AX
(define r1_12 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8073 (bitvector 64))	; Register: RSI
                    (bv 19 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035072 (bitvector 64))))	; PC

; Instruction: SBB SI, -33
(define r1_13 (list	 ;Registers.
		  (bv 596726358653 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 23 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035076 (bitvector 64))))	; PC

; Instruction: AND RAX, 0b1111111111111
(define r1_14 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035082 (bitvector 64))))	; PC

; Instruction: ADC [R14 + RAX], 99
(define r1_15 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035088 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.3
(define r1_16 (list	 ;Registers.
		  (bv 637 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035090 (bitvector 64))))	; PC

; Instruction: MUL AL
(define r1_17 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500779 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 2055 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035092 (bitvector 64))))	; PC

; Instruction: ADD DL, DIL
(define r1_18 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 2194 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035095 (bitvector 64))))	; PC

; Instruction: AND RSI, 0b1111111111111
(define r1_19 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035102 (bitvector 64))))	; PC

; Instruction: LOCK ADD [R14 + RSI], RSI
(define r1_20 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035107 (bitvector 64))))	; PC

; Instruction: JMP .bb_main.4
(define r1_21 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8105 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035109 (bitvector 64))))	; PC

; Instruction: ADD SIL, -46
(define r1_22 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 1726576877333 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2055 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035113 (bitvector 64))))	; PC

; Instruction: AND RDI, 0b1111111111111
(define r1_23 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035120 (bitvector 64))))	; PC

; Instruction: SBB CX, [R14 + RDI]
(define r1_24 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858511 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 6 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035125 (bitvector 64))))	; PC

; Instruction: DEC RBX
(define r1_25 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858510 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv 18446612985909035128 (bitvector 64))))	; PC

; Instruction: UNMAPPED 
(define r1_26 (list	 ;Registers.
		  (bv 15625 (bitvector 64))	; Register: RAX
                    (bv 1988569858510 (bitvector 64))	; Register: RBX
                    (bv 1812476199334 (bitvector 64))	; Register: RCX
                    (bv 7957 (bitvector 64))	; Register: RDI
                    (bv 459561500800 (bitvector 64))	; Register: RDX
                    (bv 8059 (bitvector 64))	; Register: RSI
                    (bv 2 (bitvector 64))	; Register: EFLAGS
                    (bv -1 (bitvector 64))))	; Final state

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9 r1_10 r1_11 r1_12 r1_13 r1_14 r1_15 r1_16 r1_17 r1_18 r1_19 r1_20 r1_21 r1_22 r1_23 r1_24 r1_25 r1_26))

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
                               (diff 27 26 r0 27 26 r1 myexpr)
))))

(print-forms sol)