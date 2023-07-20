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
; (struct OPERAND (bs) #:transparent)
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
; (IF (OPCODE #bxxxxxxx110 (ADDR 0x3C7E78F365E))
;     (OPERANDS #b00001001 (REG 5678)))
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                ; (OPCODE (bs))
                ; (INSTR (bs) (OPERANDS))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              ; (ADDR (?? integer?))
              ; (OPERANDS (?? integer?) (?? integer?)) ; Moving OPERANDS to part of INSTR.
              (REG (?? integer?))
              INSTR
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
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]))
            ; [(OPCODE bs) (eval-opcode bs xstate)]
            ; [(INSTR opcode ops) (eval-instr opcode ops xstate)]))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            [INSTR (eval-reg PC xstate)]
            ; [(OPERANDS op1 op2) (eval-operands op1 op2 xstate)]
            ; [(OPERAND op) (eval-operand op xstate)]
            ; [_ (log-error "Invalid expression for bitstring observation")]
            ))

(define (eval-opcode opcode xstate)
  (log-debug "[eval-opcode] TODO"))

; TODO: update this with our desired constraints for instruction operands.
(define (eval-operands op1 op2 xstate)
  (log-debug "[eval-operands] TODO"))
  ; (list-ref xstate op2)
  ; (list-ref xstate op1))
  ; (list ((list-ref xstate op2)
  ;        (list-ref xstate op1))))

; (define (eval-operand op xstate)
;   (log-info "[eval-operand]")
;   (match op
;     [(INTEGER value) value]
;     [(REG reg) (extract-integer (list-ref xstate reg))]
;     ))

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

; (define (instr-equal xstate1 xstate2)
;   ; TODO: do we care about equality, or value presence in regs?
;   (and (equal? (INSTR-opcode xstate1) (INSTR-opcode xstate2))
;        (equal? (OPERANDS-op1 (INSTR-operands xstate1)) (OPERANDS-op1 (INSTR-operands xstate2)))
;        (equal? (OPERANDS-op2 (INSTR-operands xstate1)) (OPERANDS-op2 (INSTR-operands xstate2)))))
    
; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
(define (obs-equal expr xstate1 xstate2)
  ; (log-debug "[obs-equal]")
  ; (log-debug (listbv-equal (obs expr xstate1) (obs expr xstate2)))
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

; Take our grammar expression and archstate as input.
; Return a list of the operands for the run step.
(define (obs-opcode xstate)
  (log-debug "[obs-opcode] TODO"))
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
      (if (equal? i_ j_) #f
                         (or (not (empty-obs expr (list-ref r_ i_)))
                             (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_) (or (not (empty-obs expr (list-ref r i)))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (list-ref r i))
                                  (diff (+ i 1) j r i_ j_ r_ expr))
                             (and (empty-obs expr (list-ref r_ i_))
                                  (diff i j r (+ i_ 1) j_ r_ expr))
                            ;  (not (obs-equal expr (run-step-instruction (list-ref r i)) (run-step-instruction (list-ref r_ i_))))
                             (and (not (empty-obs expr (list-ref r i)))
                                  (not (empty-obs expr (list-ref r_ i_)))
                                  (not (obs-equal expr (list-ref r i) (list-ref r_ i_))))))))

; ------------- END-CORE ------------------ ;
; Instruction: UNMAPPED 
(define r0_0 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035008 (bitvector 64))	; PC
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

; Instruction: UNMAPPED 
(define r0_1 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035011 (bitvector 64))	; PC
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

; Instruction: JMP .bb_main.0
(define r0_2 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035013 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869456 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422192 (bitvector 64))
))

; Instruction: IMUL RBX, RDI, 106
(define r0_3 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 6 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035017 (bitvector 64))	; PC
                   ; Instruction
                   (bv 1229804876 (bitvector 64))
                   ; Operands
                   (bv 5390936 (bitvector 64))
                   (bv 5391433 (bitvector 64))
                   (bv 3223606 (bitvector 64))
))

; Instruction: ADD EAX, 151399075
(define r0_4 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 22 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035022 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4277316 (bitvector 64))
                   ; Operands
                   (bv 4538712 (bitvector 64))
                   (bv 907723360690502645557 (bitvector 64))
))

; Instruction: AND RDX, 0b1111111111111
(define r0_5 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 2 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035029 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4279876 (bitvector 64))
                   ; Operands
                   (bv 5391448 (bitvector 64))
                   (bv 251221822755027938623366628606292273 (bitvector 64))
))

; Instruction: ADD BX, [R14 + RDX]
(define r0_6 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035034 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4277316 (bitvector 64))
                   ; Operands
                   (bv 16984 (bitvector 64))
                   (bv 1516178508220428600408 (bitvector 64))
))

; Instruction: JNS .bb_main.1
(define r0_7 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035036 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869715 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422193 (bitvector 64))
))

; Instruction: JMP .bb_main.4
(define r0_8 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035128 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869456 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422196 (bitvector 64))
))

; Instruction: UNMAPPED 
(define r0_9 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 833223655618 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 562640715907 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv -1 (bitvector 64))	; Final state
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9))

; Instruction: UNMAPPED 
(define r1_0 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035008 (bitvector 64))	; PC
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

; Instruction: UNMAPPED 
(define r1_1 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035011 (bitvector 64))	; PC
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

; Instruction: JMP .bb_main.0
(define r1_2 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 528280977531 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 150 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035013 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869456 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422192 (bitvector 64))
))

; Instruction: IMUL RBX, RDI, 106
(define r1_3 (list	 ;Registers
		  (bv 1430224109901 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 6 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035017 (bitvector 64))	; PC
                   ; Instruction
                   (bv 1229804876 (bitvector 64))
                   ; Operands
                   (bv 5390936 (bitvector 64))
                   (bv 5391433 (bitvector 64))
                   (bv 3223606 (bitvector 64))
))

; Instruction: ADD EAX, 151399075
(define r1_4 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 794568949945 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 22 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035022 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4277316 (bitvector 64))
                   ; Operands
                   (bv 4538712 (bitvector 64))
                   (bv 907723360690502645557 (bitvector 64))
))

; Instruction: AND RDX, 0b1111111111111
(define r1_5 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584772 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 2 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035029 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4279876 (bitvector 64))
                   ; Operands
                   (bv 5391448 (bitvector 64))
                   (bv 251221822755027938623366628606292273 (bitvector 64))
))

; Instruction: ADD BX, [R14 + RDX]
(define r1_6 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035034 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4277316 (bitvector 64))
                   ; Operands
                   (bv 16984 (bitvector 64))
                   (bv 1516178508220428600408 (bitvector 64))
))

; Instruction: JNS .bb_main.1
(define r1_7 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035036 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869715 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422193 (bitvector 64))
))

; Instruction: JMP .bb_main.4
(define r1_8 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv 18446612985909035128 (bitvector 64))	; PC
                   ; Instruction
                   (bv 4869456 (bitvector 64))
                   ; Operands
                   (bv 219043727635830868422196 (bitvector 64))
))

; Instruction: UNMAPPED 
(define r1_9 (list	 ;Registers
		  (bv 151399408 (bitvector 64))	; Register: RAX
                   (bv 157522220584773 (bitvector 64))	; Register: RBX
                   (bv 545460846719 (bitvector 64))	; Register: RCX
                   (bv 1486058684762 (bitvector 64))	; Register: RDI
                   (bv 185 (bitvector 64))	; Register: RDX
                   (bv 1610612736375 (bitvector 64))	; Register: RSI
                   (bv 130 (bitvector 64))	; Register: EFLAGS
                   (bv -1 (bitvector 64))	; Final state
                   ; Instruction
                   (bv 6146935484322956612 (bitvector 64))
                   ; Operands
                   ))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9))

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
                               (diff 10 9 r0 10 9 r1 myexpr)
))))

(print-forms sol)