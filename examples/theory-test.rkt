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
(struct OPERAND (bs) #:transparent);
; OPERAND1-VAL
; operand-type
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

; New object that holds register values and instruction together.
; (struct run-step (regs opcode operands)
;   #:constructor-name make-run-step
;   #:transparent)

; (define (run-step-reg-index step)
;   (cdr step))

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))               <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))                   <-- Supported (leaked program counter).
; (IF (OPCODE (bs?)) REG[OPERAND2])
; (IF (OPCODE #b0000010011 (REG[op2])))
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                ; (OPCODE (bitvector 64))
                ; OP TYPES.
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              ; (OPCODE ...)
              ; (ADDR (bs))
              ; (ADDR (b1, bs2, bs3))
              ; (OPERANDS (?? integer?) (?? integer?)) ; Moving OPERANDS to part of INSTR.
              (REG (?? integer?)) ; REG[op1]
              ; INSTR
              ; MEM[op2]
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
            [(OPCODE op) (eval-opcode op xstate)]
            [bs (log-error "Got an unknown pred") (log-error bs) #f]))
            ; [(INSTR opcode ops) (eval-instr opcode ops xstate)]))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            ; [(OPCODE op) (log-debug "Got an opcode") (eval-opcode op xstate)]
            [INSTR (eval-reg PC xstate)]
            ; [(OPERANDS op1 op2) (eval-operands op1 op2 xstate)]
            ; [(OPERAND op) (eval-operand op xstate)]
            ; [_ (log-error "Invalid expression for bitstring observation")]
            ))

(define (eval-opcode opcode xstate)
  ; (log-debug "[eval-opcode]")
  ; (log-debug opcode)
  (match opcode
    [op op]
    [_ (log-error "Invalid opcode") #f]))
  ; (define opcode-value (match opcode
  ;                       [bv bv]
  ;                       [_ (log-error "Invalid opcode")]))
  ; (define pc-value (match (list-ref xstate 7)
  ;                       [(REG bv) bv]
  ;                       [_ (log-error "Invalid PC")]))

  ; (log-debug opcode-value)
  ; (bveq opcode-value pc-value))

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

; (define (instr-equal xstate1 xstate2)
;   ; TODO: do we care about equality, or value presence in regs?
;   (and (equal? (INSTR-opcode xstate1) (INSTR-opcode xstate2))
;        (equal? (OPERANDS-op1 (INSTR-operands xstate1)) (OPERANDS-op1 (INSTR-operands xstate2)))
;        (equal? (OPERANDS-op2 (INSTR-operands xstate1)) (OPERANDS-op2 (INSTR-operands xstate2)))))
    
; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
(define (obs-equal expr xstate1 xstate2)
  ; (log-debug "[obs-equal] ...")
  ; (log-debug xstate1)
  ; (log-debug xstate2)
  ; (log-debug (listbv-equal (obs expr xstate1) (obs expr xstate2)))
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

; Take our grammar expression and archstate as input.
; Return a list of the operands for the run step.
(define (obs-opcode xstate)
  (log-debug "[obs-opcode] TODO"))
  ; (match xstate
  ;   [(OPCODE opcode operands)
  ;    (list opcode operands)]
  ;   [_
  ;    (println "Invalid expression for opcode observation")]))

(define (create-pred opcode-val reg-val)
  (log-debug "[create-pred] TODO")
  (list 'IF
        (list 'OPCODE opcode-val)
        (list 'REG reg-val)))

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
      [(OPCODE opcode) (eval-opcode (list 'OPCODE opcode) xstate)]  ; Use OPCODE label for consistency.
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
  ; (log-debug expr)
  ; (log-debug (get-structs (list-ref r i)))
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
; Instruction: UNMAPPED 
(define r0_0 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230479 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973972 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 130 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381312 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 110010110 (bitvector 64)))
                   ; Operands
                   ))

; Instruction: UNMAPPED 
(define r0_1 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230479 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973972 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 130 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381315 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 110010110 (bitvector 64)))
                   ; Operands
                   ))

; Instruction: JMP .bb_main.0
(define r0_2 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230479 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973972 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 130 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381317 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100110100 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 219043727635830868422192 (bitvector 64)))
))

; Instruction: CMP BL, -13
(define r0_3 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230479 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973972 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 3 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381320 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1011101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 16972 (bitvector 64)))
                   (OPERAND (bv 2961715 (bitvector 64)))
))

; Instruction: INC DI
(define r0_4 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230479 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 3 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381323 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010111 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 17481 (bitvector 64)))
))

; Instruction: SBB RCX, -43
(define r0_5 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230521 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381327 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1010101001 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 5391192 (bitvector 64)))
                   (OPERAND (bv 2962483 (bitvector 64)))
))

; Instruction: ADC CL, DIL
(define r0_6 (list	 ;Registers
                   (REG (bv 1035087118577 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230287 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381330 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 17228 (bitvector 64)))
                   (OPERAND (bv 4475212 (bitvector 64)))
))

; Instruction: SUB AX, -23911
(define r0_7 (list	 ;Registers
                   (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                   (REG (bv 1206885810457 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230287 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 19 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381334 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1011100100 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 16728 (bitvector 64)))
                   (OPERAND (bv 49693631000881 (bitvector 64)))
))

; Instruction: IMUL EBX, EDX, 57
(define r0_8 (list	 ;Registers
                   (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                   (REG (bv 22116 (bitvector 64)))	; Register: RBX
                   (REG (bv 889058230287 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 2 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381337 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 4538968 (bitvector 64)))
                   (OPERAND (bv 4539480 (bitvector 64)))
                   (OPERAND (bv 13623 (bitvector 64)))
))

; Instruction: IMUL ECX, ECX, 52
(define r0_9 (list	 ;Registers
                   (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                   (REG (bv 22116 (bitvector 64)))	; Register: RBX
                   (REG (bv 780 (bitvector 64)))	; Register: RCX
                   (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                   (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 6 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381340 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 4539224 (bitvector 64)))
                   (OPERAND (bv 4539224 (bitvector 64)))
                   (OPERAND (bv 13618 (bitvector 64)))
))

; Instruction: AND RSI, 0b1111111111111
(define r0_10 (list	 ;Registers
                    (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                    (REG (bv 22116 (bitvector 64)))	; Register: RBX
                    (REG (bv 780 (bitvector 64)))	; Register: RCX
                    (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                    (REG (bv 1666447311236 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381347 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 10101 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 5395273 (bitvector 64)))
                    (OPERAND (bv 251221822755027938623366628606292273 (bitvector 64)))
))

; Instruction: ADD DL, [R14 + RSI]
(define r0_11 (list	 ;Registers
                    (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                    (REG (bv 22116 (bitvector 64)))	; Register: RBX
                    (REG (bv 780 (bitvector 64)))	; Register: RCX
                    (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                    (REG (bv 1666447311128 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2055 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381351 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 111 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 17484 (bitvector 64)))
                    (OPERAND (bv 1516178508220428604233 (bitvector 64)))
))

; Instruction: JNBE .bb_main.1
(define r0_12 (list	 ;Registers
                    (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                    (REG (bv 22116 (bitvector 64)))	; Register: RBX
                    (REG (bv 780 (bitvector 64)))	; Register: RCX
                    (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                    (REG (bv 1666447311128 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2055 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381353 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 100101001 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 219043727635830868422193 (bitvector 64)))
))

; Instruction: JMP .bb_main.exit
(define r0_13 (list	 ;Registers
                    (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                    (REG (bv 22116 (bitvector 64)))	; Register: RBX
                    (REG (bv 780 (bitvector 64)))	; Register: RCX
                    (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                    (REG (bv 1666447311128 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2055 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381405 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 100110100 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 3674943931991503818987591461236 (bitvector 64)))
))

; Instruction: UNMAPPED 
(define r0_14 (list	 ;Registers
                    (REG (bv 1035087142488 (bitvector 64)))	; Register: RAX
                    (REG (bv 22116 (bitvector 64)))	; Register: RBX
                    (REG (bv 780 (bitvector 64)))	; Register: RCX
                    (REG (bv 1185410973973 (bitvector 64)))	; Register: RDI
                    (REG (bv 1666447311128 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2055 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv -1 (bitvector 64)))	; Final state
                    ; Opcode
                    (OPCODE (bv 110010110 (bitvector 64)))
                    ; Operands
                    ))

(define r0 (list r0_0 r0_1 r0_2 r0_3 r0_4 r0_5 r0_6 r0_7 r0_8 r0_9 r0_10 r0_11 r0_12 r0_13 r0_14))

; Instruction: UNMAPPED 
(define r1_0 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271858 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689879 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 70 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381312 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 110010110 (bitvector 64)))
                   ; Operands
                   ))

; Instruction: UNMAPPED 
(define r1_1 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271858 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689879 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 70 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381315 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 110010110 (bitvector 64)))
                   ; Operands
                   ))

; Instruction: JMP .bb_main.0
(define r1_2 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271858 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689879 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 70 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381317 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100110100 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 219043727635830868422192 (bitvector 64)))
))

; Instruction: CMP BL, -13
(define r1_3 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271858 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689879 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 131 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381320 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1011101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 16972 (bitvector 64)))
                   (OPERAND (bv 2961715 (bitvector 64)))
))

; Instruction: INC DI
(define r1_4 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271858 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 3 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381323 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010111 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 17481 (bitvector 64)))
))

; Instruction: SBB RCX, -43
(define r1_5 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271900 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 23 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381327 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1010101001 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 5391192 (bitvector 64)))
                   (OPERAND (bv 2962483 (bitvector 64)))
))

; Instruction: ADC CL, DIL
(define r1_6 (list	 ;Registers
                   (REG (bv 1060856922359 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271797 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 2071 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381330 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 17228 (bitvector 64)))
                   (OPERAND (bv 4475212 (bitvector 64)))
))

; Instruction: SUB AX, -23911
(define r1_7 (list	 ;Registers
                   (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                   (REG (bv 592705486986 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271797 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 19 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381334 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 1011100100 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 16728 (bitvector 64)))
                   (OPERAND (bv 49693631000881 (bitvector 64)))
))

; Instruction: IMUL EBX, EDX, 57
(define r1_8 (list	 ;Registers
                   (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                   (REG (bv 21033 (bitvector 64)))	; Register: RBX
                   (REG (bv 489626271797 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 2 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381337 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 4538968 (bitvector 64)))
                   (OPERAND (bv 4539480 (bitvector 64)))
                   (OPERAND (bv 13623 (bitvector 64)))
))

; Instruction: IMUL ECX, ECX, 52
(define r1_9 (list	 ;Registers
                   (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                   (REG (bv 21033 (bitvector 64)))	; Register: RBX
                   (REG (bv 2756 (bitvector 64)))	; Register: RCX
                   (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                   (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                   (REG (bv 429496729700 (bitvector 64)))	; Register: RSI
                   (REG (bv 2 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446637029482381340 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 4539224 (bitvector 64)))
                   (OPERAND (bv 4539224 (bitvector 64)))
                   (OPERAND (bv 13618 (bitvector 64)))
))

; Instruction: AND RSI, 0b1111111111111
(define r1_10 (list	 ;Registers
                    (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                    (REG (bv 21033 (bitvector 64)))	; Register: RBX
                    (REG (bv 2756 (bitvector 64)))	; Register: RCX
                    (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                    (REG (bv 1584842932593 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 2 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381347 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 10101 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 5395273 (bitvector 64)))
                    (OPERAND (bv 251221822755027938623366628606292273 (bitvector 64)))
))

; Instruction: ADD DL, [R14 + RSI]
(define r1_11 (list	 ;Registers
                    (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                    (REG (bv 21033 (bitvector 64)))	; Register: RBX
                    (REG (bv 2756 (bitvector 64)))	; Register: RCX
                    (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                    (REG (bv 1584842932555 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381351 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 111 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 17484 (bitvector 64)))
                    (OPERAND (bv 1516178508220428604233 (bitvector 64)))
))

; Instruction: JNBE .bb_main.1
(define r1_12 (list	 ;Registers
                    (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                    (REG (bv 21033 (bitvector 64)))	; Register: RBX
                    (REG (bv 2756 (bitvector 64)))	; Register: RCX
                    (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                    (REG (bv 1584842932555 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381353 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 100101001 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 219043727635830868422193 (bitvector 64)))
))

; Instruction: JMP .bb_main.exit
(define r1_13 (list	 ;Registers
                    (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                    (REG (bv 21033 (bitvector 64)))	; Register: RBX
                    (REG (bv 2756 (bitvector 64)))	; Register: RCX
                    (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                    (REG (bv 1584842932555 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv 18446637029482381405 (bitvector 64)))	; PC
                    ; Opcode
                    (OPCODE (bv 100110100 (bitvector 64)))
                    ; Operands
                    (OPERAND (bv 3674943931991503818987591461236 (bitvector 64)))
))

; Instruction: UNMAPPED 
(define r1_14 (list	 ;Registers
                    (REG (bv 1060856946270 (bitvector 64)))	; Register: RAX
                    (REG (bv 21033 (bitvector 64)))	; Register: RBX
                    (REG (bv 2756 (bitvector 64)))	; Register: RCX
                    (REG (bv 1748051689880 (bitvector 64)))	; Register: RDI
                    (REG (bv 1584842932555 (bitvector 64)))	; Register: RDX
                    (REG (bv 100 (bitvector 64)))	; Register: RSI
                    (REG (bv 7 (bitvector 64)))	; Register: EFLAGS
                    (REG (bv -1 (bitvector 64)))	; Final state
                    ; Opcode
                    (OPCODE (bv 110010110 (bitvector 64)))
                    ; Operands
                    ))

(define r1 (list r1_0 r1_1 r1_2 r1_3 r1_4 r1_5 r1_6 r1_7 r1_8 r1_9 r1_10 r1_11 r1_12 r1_13 r1_14))

(define myexpr (cexpr #:depth 2))

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                               (diff 1 2 r0 1 2 r1 myexpr)
                              ;  (diff 2 2 r0 2 2 r1 myexpr)
                              ;  (diff 2 3 r0 2 3 r1 myexpr)
                              ;  (diff 3 3 r0 3 3 r1 myexpr)
                              ;  (diff 3 4 r0 3 4 r1 myexpr)
                              ;  (diff 4 4 r0 4 4 r1 myexpr)
                              ;  (diff 4 5 r0 4 5 r1 myexpr)
                              ;  (diff 5 5 r0 5 5 r1 myexpr)
                              ;  (diff 5 6 r0 5 6 r1 myexpr)
                              ;  (diff 6 6 r0 6 6 r1 myexpr)
                              ;  (diff 6 7 r0 6 7 r1 myexpr)
                              ;  (diff 7 7 r0 7 7 r1 myexpr)
                              ;  (diff 7 8 r0 7 8 r1 myexpr)
                              ;  (diff 8 8 r0 8 8 r1 myexpr)
                              ;  (diff 8 9 r0 8 9 r1 myexpr)
                              ;  (diff 9 9 r0 9 9 r1 myexpr)
                              ;  (diff 9 10 r0 9 10 r1 myexpr)
                              ;  (diff 10 10 r0 10 10 r1 myexpr)
                              ;  (diff 10 11 r0 10 11 r1 myexpr)
                              ;  (diff 11 11 r0 11 11 r1 myexpr)
                              ;  (diff 11 12 r0 11 12 r1 myexpr)
                              ;  (diff 12 12 r0 12 12 r1 myexpr)
                              ;  (diff 12 13 r0 12 13 r1 myexpr)
                              ;  (diff 13 13 r0 13 13 r1 myexpr)
                              ;  (diff 13 14 r0 13 14 r1 myexpr)
                              ;  (diff 14 14 r0 14 14 r1 myexpr)
                              ;  (diff 14 15 r0 14 15 r1 myexpr)
                              ;  (diff 15 14 r0 15 14 r1 myexpr)
))))

(print-forms sol)
