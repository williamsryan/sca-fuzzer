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
(struct OPCODE (op) #:transparent)
(struct OPERAND (bs) #:transparent)
(struct OPERAND-TYPE (bs) #:transparent)
(struct OPERAND-VALUE (bs) #:transparent)
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
(struct ADDR (a) #:transparent)         ; Address value.

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
(struct run-step (regs opcode operands)
  #:constructor-name make-run-step
  #:transparent)

(define (run-step-reg-index step)
  (cdr step))

; Grammar for the actual contract.
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                ; (OPERAND-TYPE ...)
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              ; (OPCODE (?? (bitvector 16)))  ; TODO: update the structure to expect a concrete value for OPCODE.
              ; (OPCODE (?? integer?))
              ; (REG (?? integer?))
              (REG (list (?? integer?) (?? integer?))) ; Register followed by optional OPCODE. Here or as predicate better?
              )])

(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval expr xstate)
  ; (log-debug "[eval]")
  ; (log-debug expr)
  (destruct expr
    [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]
    [_ EMPTY]))

; (define (eval expr xstate)
;   ; (log-debug "[eval]")
;   ; (log-debug expr)
;   (destruct expr
;     [(IF pred bs)
;      (if (eval-pred pred xstate)
;          (cons (eval-bs bs xstate) ; Use cons to add the result of eval-bs as the first element
;                (eval-opcode bs xstate)) ; eval-opcode may return an empty list or a list with one element
;          EMPTY)]
;     [_ EMPTY]))

; Evaluation function for predicates.
(define (eval-pred p x)
  ; (log-debug "[eval-pred]")
  ; (log-debug pred)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p x))]
            [(AND p1 p2) (and (eval-pred p1 x) (eval-pred p2 x))]
            [(OR p1 p2) (or (eval-pred p1 x) (eval-pred p2 x))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 x) (eval-bs bs2 x))]))
            ; [bs (log-error "Got an unknown pred") (log-error bs) #f]))
            ; [(INSTR opcode ops) (eval-instr opcode ops xstate)]))

; Evaluation function for bit sequences.
(define (eval-bs bs x)
  ; (log-debug "[eval-bs]")
  ; (log-debug (type-of bs))
  (match bs ; destruct doesn't work for nested subpatterns. Changed to match instead.
            [(BS b) b]
            ; [(OPCODE (bv value (bitvector _))) (eval-opcode value x)]
            [(OPCODE op) (eval-opcode op x)]
            [(REG (list reg op)) (eval-reg (list reg op) x)]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b x))]
            ; [INSTR (eval-reg PC x)]
            [_ (log-error "Invalid expression for bitstring observation") #f]
            ))

(define (eval-opcode opcode xstate)
  (log-debug "[eval-opcode]")
  ; (log-debug (list-ref xstate 8))
  ; (list-ref xstate 8))
  (match (list-ref xstate 8)
    [(list 'OPCODE (bv value (bitvector 16))) value]
    [_ #f ]));(log-error "Invalid opcode") #f]))

; Evaluation function for registers.
(define (eval-reg reg xstate)
  ; (log-debug "[eval-reg]")
  ; (log-debug reg)
  ; (cond
  ;   [(integer? reg) (list-ref xstate reg)]
  ;   [else (log-error "Invalid register format") #f]))

  ; TODO: also return the OPCODE corresponding to this register state.
  (cond
    [(integer? reg) (list (list-ref xstate reg))]
    [(list? reg) ; Handle the list (REG, OPCODE)
     (let ([reg-value (list-ref xstate (first reg))]
           [opcode-value (eval-opcode (second reg) xstate)])
       (list reg-value opcode-value))]
    [else (log-error "Invalid register format") #f]))

; TODO: update this with our desired constraints for instruction operands.
; (define (eval-operands op1 op2 xstate)
;   (log-debug "[eval-operands] TODO"))
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
; (define (eval-instr opcode op1 op2 xstate)
;   (log-info "[eval-instr]")
;   (let* ((opcode-value (eval-bs opcode xstate))
;          (op1-value (eval-bs op1 xstate))
;          (op2-value (eval-bs op2 xstate)))
;     (cond
;       ((eq? opcode-value 'memory-store) op1-value)  ; Update instruction opcode later.
;       ((eq? opcode-value 'memory-load) op2-value)   ; Update instruction opcode later.
;       (else
;         (log-error "Invalid opcode value")
;   ))))

; Evaluation function for addresses.
; (define (eval-addr addr xstate)
;   (list-ref xstate addr))
  ; (eval-bs addr xstate))
  ; (destruct addr
  ;   [(MEM-LOAD a) (eval-bs a xstate)]
  ;   [(MEM-STORE a _) (eval-bs a xstate)]))

; obs() takes an expression and a xstate
;       returns its observation
(define (obs expr xstate)
  ; (log-debug "[obs]")
  ; (log-debug (eval expr xstate))
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
  ; (log-debug expr)
  ; (log-debug xstate1)
  ; (log-debug xstate2)
  ; (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  (equal? (obs expr xstate1) (obs expr xstate2)))

; listbv-equal() takes two observations
;                returns true if they are the same
;                        false otherwise
(define (listbv-equal bvs1 bvs2)
  ; (log-debug "[listbv-equal]")
  ; (log-debug bvs1)
  ; (log-debug bvs2)
  (if (empty? bvs1)
      (if (empty? bvs2) #t #f)
      (if (empty? bvs2)
          #f
          (and (bveq (first bvs1) (first bvs2)) (listbv-equal (rest bvs1) (rest bvs2))))))

; Helper function used in our equality check now that the structs are all labeled.
; (define (compare-objects obj1 obj2)
;   (or (bitvector? obj1)
;       (bitvector? obj2)
;       (and (REG? obj1) (REG? obj2) (equal? (REG-r obj1) (REG-r obj2)))
;       (and (OPCODE? obj1) (OPCODE? obj2) (equal? (OPCODE-op obj1) (OPCODE-op obj2)))))
      ; (and (OPERAND? obj1) (OPERAND? obj2) (equal? (OPERAND-bv obj1) (OPERAND-bv obj2)))))

; Take our grammar expression and archstate as input.
; Return a list of the operands for the run step.
; (define (obs-opcode xstate)
;   (log-debug "[obs-opcode] TODO"))
  ; (match xstate
  ;   [(OPCODE opcode operands)
  ;    (list opcode operands)]
  ;   [_
  ;    (println "Invalid expression for opcode observation")]))

; Extract the list of register values from the xstate.
(define (parse-state xstate)
  ; (log-debug "[parse-state]")
  (define (process-item item)
    (match item
      [(REG reg) (list 'REG reg)]
      [(OPCODE opcode) (list 'OPCODE opcode)]
      [(OPERAND operand) (list 'OPERAND operand)]
      [_ (log-error "Invalid state object") #f]))

  (map process-item xstate))

; diff() takes the following arguments:
;              i,j,i_,j_  : natural numbers such that i <= j and i_ <= j_
;              r, r_      : two run objects
;              expr       : our grammar expression
;
;        returns true if the trace produced by r[i]->r[j] and r_[i_]->r_[j_] are distinguishable
;                false otherwise
(define (diff i j r i_ j_ r_ expr)
  ; (log-debug expr)
  ; (log-debug (parse-state (list-ref r i)))
  (if (equal? i j)
      (if (equal? i_ j_) #f
                         (or (not (empty-obs expr (parse-state (list-ref r_ i_))))
                             (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_) (or (not (empty-obs expr (parse-state (list-ref r i))))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (parse-state (list-ref r i)))
                                  (diff (+ i 1) j r i_ j_ r_ expr))
                             (and (empty-obs expr (parse-state (list-ref r_ i_)))
                                  (diff i j r (+ i_ 1) j_ r_ expr))
                             (and (not (empty-obs expr (parse-state (list-ref r i))))
                                  (not (empty-obs expr (parse-state (list-ref r_ i_))))
                                  (not (obs-equal expr (parse-state (list-ref r i)) (parse-state (list-ref r_ i_)))))))))

; ------------- END-CORE ------------------ ;
; Instruction: ADD RSI, RDX
(define r0_0 (list	 ;Registers
                   (REG (bv 721554522859 (bitvector 64)))	; Register: RAX
                   (REG (bv 455266533482 (bitvector 64)))	; Register: RBX
                   (REG (bv 730144440490 (bitvector 64)))	; Register: RCX
                   (REG (bv 107374182398 (bitvector 64)))	; Register: RDI
                   (REG (bv 940597838044 (bitvector 64)))	; Register: RDX
                   (REG (bv 1971389989324 (bitvector 64)))	; Register: RSI
                   (REG (bv 60 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446612985909035028 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 111 (bitvector 16)))
                   ; Operands
                   (OPERAND (bv 5395273 (bitvector 64)))
                   (OPERAND (bv 5391448 (bitvector 64)))
))

; Instruction: IMUL EBX, EBX
(define r0_1 (list	 ;Registers
                   (REG (bv 721554522859 (bitvector 64)))	; Register: RAX
                   (REG (bv 455266533482 (bitvector 64)))	; Register: RBX
                   (REG (bv 730144440490 (bitvector 64)))	; Register: RCX
                   (REG (bv 107374182398 (bitvector 64)))	; Register: RDI
                   (REG (bv 940597838044 (bitvector 64)))	; Register: RDX
                   (REG (bv 1971389989324 (bitvector 64)))	; Register: RSI
                   (REG (bv 6 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446612985909035028 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 16)))
                   ; Operands
                   (OPERAND (bv 4538968 (bitvector 64)))
                   (OPERAND (bv 4538968 (bitvector 64)))
))

(define r0 (list r0_0 r0_1))

; Instruction: ADD RSI, RDX
(define r1_0 (list	 ;Registers
                   (REG (bv 721554522859 (bitvector 64)))	; Register: RAX
                   (REG (bv 455266533482 (bitvector 64)))	; Register: RBX
                   (REG (bv 730144440490 (bitvector 64)))	; Register: RCX
                   (REG (bv 107374182398 (bitvector 64)))	; Register: RDI
                   (REG (bv 940597838044 (bitvector 64)))	; Register: RDX
                   (REG (bv 1971389989324 (bitvector 64)))	; Register: RSI
                   (REG (bv 6 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446612985909035028 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 111 (bitvector 16)))
                   ; Operands
                   (OPERAND (bv 5395273 (bitvector 64)))
                   (OPERAND (bv 5391448 (bitvector 64)))
))

; Instruction: IMUL EBX, EBX
(define r1_1 (list	 ;Registers
                   (REG (bv 721554522859 (bitvector 64)))	; Register: RAX
                   (REG (bv 455266533482 (bitvector 64)))	; Register: RBX
                   (REG (bv 730144440490 (bitvector 64)))	; Register: RCX
                   (REG (bv 107374182398 (bitvector 64)))	; Register: RDI
                   (REG (bv 940597838044 (bitvector 64)))	; Register: RDX
                   (REG (bv 1971389989324 (bitvector 64)))	; Register: RSI
                   (REG (bv 6 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446612985909035028 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 100010101 (bitvector 16)))
                   ; Operands
                   (OPERAND (bv 4538968 (bitvector 64)))
                   (OPERAND (bv 4538968 (bitvector 64)))
))

(define r1 (list r1_0 r1_1))

(define myexpr (cexpr #:depth 1))
; (log-debug myexpr)

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                              ;  (diff 1 2 r0 1 2 r1 myexpr)
))))

(print-forms sol)

; NOTES.

; SUB AX, -23911
; OPCODE --> bitstring representing SUB

; // op1 is AX
; OPERAND1 (bv $AX-ENCODING)
; OPERAND1-VAL (bv Value-of-AX)
; OPERAND1-TYPE (REG)

; // op2 is -23911
; OPERAND2 (bv -23911)
; OPERAND2-VAL (bv  -23911)
; OPERAND2-TYPE (IMMEDIATE)


; #  sub rsp,200h
; #     OpCode: o64 81 /5 id
; #     Instruction: SUB r/m64, imm32
; #     Encoding: LEGACY
; #     Mnemonic: SUB
; #     Code: SUB_RM64_IMM32
; #     CpuidFeature: X64
; #     FlowControl: NEXT
; #     Immediate offset = 3, size = 4
; #     RFLAGS Written: OF, SF, ZF, AF, CF, PF
; #     RFLAGS Modified: OF, SF, ZF, AF, CF, PF
; #     Op0Access: READ_WRITE
; #     Op1Access: READ
; #     Op0: R64_OR_MEM
; #     Op1: IMM32SEX64
; #     Used reg: RSP:READ_WRITE
