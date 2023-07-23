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
(struct run-step (regs opcode operands)
  #:constructor-name make-run-step
  #:transparent)

(define (run-step-reg-index step)
  (cdr step))

; Grammar for the actual contract.
; (IF (BOOL #t) (REG 12))               <-- Supported (leaked registers).
; (IF (BOOL #t) (PC))                   <-- Supported (leaked program counter).
; (IF (OPCODE (bs?)) REG[OPERAND2])
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
                (OPCODE (bs)) ; (?? (bitvector (?? integer?))) || BS
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
  ; (log-debug "[eval]")
  (destruct expr
    [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]
    ; [(IF OPCODE bs) (if (eval-opcode bs xstate) (list (eval-bs bs xstate)) EMPTY)]  ; Test this later.
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
            [(OPCODE op) (eval-opcode xstate)]
            [bs (log-error "Got an unknown pred") (log-error bs) #f]))
            ; [(INSTR opcode ops) (eval-instr opcode ops xstate)]))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            ; [(OPCODE op) (eval-opcode op xstate)]
            [INSTR (eval-reg PC xstate)]
            ; [(OPERANDS op1 op2) (eval-operands op1 op2 xstate)]
            ; [(OPERAND op) (eval-operand op xstate)]
            ; [_ (log-error "Invalid expression for bitstring observation")]
            ))

(define (eval-opcode opcode)
  ; (log-debug "[eval-opcode]")
  (match opcode
    [(bv 110 (bitvector 64)) (list 'OPCODE 110)]
    [(bv 111 (bitvector 64)) (list 'OPCODE 111)]
    [(bv 100010101 (bitvector 64)) (list 'OPCODE 100010101)]
    [_ (log-error "Got unknown opcode") #f]))

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

; TODO: test later for parsing the updated struct.
(define (get-structs xstate)
  ; (log-debug "[get-structs]")
  (define (process-item item)
    (match item
      [(REG reg) reg]
      [(OPCODE opcode) (eval-opcode opcode)]  ; Don't check for diff like with registers.
      [(OPERAND operand) #f]                  ; TODO: utilize this later.
      [_ (log-error "Got an unknown structure")]))

  (map process-item xstate))

; diff() takes the following arguments:
;              i,j,i_,j_  : natural numbers such that i <= j and i_ <= j_
;              r, r_      : two run objects
;              expr       : our grammar expression
;
;        returns true if the trace produced by r[i]->r[j] and r_[i_]->r_[j_] are distinguishable
;                false otherwise
(define (diff i j r i_ j_ r_ expr)
  (log-debug (get-structs (list-ref r i)))
  (if (equal? i j)
      (if (equal? i_ j_) #f
                         (or (not (empty-obs expr (get-structs (list-ref r_ i_))))
                             (diff j j r (+ i_ 1) j_ r_ expr)))
      (if (equal? i_ j_) (or (not (empty-obs expr (get-structs (list-ref r i))))
                             (diff (+ i 1) j r j_ j_ r_ expr))
                         (or (and (empty-obs expr (get-structs (list-ref r i)))
                                  (diff (+ i 1) j r i_ j_ r_ expr))
                             (and (empty-obs expr (get-structs (list-ref r_ i_)))
                                  (diff i j r (+ i_ 1) j_ r_ expr))
                            ;  (not (obs-equal expr (run-step-instruction (list-ref r i)) (run-step-instruction (list-ref r_ i_))))
                             (and (not (empty-obs expr (get-structs (list-ref r i))))
                                  (not (empty-obs expr (get-structs (list-ref r_ i_))))
                                  (not (obs-equal expr (get-structs (list-ref r i)) (get-structs (list-ref r_ i_)))))))))

; ------------- END-CORE ------------------ ;
; Instruction: ADD RSI, RDX
(define r0_0 (list	 ;Registers
                   (REG (bv 721554522859 (bitvector 64)))	; Register: RAX
                   (REG (bv 455266533482 (bitvector 64)))	; Register: RBX
                   (REG (bv 730144440491 (bitvector 64)))	; Register: RCX ; FOR TESTING: leak is here.
                   (REG (bv 107374182398 (bitvector 64)))	; Register: RDI
                   (REG (bv 940597838044 (bitvector 64)))	; Register: RDX
                   (REG (bv 1971389989324 (bitvector 64)))	; Register: RSI
                   (REG (bv 6 (bitvector 64)))	; Register: EFLAGS
                   (REG (bv 18446612985909035028 (bitvector 64)))	; PC
                   ; Opcode
                   (OPCODE (bv 110 (bitvector 64)))
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
                   (OPCODE (bv 100010101 (bitvector 64)))
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
                   (OPCODE (bv 111 (bitvector 64)))
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
                   (OPCODE (bv 100010101 (bitvector 64)))
                   ; Operands
                   (OPERAND (bv 4538968 (bitvector 64)))
                   (OPERAND (bv 4538968 (bitvector 64)))
))

(define r1 (list r1_0 r1_1))

(define myexpr (cexpr #:depth 1)) ; Note: at depth 2, eval-opcode starts getting called.

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                               (diff 1 2 r0 1 2 r1 myexpr)
))))

(print-forms sol)


; Dummy tests.
; (define reg-test (cdr (get-regs r0_0)))
; (log-debug "TEST")
; (diff 0 1 r0 0 1 r1 myexpr)
; (diff 1 2 r0 1 2 r1 myexpr)
