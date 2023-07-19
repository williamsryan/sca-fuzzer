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

(define r0_0 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035008 (bitvector 64))))


(define r0_1 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035011 (bitvector 64))))


(define r0_2 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035013 (bitvector 64))))


(define r0_3 (list (bv 606415022527428 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035016 (bitvector 64))))


(define r0_4 (list (bv 606415022527428 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035019 (bitvector 64))))


(define r0_5 (list (bv 606415022527428 (bitvector 64))
                   (bv 382 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035026 (bitvector 64))))


(define r0_6 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035031 (bitvector 64))))


(define r0_7 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035033 (bitvector 64))))


(define r0_8 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035037 (bitvector 64))))


(define r0_9 (list (bv 5916986782927203856 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 682899800223 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035040 (bitvector 64))))


(define r0_10 (list (bv 5916986782927217600 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65393 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 2183 (bitvector 64))
                    (bv 18446612985909035043 (bitvector 64))))


(define r0_11 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65393 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446612985909035049 (bitvector 64))))


(define r0_12 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 151 (bitvector 64))
                    (bv 18446612985909035053 (bitvector 64))))


(define r0_13 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035055 (bitvector 64))))


(define r0_14 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035057 (bitvector 64))))


(define r0_15 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035111 (bitvector 64))))


(define r0_16 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 682899800223 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list (make-run-step r0_0 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_1 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_2 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_3 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_4 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_5 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_6 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_7 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_8 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_9 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_10 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_11 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_12 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_13 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_14 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_15 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r0_16 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4))))))


(define r1_0 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035008 (bitvector 64))))


(define r1_1 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035011 (bitvector 64))))


(define r1_2 (list (bv 682899800223 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 1915555414462 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035013 (bitvector 64))))


(define r1_3 (list (bv 606415022527428 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 210453397553 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035016 (bitvector 64))))


(define r1_4 (list (bv 606415022527428 (bitvector 64))
                   (bv 1640677507454 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035019 (bitvector 64))))


(define r1_5 (list (bv 606415022527428 (bitvector 64))
                   (bv 382 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035026 (bitvector 64))))


(define r1_6 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3459204 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 134 (bitvector 64))
                   (bv 18446612985909035031 (bitvector 64))))


(define r1_7 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035033 (bitvector 64))))


(define r1_8 (list (bv 606415022527428 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 70596 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035037 (bitvector 64))))


(define r1_9 (list (bv 5916986782927203856 (bitvector 64))
                   (bv 3582722048 (bitvector 64))
                   (bv 3388608 (bitvector 64))
                   (bv 1249835483427 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 1906965479868 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035040 (bitvector 64))))


(define r1_10 (list (bv 5916986782927217600 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65393 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 2183 (bitvector 64))
                    (bv 18446612985909035043 (bitvector 64))))


(define r1_11 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65393 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 6 (bitvector 64))
                    (bv 18446612985909035049 (bitvector 64))))


(define r1_12 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 151 (bitvector 64))
                    (bv 18446612985909035053 (bitvector 64))))


(define r1_13 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035055 (bitvector 64))))


(define r1_14 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035057 (bitvector 64))))


(define r1_15 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv 18446612985909035111 (bitvector 64))))


(define r1_16 (list (bv 960 (bitvector 64))
                    (bv 3582722048 (bitvector 64))
                    (bv 3388608 (bitvector 64))
                    (bv 1249835483427 (bitvector 64))
                    (bv 65530 (bitvector 64))
                    (bv 1906965479868 (bitvector 64))
                    (bv 22 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list (make-run-step r1_0 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_1 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_2 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_3 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_4 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_5 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_6 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_7 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_8 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_9 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_10 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_11 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_12 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_13 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_14 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_15 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4)))) (make-run-step r1_16 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4))) (bv #b0111 (bitvector 4))))))

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
                               (diff 17 16 r0 17 16 r1 myexpr)
))))

(print-forms sol)