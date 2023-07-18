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
(struct OPERANDS (op1 op2) #:transparent)
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
(struct run-step (regs opcode operands)
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
                ; (INSTR (name))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (REG (?? integer?))
              (ADDR (?? integer?))
              (OPERANDS (?? integer?) (?? integer?)) ; integer or bv int?
              )]
  )

(define EMPTY (list '()))

; Evaluation function for expressions.
(define (eval e xstate)
  (destruct e
    [(IF pred bs) (if (eval-pred pred xstate) (list (eval-bs bs xstate)) EMPTY)]
    [_ EMPTY]))

; Evaluation function for predicates.
(define (eval-pred p xstate)
  (destruct p
            [(BOOL b) b]
            [(NOT some-p) (not (eval-pred some-p xstate))]
            [(AND p1 p2) (and (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(OR p1 p2) (or (eval-pred p1 xstate) (eval-pred p2 xstate))]
            [(EQ bs1 bs2) (bveq (eval-bs bs1 xstate) (eval-bs bs2 xstate))]
            [(OPCODE opcode) (eval-opcode opcode xstate)]))
            ; [(INSTR name operand) (eval-instr name operand xstate)]))

; Format notes:
; (IF (OPCODE #b0000001010) (REG[operand2])) : 
; This clause represents the condition where a memory load opcode (#b0000001010) is encountered,
; and the value stored in the register specified by operand2 is leaked.
;
; (IF (OPCODE #b0000010101) ADDR[operand1]) : 
; This clause represents the condition where a memory store opcode (#b0000010101) is encountered,
; and the address referenced by operand1 is considered leaked based on the leakage-expression function.
(define (eval-opcode opcode xstate)
  (println "TODO"))

; (define (get-opcode bs)
;   (bvextract 0 3 bs))     ; Extract bits 0 to 3 (inclusive) as opcode.

; (define (extract-op1 bs)
;   (extract-bits bs 4 7))  ; Extract bits 4 to 7 (inclusive) as operand1.

; (define (extract-op2 bs)
;   (extract-bits bs 8 11))  ; Extract bits 8 to 11 (inclusive) as operand2.

(define (extract-integer value)
  (match value
    [(bv i _) i]
    [_ (println "Invalid integer value from bitvector")]))

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

(define (eval-operands op1 op2 xstate)
  (println "[eval-operands] TODO"))

; Evaluation function for bit sequences.
(define (eval-bs bs xstate)
  (destruct bs
            [(BS b) b]
            [(SLIDE i1 i2 b) (extract i2 i1 (eval-bs b xstate))]
            [(REG reg) (eval-reg reg xstate)]
            [(OPERANDS op1 op2) (eval-operands op1 op2 xstate)]
            ))

; Evaluation function for instructions.
; (define (eval-instr name operand xstate)
;   (match name
;     ['LOAD
;       (println "Got a 'LOAD; just testing for now.")
;       (eval-reg operand xstate)]
;     ['STORE
;       (println "Got a 'STORE; not yet implemented.")]))

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

; (define (opcode-equal expr xstate1 xstate2)
;   (and (equal? (OPCODE-bs xstate1) (OPCODE-bs xstate2))
;        (equal? (OPCODE-op1 xstate1) (OPCODE-op1 xstate2))
;        (equal? (OPCODE-op2 xstate2) (OPCODE-op2 xstate2))))

(define (opcode-equal opcode1 opcode2)
  (println "TODO"))
  ; (println "[opcode-equal]")
  ; (and (equal? (OPCODE-bs opcode1) (OPCODE-bs opcode2))
  ;      (equal? (OPCODE-operands opcode1) (OPCODE-operands opcode2))))
    
; obs-equal() takes an expression and two xstates
;             returns true if the two xstates produces same observations
;                     false otherwise
; TODO: this is just operating on registers currently, so opcode-equal is ignored.
(define (obs-equal expr xstate1 xstate2)
  ; (println "[obs-equal]")
  ; (println xstate1)
  ; (println xstate2)
  ; (println (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  ; (listbv-equal (obs expr xstate1) (obs expr xstate2)))
  ; (or (listbv-equal (obs expr xstate1) (obs expr xstate2))
      ; (opcode-equal (obs-opcode xstate1) (obs-opcode xstate2))))

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
  (println "TODO"))
  ; (match xstate
  ;   [(OPCODE opcode operands)
  ;    (list opcode operands)]
  ;   [_
  ;    (println "Invalid expression for opcode observation")]))
          

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
          (or (not (empty-obs expr (run-step-regs (list-ref r_ i_)))
              (diff j j r (+ i_ 1) j_ r_ expr))))
      (if (equal? i_ j_)
          (or (not (empty-obs expr (run-step-regs (list-ref r i)))
              (diff (+ i 1) j r j_ j_ r_ expr)))
          (or (and (empty-obs expr (run-step-regs (list-ref r i)))
                  (diff (+ i 1) j r i_ j_ r_ expr))
              (and (empty-obs expr (run-step-regs (list-ref r_ i_)))
                  (diff i j r (+ i_ 1) j_ r_ expr))
              (and (not (obs-equal expr (run-step-operands (list-ref r i)) (run-step-operands (list-ref r_ i_))))) ; Checking if second oeprand for instruciton at each step is equal between runs. If not, operand value is leaked.)
              (and (not (empty-obs expr (run-step-regs (list-ref r i))))
                   (not (empty-obs expr (run-step-regs (list-ref r_ i_))))
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

(define r0 (list (make-run-step r0_0 (OPCODE (bv #b0000001011 (bitvector 8)))
                                     (list (bv #b0111 (bitvector 4)) (bv #b1100 (bitvector 4))))
                 (make-run-step r0_1 (OPCODE (bv #b0000001010 (bitvector 8)))
                                     (list (bv #b0110 (bitvector 4)) (bv #b1101 (bitvector 4))))))

; Register state @ instruction: PLACEHOLDER
(define r1_0 (list (bv 176093659177 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439808 (bitvector 64))))

; Register state @ instruction: PLACEHOLDER
(define r1_1 (list (bv 176093659177 (bitvector 64))
                   (bv 1662152343939 (bitvector 64))
                   (bv 1524713390435 (bitvector 64))
                   (bv 176093659177 (bitvector 64))
                   (bv 1984274891214 (bitvector 64))
                   (bv 335007449166 (bitvector 64))
                   (bv 66 (bitvector 64))
                   (bv 18446630612648439811 (bitvector 64))))

(define r1 (list (make-run-step r1_0 (OPCODE (bv #b0000001011 (bitvector 8)))
                                     (list (bv #b0111 (bitvector 4)) (bv #b1100 (bitvector 4))))
                 (make-run-step r1_1 (OPCODE (bv #b0000001000 (bitvector 8)))
                                     (list (bv #b1000 (bitvector 4)) (bv #b0011 (bitvector 4))))))

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert (or (diff 0 1 r0 0 1 r1 myexpr)
                               (diff 1 2 r0 1 2 r1 myexpr)
))))

(print-forms sol)

; (diff 0 1 r0 0 1 r1 myexpr)
; (diff 1 2 r0 1 2 r1 myexpr)
