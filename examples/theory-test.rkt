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
            [(OPCODE bs) (eval-opcode bs xstate)]))
            ; [(INSTR name operand) (eval-instr name operand xstate)]))

; Format notes:
; (IF (OPCODE #b0000001010) (REG[operand2])) : 
; This clause represents the condition where a memory load opcode (#b0000001010) is encountered,
; and the value stored in the register specified by operand2 is leaked.
;
; (IF (OPCODE #b0000010101) ADDR[operand1]) : 
; This clause represents the condition where a memory store opcode (#b0000010101) is encountered,
; and the address referenced by operand1 is considered leaked based on the leakage-expression function.
(define (eval-opcode bs xstate)
  (let* ((opcode (get-opcode bs))
         (op1 (extract-op1 bs)) ; Address where data is being stored (for a store instr).
         (op2 (extract-op2 bs))) ; Value/data being stored.
    (cond
      ((eq? opcode #b0000001010) ; Example opcode value for memory load.
       ; Retrieve the values of registers or operands based on the opcode.
       ; Perform the evaluation or comparison using the retrieved values.
       ; Return the result of the evaluation.
       (let* ((reg-index (extract-integer op1))
              (register (list-ref xstate reg-index))
              (operand-value (extract-integer op2)))
         (eq? register operand-value))) ; If register value == op2 value -> op2 value is leaked.
      ((eq? opcode #b0000001100) ; Another example opcode value for memory store.
       ; Handle the specific behavior for this opcode value.
       (let* ((addr-index (extract-integer op1))
              (address (list-ref xstate addr-index)))
              #t) ; Just return that the address is leaked. Later we will likely want to constrain this.
       )
      (else
       ; Handle other opcode values, if any.
       #f))))

(define (get-opcode bs)
  (bvextract 0 3 bs))     ; Extract bits 0 to 3 (inclusive) as opcode.

(define (extract-op1 bs)
  (extract-bits bs 4 7))  ; Extract bits 4 to 7 (inclusive) as operand1.

(define (extract-op2 bs)
  (extract-bits bs 8 11))  ; Extract bits 8 to 11 (inclusive) as operand2.

(define (extract-integer value)
  (match value
    [(bv i _) i]
    [_ (println "Invalid value for extracting integer")]))

(define (extract-bits bs start-bit end-bit)
  (bvextract start-bit (+ 1 (- end-bit start-bit)) bs))

(define (bvextract start-bit width bs)
  (bvlshr (bvand bs (bvneg (bvshl (bvneg (bvlshr (bvneg #b0) start-bit)) width))) start-bit))

(define (bv-eq? bv1 bv2)
  (bveq bv1 bv2))

; (define (get-opcode bs)
;   (match bs
;     [(bv #b00000000 _) 'LOAD]
;     [(bv #b00000001 _) 'STORE]
;     ; Add more cases for other opcodes if needed.
;     [_ #f]))

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

(define r0_0 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035008 (bitvector 64))))


(define r0_1 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035011 (bitvector 64))))


(define r0_2 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035013 (bitvector 64))))


(define r0_3 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446612985909035015 (bitvector 64))))


(define r0_4 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035022 (bitvector 64))))


(define r0_5 (list (bv 1610612736375 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035027 (bitvector 64))))


(define r0_6 (list (bv 18446742463096815241 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 147 (bitvector 64))
                   (bv 18446612985909035030 (bitvector 64))))


(define r0_7 (list (bv 18446742463096759633 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035034 (bitvector 64))))


(define r0_8 (list (bv 1361 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446612985909035040 (bitvector 64))))


(define r0_9 (list (bv 1361 (bitvector 64))
                   (bv 2181843386876 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 131 (bitvector 64))
                   (bv 18446612985909035044 (bitvector 64))))


(define r0_10 (list (bv 1361 (bitvector 64))
                    (bv 2181843386876 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446612985909035046 (bitvector 64))))


(define r0_11 (list (bv 1361 (bitvector 64))
                    (bv 2181843386876 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446612985909035097 (bitvector 64))))


(define r0_12 (list (bv 1361 (bitvector 64))
                    (bv 2181843386876 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r0 (list (make-run-step r0_0 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_1 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_2 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_3 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_4 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_5 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_6 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_7 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_8 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_9 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_10 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_11 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r0_12 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4)))))))


(define r1_0 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035008 (bitvector 64))))


(define r1_1 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035011 (bitvector 64))))


(define r1_2 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 1928440316353 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 86 (bitvector 64))
                   (bv 18446612985909035013 (bitvector 64))))


(define r1_3 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 167503724583 (bitvector 64))
                   (bv 22 (bitvector 64))
                   (bv 18446612985909035015 (bitvector 64))))


(define r1_4 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035022 (bitvector 64))))


(define r1_5 (list (bv 1610612736375 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 6 (bitvector 64))
                   (bv 18446612985909035027 (bitvector 64))))


(define r1_6 (list (bv 18446742463096815241 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 147 (bitvector 64))
                   (bv 18446612985909035030 (bitvector 64))))


(define r1_7 (list (bv 18446742463096759633 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 2051 (bitvector 64))
                   (bv 18446612985909035034 (bitvector 64))))


(define r1_8 (list (bv 1361 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 2 (bitvector 64))
                   (bv 18446612985909035040 (bitvector 64))))


(define r1_9 (list (bv 1361 (bitvector 64))
                   (bv 1911260447165 (bitvector 64))
                   (bv 558345748610 (bitvector 64))
                   (bv 319 (bitvector 64))
                   (bv 1872605741492 (bitvector 64))
                   (bv 39 (bitvector 64))
                   (bv 131 (bitvector 64))
                   (bv 18446612985909035044 (bitvector 64))))


(define r1_10 (list (bv 1361 (bitvector 64))
                    (bv 1911260447165 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446612985909035046 (bitvector 64))))


(define r1_11 (list (bv 1361 (bitvector 64))
                    (bv 1911260447165 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv 18446612985909035097 (bitvector 64))))


(define r1_12 (list (bv 1361 (bitvector 64))
                    (bv 1911260447165 (bitvector 64))
                    (bv 558345748610 (bitvector 64))
                    (bv 319 (bitvector 64))
                    (bv 1872605741492 (bitvector 64))
                    (bv 39 (bitvector 64))
                    (bv 131 (bitvector 64))
                    (bv -1 (bitvector 64))))

(define r1 (list (make-run-step r1_0 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_1 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_2 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_3 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_4 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_5 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_6 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_7 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_8 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_9 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_10 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_11 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4))))) (make-run-step r1_12 (INSTR (OPCODE (bv #b0111 (bitvector 4))) (OPERANDS (bv #b0111 (bitvector 4)) (bv #b0111 (bitvector 4)))))))

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
                               (diff 13 12 r0 13 12 r1 myexpr)
))))

(print-forms sol)