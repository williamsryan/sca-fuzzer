#lang rosette/safe
; ----------------------------------------- ;
; <Generated file>
; ----------------------------------------- ;

; ----------------- CORE ------------------ ;
(require rosette/lib/destruct)  ; Value destructuring library.
(require rosette/lib/synthax)   ; Synthesis library.
; (require syntax/parse)

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
(struct OPCODE ())                      ; An opcode.
(struct INSTR ())                       ; An instruction.
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

; TODO: extending the base contract grammar to include
;       constructs for cache/memory access so we can learn
;       addresses from memory accesses.
(define-grammar (cexpr)
  [expr (IF (pred) (bs))]
  [pred (choose (BOOL (?? boolean?))
                (NOT (pred))
                (AND (pred) (pred))
                (OR (pred) (pred))
                (EQ (bs) (bs))
                )]
  [bs (choose (BS (?? (bitvector (?? integer?))))
              (SLIDE (?? integer?) (?? integer?) (bs))
              (REG (?? integer?))
              (INSTR)
              (ADDR (?? integer?))
              ; (MEMORY-ACCESS (?? integer?) (bs))
              ; (CACHE-ACCESS (?? integer?) (bs))
              )]
)

(define-struct RegisterValue (value))

(define-struct SystemState (registers))

; --------------- AUX-FUNCS ------------------ ;
(define EMPTY (list '()))

; Check for differences between two register values.
(define (register-diff reg1 reg2)
  (not (equal? (RegisterValue-value reg1)
               (RegisterValue-value reg2))))

; Check if observables of two runs are distinguishable, return #t if so.
(define (obs-diff? run1 run2)
  (define (check-registers regs1 regs2)
    (cond
      [(and (null? regs1) (null? regs2)) #f]              ; Both runs have ended
      [(or (null? regs1) (null? regs2)) #t]               ; One run has ended while the other continues
      [(register-diff (car regs1) (car regs2)) #t]        ; Register values differ
      [else (check-registers (cdr regs1) (cdr regs2))]    ; Continue checking the remaining registers
    ))

  (check-registers (SystemState-registers run1) (SystemState-registers run2)))
  
; --------------- TESTS ------------------ ;

; Example usage for comparing two runs.
(define register1 (make-RegisterValue "foo"))
(define register2 (make-RegisterValue "bar"))
(define register3 (make-RegisterValue 42))

(define system-state1 (make-SystemState (list register1 register2 register3)))
(define system-state2 (make-SystemState (list register2 register1 register3)))

(define result (obs-diff? system-state1 system-state2))
(printf "Registers are distinguishable: ~a\n" result)

(define myexpr (cexpr #:depth 1))

(define sol (solve (assert(obs-diff? system-state1 system-state2))))

(print-forms sol)

; ------------- END-CORE ------------------ ;
