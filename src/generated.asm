.intel_syntax noprefix
MFENCE # instrumentation
.test_case_enter:
.function_main:
.bb_main.entry:
JMP .bb_main.0 
.bb_main.0:
ADD SIL, -72 # instrumentation
ADC AL, 81 
IMUL AL 
IMUL RBX 
AND RSI, 0b1111111111111 # instrumentation
LOCK NEG dword ptr [R14 + RSI] 
ADC RAX, 577908958 
SBB BL, AL 
JNO .bb_main.1 
JMP .bb_main.exit 
.bb_main.1:
AND RSI, 0b1111111111111 # instrumentation
IMUL byte ptr [R14 + RSI] 
ADC BL, BL 
SUB EDI, EDI 
AND RAX, 0b1111111111111 # instrumentation
DEC dword ptr [R14 + RAX] 
AND RDI, 0b1111111111111 # instrumentation
CMP word ptr [R14 + RDI], CX 
AND RBX, 0b1111111111111 # instrumentation
CMP BL, byte ptr [R14 + RBX] 
SBB AX, -25594 
AND RCX, 0b1111111111111 # instrumentation
LOCK ADD word ptr [R14 + RCX], DI 
ADC RSI, RDX 
AND RCX, 0b1111111111111 # instrumentation
ADD qword ptr [R14 + RCX], -66 
.bb_main.exit:
.test_case_exit:
MFENCE # instrumentation
