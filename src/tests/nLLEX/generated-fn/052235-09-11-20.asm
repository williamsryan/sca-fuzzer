.intel_syntax noprefix
.test_case_enter:
test_case_main:
.test_case_main.entry:
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV EAX, EDI
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV EBX, EDI
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV ECX, EDI
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV EDX, EDI
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV ESI, EDI
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
MOV R13D, EDI
ADD RSP, 8
IMUL EDI, EDI, 2891336453
ADD EDI, 12345
PUSHQ RDI
AND qword ptr [RSP], 2263
OR qword ptr [RSP], 2
POPFQ 
LEA RSP, qword ptr [RSP - 8]
LFENCE 
JMP .bb0
.bb0:
INC BX
AND RAX, 45784988
AND RSI, 0b111111000000
ADD RSI, R14
CMP qword ptr [RSI], -2
AND RBX, 0b111111000000
ADD RBX, R14
CMOVLE RAX, qword ptr [RBX]
{load} SUB CL, BL
OR EAX, -830192819
REX MOVZX R13, DL
CMOVNB EAX, EAX
XOR R13D, 20
SBB RAX, 31
REX MOVSX RDX, CL
{store} ADC EAX, R13D
AND R13, 0b111111000000
ADD R13, R14
ADC byte ptr [R13], AL
CMOVNO ECX, ECX
AND R13, 0b111111000000
ADD R13, R14
CMP DX, word ptr [R13]
SETB AL
JNB .bb1
JMP .bb2
.bb1:
AND RBX, 0b111111000000
ADD RBX, R14
CMOVNL EBX, dword ptr [RBX]
AND RBX, 0b111111000000
ADD RBX, R14
LOCK XOR word ptr [RBX], -10448
CMOVL RBX, RBX
AND RCX, 0b111111000000
ADD RCX, R14
OR qword ptr [RCX], -39
NEG AL
XOR SI, -115
AND RAX, 0b111111000000
ADD RAX, R14
MOV dword ptr [RAX], 1084301042
SETBE DL
CMOVO EAX, EDX
AND RBX, 0b111111000000
ADD RBX, R14
SETZ byte ptr [RBX]
XOR RAX, 733185866
CMOVNBE EDX, ESI
TEST RBX, -265360950
REX SETNP R13B
REX NOT AL
CMP EBX, 1568348698
JL .bb2
JMP .bb3
.bb2:
{store} SUB EDX, ESI
SETS CL
CMP BX, 53
AND R13, 0b111111000000
ADD R13, R14
LOCK DEC dword ptr [R13]
AND RAX, 0b111111000000
ADD RAX, R14
ADC word ptr [RAX], 11261
AND RCX, 0b111111000000
ADD RCX, R14
LOCK INC dword ptr [RCX]
REX SETNL DL
CMOVS AX, SI
REX XOR R13B, -43
{load} XOR SI, DX
AND RBX, 0b111111000000
ADD RBX, R14
XOR byte ptr [RBX], 97
{load} REX XOR R13B, DL
AND RCX, 818018834
REX SETNLE CL
CWDE 
AND R13, 0b111111000000
ADD R13, R14
SUB dword ptr [R13], 637568705
JMP .bb3
.bb3:
AND AX, -115
MOV RDX, 0
OR R13D, 0x98
AND RAX, 0xff
DIV R13D
{store} XOR AX, DX
STD 
TEST AX, 14406
CMOVNP RAX, RAX
CMOVNO EBX, ESI
XOR RAX, -751976872
ADC RAX, -1372529725
ADC RAX, -752868219
REX SBB CL, 86
{load} ADC RCX, RSI
ADD BL, -8
SUB RSI, 14
REX DEC AL
{store} ADD RDX, RSI
JMP .test_case_main.exit
.test_case_main.exit:
.test_case_exit:
MFENCE
