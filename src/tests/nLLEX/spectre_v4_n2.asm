.intel_syntax noprefix
.test_case_enter:
MOV rax, 0

# the leaked value - rcx
# construct a page offset from the random value
AND rcx, 0b111111000000
ADD rcx, 64

# save some value into the test address
MOV qword ptr [r14], rcx
MFENCE

# delay the store
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]
LEA rbx, qword ptr [rbx + rax + 1]
LEA rbx, qword ptr [rbx + rax - 1]

# store and load, potentially matching
AND rbx, 0b111111000000
MOV qword ptr [r14 + rbx], 4096 - 64
MOV qword ptr [r14 + rbx], 4096 - 128
MOV rdx, qword ptr [r14]  # misprediction happens here

# dependent load
MOV rdx, qword ptr [r14 + rdx]
MFENCE
