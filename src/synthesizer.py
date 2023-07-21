from fileinput import filename
import shutil

from numpy import empty
from interfaces import Run
from typing import List
from pathlib import Path

from config import CONF

class Synthesizer:
    filename: str
    work_dir: str
    depth: int

    def __init__(self, filename, work_dir, depth):
        self.filename = filename
        self.work_dir = work_dir
        self.depth = int(depth)
        self.add_core()

    def add_core(self):
        Path(self.work_dir).mkdir(exist_ok=True)
        core_path = "core.rkt"
        # print(f"[*] Workdir: {self.work_dir}, filename: {self.filename}")
        shutil.copy2(core_path, self.work_dir + "/" + self.filename)

    def get_xstate_name(self, rid, xid):
        return "{0}_{1}".format(self.get_run_name(rid), str(xid))

    def get_run_name(self, rid):
        return "r{0}".format(str(rid))
    
    def make_opcode(opcode_val, mod_rm_val):
        # Convert the opcode value + mod_rm to bitstrings.
        opcode_bits = bin(opcode_val)[2:].zfill(6)
        mod_rm_bits = bin(mod_rm_val)[2:].zfill(8)

        # Concat to make full opcode bitstring.
        full_bits = opcode_bits + mod_rm_bits

        # Convert to int.
        full_opcode = int(full_bits, 2)

        return full_opcode

    """
        Separate method for generating Rosette structures for architecture states.
    """
    def map(self, run):
        def model(rid, xid, xstate, instrs, opcodes):

            xstate_name = self.get_xstate_name(rid, xid)
            header = len('(define {0} (list '.format(xstate_name))
            indentation = ''
            for _ in range (0,header):
                indentation += ' '
            regs = ''

            print(f"[synthesizer] Instr: {instrs[xid-1]} Opcode: {opcodes[xid-1]}")

            # Opcode and operand encoding.
            opcode_bs = bin(opcodes[xid-1])[2:] # Ignore 0b.
            opcode = int(opcode_bs, 2)

            operand_bv = ""
            for operand in instrs[xid-1].operands:
                op_bs = "".join(format(ord(c), '08b') for c in operand.value)
                op = int(op_bs, 2)
                if operand_bv != "":
                    operand_bv += indentation
                operand_bv += f"(bv {op} (bitvector 64))\n"

            for idx, reg in enumerate(xstate.regs.values()):
                if regs != '':
                    regs += indentation
                reg_idx = CONF.map_reg(idx)
                reg_name = CONF.registers.get(reg_idx)

                regs += f"(bv {str(reg)} (bitvector 64))\t; Register: {reg_name.upper()}\n"
            if xstate.pc is not None:
                regs += indentation + f"(bv {str(xstate.pc)} (bitvector 64))\t; PC\n"
                regs += indentation + f"; Opcode\n"
                regs += indentation + f"(bv {opcode} (bitvector 64))\n"
                regs += indentation + f"; Operands\n"
                regs += indentation + operand_bv + "))"
                # End of archstate object. Add more information, e.g., opcode + operands.
            else:
                # Meaning this is the final state
                regs += indentation + f"(bv {str(-1)} (bitvector 64))\t; Final state\n"
                # End of archstate object. Add more information, e.g., opcode + operands.
                regs += indentation + f"; Opcode\n"
                regs += indentation + f"(bv {opcode} (bitvector 64))\n"
                regs += indentation + f"; Operands\n"
                regs += indentation + operand_bv + "))"

            return f"; Instruction: {instrs[xid-1]}\n(define {xstate_name} (list\t ;Registers\n{indentation}{regs}\n\n"

        with open(self.work_dir + "/" + self.filename, "a") as f:   
            xstates = ''
            for i, xstate in enumerate(run.archstates):  
                f.write(model(run.id, i, xstate, run.mem_instrs, run.opcodes))
                if xstates == '':
                    xstates += self.get_xstate_name(run.id, i)
                else:
                    xstates += ' ' + self.get_xstate_name(run.id, i) 
            f.write(f"(define {self.get_run_name(run.id)} (list {xstates}))\n\n")

    # TODO: update to look at sequential traces, and ignore silent steps for now.
    def generate_constraints(self, pairs, run1, run2):
        with open(self.work_dir + "/" + self.filename, "a") as f:
            f.write("(define myexpr (cexpr #:depth {0}))\n\n".format(
                str(self.depth)))
            header = len('(define sol (solve (assert (or ')
            indentation = ''
            for i in range(0, header):
                indentation += ' '
            i = i_ = 0
            pairs.append((len(run1.archstates)-1, len(run2.archstates)-1))
            # print(f"[generate_constraints] pairs.len: {len(pairs)}")
            j, j_ = pairs[0]
            k = 1
            rid1 = run1.id
            rid2 = run2.id
            constraints = ''
            constraints += "(diff {0} {1} {2} {3} {4} {5} myexpr)\n".format(
                str(i), str(j), self.get_run_name(rid1),
                str(i_), str(j_), self.get_run_name(rid2)
            )
            i, i_ = j, j_
            # TODO: test without below constraints.
            while (k < len(pairs)):
                j, j_ = pairs[k]
                constraints += indentation + "(diff {0} {1} {2} {3} {4} {5} myexpr)\n".format(
                    str(i), str(i+1), self.get_run_name(rid1),
                    str(i_), str(i_+1), self.get_run_name(rid2)
                )
                constraints += indentation + "(diff {0} {1} {2} {3} {4} {5} myexpr)\n".format(
                    str(i+1), str(j), self.get_run_name(rid1),
                    str(i_+1), str(j_), self.get_run_name(rid2)
                )
                i, i_ = j, j_
                k += 1
            f.write(
                "(define sol (solve (assert (or {0}))))\n\n".format(constraints))
            f.write("(print-forms sol)")

# (define myexpr (cexpr #:depth 1))
# (define sol (solve (assert (or (myexpr r34_1 r84_1))))
# sol
# myexpr
# (print-forms sol)
