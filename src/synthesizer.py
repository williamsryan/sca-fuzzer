from fileinput import filename
import shutil

from numpy import empty
from interfaces import Run
from typing import List
from pathlib import Path


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
        core_path = "core-new.rkt"
        # print(f"[*] Workdir: {self.work_dir}, filename: {self.filename}")
        shutil.copy2(core_path, self.work_dir + "/" + self.filename)

    def get_xstate_name(self, rid, xid):
        return "{0}_{1}".format(self.get_run_name(rid), str(xid))

    def get_run_name(self, rid):
        return "r{0}".format(str(rid))

    """
        Separate method for generating Rosette structures for register values.
    """
    def map_regs(self, run):
        def model(rid, xid, xstate, instrs):
            xstate_name = self.get_xstate_name(rid, xid)
            header = len('(define {0} (list '.format(xstate_name))
            indentation = ''
            for i in range(0, header):
                indentation += ' '
            regs = ''
            for reg in xstate.regs.values():
                if regs != '':
                    regs += indentation
                regs += "(bv {0} (bitvector 64))\n".format(str(reg)) # Each of these is the value of a given register.
            if xstate.pc is not None:
                regs += indentation + \
                    "(bv {0} (bitvector 64))".format(str(xstate.pc))
            else:
                # Meaning this is the final state; end with -1.
                regs += indentation + "(bv {0} (bitvector 64))".format(str(-1))

            # Annotate each register state object with the observed instruction.
            # Depending on results, we may actually use it in the tuple to help
            # guide the synthesizer to learning instructions vs. registers.
            for instr in instrs:
                print(f"[synthesizer] Test instrs: {instr} : {instr.operands}")
            return "\n(define {0} (list {1}))\n\n".format(xstate_name, regs)

        with open(self.work_dir + "/" + self.filename, "a") as f:
            xstates = ''
            for i, xstate in enumerate(run.archstates):
                f.write(model(run.id, i, xstate, run.instructions))

                if xstates == '':
                    # Implement logic for making xstate look like:
                    # (make-run-step r0_0 'LOAD).
                    # xstates += self.get_xstate_name(run.id, i)
                    xstates += f"(make-run-step {self.get_xstate_name(run.id, i)} 'LOAD)"
                else:
                    xstates += ' ' + f"(make-run-step {self.get_xstate_name(run.id, i)} 'LOAD)"

            # This is where the run (r0, r1) objects are defined (as a list of registers).
            # TODO: update to match new struct where instruction is included too.
            new_obj = f"(define {self.get_run_name(run.id)} (list {xstates}))\n\n"
            
            f.write("(define {0} (list {1}))\n\n".format(
                self.get_run_name(run.id), xstates))

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
