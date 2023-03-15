#!/usr/bin/env python3

"""
Simple test script for checking constraint -> contract
clause generation.
Works independently from Revizor (requires a theory rkt file).
"""

from parser import Parser

import subprocess
import signal
import sys, os

def main():
    theory_fname = "theory-old.rkt"
    command = "racket " + "./" + theory_fname

    expr_fname = "expr-" + "001" + ".txt"

    with subprocess.Popen(command, shell=True, stdout=subprocess.PIPE,
                            start_new_session=True) as process:
        try:
            stdout, stderr = process.communicate(timeout=6000)
            return_code = process.poll()
            if return_code:
                raise subprocess.CalledProcessError(return_code, process.args,
                                                    output=stdout, stderr=stderr)
        except subprocess.TimeoutExpired:
            os.killpg(process.pid, signal.SIGINT)
            raise

    # with open("./" + expr_fname, "w") as file:
    #     file.write(stdout.decode("UTF-8"))

    with open("./" + expr_fname, "r") as file:
        s = file.readlines()[-1]
        s = s[len("(define myexpr "):-1]
        parser = Parser(s)
        print(f"[+] Test parse: {parser.parse()}")
        # contract.append(parser.parse())

if __name__ == "__main__":
    sys.exit(main())