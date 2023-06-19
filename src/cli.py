#!/usr/bin/env python3
"""
File: Command Line Interface

Copyright (C) Microsoft Corporation
SPDX-License-Identifier: MIT
"""

import os
import yaml
from ast import List
from typing import Dict
from datetime import datetime
from argparse import ArgumentParser
from factory import get_minimizer, get_fuzzer
from fuzzer import Fuzzer
from config import CONF
from service import LOGGER

from parser import Parser, Expr

# Synthesis parts.
from synthesizer import Synthesizer


def main() -> int:
    parser = ArgumentParser(description='', add_help=False)
    subparsers = parser.add_subparsers(dest='subparser_name')

    # Fuzzing
    parser_fuzz = subparsers.add_parser('fuzz')
    parser_fuzz.add_argument(
        "-s", "--instruction-set",
        type=str,
        required=True
    )
    parser_fuzz.add_argument(
        "-c", "--config",
        type=str,
        required=False
    )
    parser_fuzz.add_argument(
        "-n", "--num-test-cases",
        type=int,
        default=1,
        help="Number of test cases.",
    )
    parser_fuzz.add_argument(
        "-i", "--num-inputs",
        type=int,
        default=100,
        help="Number of inputs per test case.",
    )
    parser_fuzz.add_argument(
        '-w', '--working-directory',
        type=str,
        default='',
    )
    parser_fuzz.add_argument(
        '-t', '--testcase',
        type=str,
        default=None,
        help="Use an existing test case [DEPRECATED - see reproduce]"
    )
    parser_fuzz.add_argument(
        '--timeout',
        type=int,
        default=0,
        help="Run fuzzing with a time limit [seconds]. No timeout when set to zero."
    )
    parser_fuzz.add_argument(
        '--nonstop',
        action='store_true',
        help="Don't stop after detecting an unexpected result"
    )

    parser_analyser = subparsers.add_parser('analyse')
    parser_analyser.add_argument(
        '--ctraces',
        type=str,
        required=True,
    )
    parser_analyser.add_argument(
        '--htraces',
        type=str,
        required=True,
    )
    parser_analyser.add_argument(
        "-c", "--config",
        type=str,
        required=False
    )

    parser_reproduce = subparsers.add_parser('reproduce')
    parser_reproduce.add_argument(
        "-s", "--instruction-set",
        type=str,
        required=True
    )
    parser_reproduce.add_argument(
        "-c", "--config",
        type=str,
        required=False
    )
    parser_reproduce.add_argument(
        '-t', '--testcase',
        type=str,
        default=None,
        required=True,
        help="Path to the test case",
    )
    parser_reproduce.add_argument(
        '-i', '--inputs',
        type=str,
        nargs='*',
        default=None,
        help="Path to the directory with inputs"
    )
    parser_reproduce.add_argument(
        "-n",
        "--num-inputs",
        type=int,
        default=100,
        help="Number of inputs per test case. [IGNORED if --input-dir is set]",
    )

    parser_mini = subparsers.add_parser('minimize')
    parser_mini.add_argument(
        '--infile', '-i',
        type=str,
        required=True,
    )
    parser_mini.add_argument(
        '--outfile', '-o',
        type=str,
        required=True,
    )
    parser_mini.add_argument(
        "-c", "--config",
        type=str,
        required=False
    )
    parser_mini.add_argument(
        "-n", "--num-inputs",
        type=int,
        default=100,
        help="Number of inputs per test case.",
    )
    parser_mini.add_argument(
        "-f", "--add-fences",
        action='store_true',
        default=False,
        help="Add as many LFENCEs as possible, while preserving the violation.",
    )
    parser_mini.add_argument(
        "-s", "--instruction-set",
        type=str,
        required=True
    )

    parser_generator = subparsers.add_parser('generate')
    parser_generator.add_argument(
        "-s", "--instruction-set",
        type=str,
        required=True
    )
    parser_generator.add_argument(
        "-r", "--seed",
        type=int,
        default=0,
        help="Add seed to generate test case.",
    )
    parser_generator.add_argument(
        "-n", "--num-test-cases",
        type=int,
        default=5,
        help="Number of test cases.",
    )
    parser_generator.add_argument(
        "-i", "--num-inputs",
        type=int,
        default=100,
        help="Number of inputs per test case.",
    )
    parser_generator.add_argument(
        "-c", "--config",
        type=str,
        required=False
    )
    parser_generator.add_argument(
        '-w', '--working-directory',
        type=str,
        default='',
    )
    parser_generator.add_argument(
        '--permit-overwrite',
        action='store_true',
    )

    args = parser.parse_args()

    # Update configuration
    if args.config:
        CONF.config_path = args.config
        with open(args.config, "r") as f:
            config_update: Dict = yaml.safe_load(f)
        for var, value in config_update.items():
            setattr(CONF, var, value)
    LOGGER.set_logging_modes()

    # Fuzzing
    if args.subparser_name == 'fuzz':
        # Make sure we're ready for fuzzing.
        if args.working_directory and not os.path.isdir(args.working_directory):
            SystemExit("The working directory does not exist")

        contract_str: List[str] = [
            "(IF (BOOL #t) (REG 0))",
            "(IF (BOOL #t) (REG 1))",
            "(IF (BOOL #t) (REG 2))",
            "(IF (BOOL #t) (REG 4))"
        ]

        contract: List[Expr] = []

        for s in contract_str:
            parser = Parser(s)
            contract.append(parser.parse())

        # TODO: testing looping over violations differently to continuously update contract object.
        while True:
            LOGGER.reset()
            LOGGER.set_logging_modes()

            # Normal fuzzing mode
            fuzzer = get_fuzzer(args.instruction_set,
                                args.working_directory, args.testcase, "", contract)
            result = fuzzer.start(
                args.num_test_cases,
                args.num_inputs,
                args.timeout,
                args.nonstop,
            )            

            if result is None:
                print(f"[-] No result tuple from fuzzer.")
                print(f"[+] Latest contract:")
                LOGGER.show_contract(contract)
                break
            else:
                # print(f"[+] Violation result: {result}")
                run1, run2, pairs = result
                timestamp = datetime.today().strftime('%H%M%S-%d-%m-%y')
                theory_fname = "theory-" + timestamp + ".rkt"
                expr_fname = "expr-" + timestamp + ".txt"

                # TODO: for all instructions in run trace, get memory operands.
                #       add these to our synthesis mapper to have more than just
                #       register values between runs. A load/store leaking an
                #       address will be covered by this. Look at differences
                #       between operand values (addresses) as opposed to registers.

                # Instructions should always be the same, but the differences are in
                # the register values from those instructions (e.g., a violation is input-dependent).
                # TODO: test for differences in memory instruction operands/addresses.
                #       A difference in addresses for same instruction would be a new clause.
                
                # Test printing memory differences between run1 and run2.
                run1_mems = {}
                run2_mems = {}

                for xstate in run1.archstates:
                    run1_mems.update(xstate.mems)
                for xstate in run2.archstates:
                    run2_mems.update(xstate.mems)

                unique_mems = {k: run1_mems[k] for k in run1_mems if k in run2_mems and run1_mems[k] != run2_mems[k]}
                print(f"[+] Testing unique mems between runs: {unique_mems}")
                
                synth = Synthesizer(theory_fname, args.working_directory, 1)
                # Each run object corresponds to an execution of a same program with different inputs
                # that produce the same contract trace and different hardware trace.
                synth.map(run1)
                synth.map(run2)
                synth.generate_constraints(pairs, run1, run2)

                import subprocess
                import signal

                command = "racket " + args.working_directory + "/" + theory_fname
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

                with open(args.working_directory + "/" + expr_fname, "w") as file:
                    file.write(stdout.decode("UTF-8"))
                with open(args.working_directory + "/" + expr_fname, "r") as file:
                    s = file.readlines()[-1]
                    s = s[len("(define myexpr "):-1]
                    parser = Parser(s)
                    # print(f"[+] Test parse: {parser.parse()}")
                    contract.append(parser.parse())

                LOGGER.show_contract(contract)
        # End test.
        return result

    # Reproducing a violation
    if args.subparser_name == 'reproduce':
        fuzzer = get_fuzzer(args.instruction_set, "",
                            args.testcase, args.inputs)
        exit_code = fuzzer.start(1, args.num_inputs, 0, False)
        return exit_code

    # Stand-alone generator
    if args.subparser_name == "generate":
        fuzzer = get_fuzzer(args.instruction_set,
                            args.working_directory, None, "")
        fuzzer.generate_test_batch(
            args.seed,
            args.num_test_cases,
            args.num_inputs,
            args.permit_overwrite
        )
        return 0

    # Trace analysis
    if args.subparser_name == 'analyse':
        fuzzer = Fuzzer.analyse_traces_from_files(args.ctraces, args.htraces)
        return 0

    # Test case minimisation
    if args.subparser_name == "minimize":
        minimizer = get_minimizer(args.instruction_set)
        minimizer.minimize(args.infile, args.outfile,
                           args.num_inputs, args.add_fences)
        return 0

    raise Exception("Unreachable")


if __name__ == '__main__':
    exit_code = main()
    exit(exit_code)
