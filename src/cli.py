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
from rosette import Rosette


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
        # Make sure we're ready for fuzzing
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

        # Normal fuzzing mode
        fuzzer = get_fuzzer(args.instruction_set,
                            args.working_directory, args.testcase, "")
        result = fuzzer.start(
            args.num_test_cases,
            args.num_inputs,
            args.timeout,
            args.nonstop,
        )

        if result is False:
            print(f"[-] No result tuple from fuzzer.")
        else:
            print(f"[+] Violation result: {result}")
            run1, run2, pairs = result
            timestamp = datetime.today().strftime('%H%M%S-%d-%m-%y')
            theory_fname = "theory-" + timestamp + ".rkt"
            expr_fname = "expr-" + timestamp + ".txt"

            rosette = Rosette(theory_fname, args.working_directory, 1)
            # Do the below two run maps correspond to contract traces?
            # If so, they should always be the same since we want equivalent ctraces with different inputs, producing different hardware traces.
            rosette.map(run1)
            rosette.map(run2)

            # TODO: should run1 and run2 be the same? Re-check definition of a valuable test case/trace.
            rosette.generate_constraints(pairs, run1, run2)

            # TODO: run synthesis refinement loop elsewhere.
            # import subprocess
            # import signal

            # command = "racket " + args.working_directory + "/" + theory_fname
            # with subprocess.Popen(command, shell=True, stdout=subprocess.PIPE,
            #                       start_new_session=True) as process:
            #     try:
            #         stdout, stderr = process.communicate(timeout=6000)
            #         return_code = process.poll()
            #         if return_code:
            #             raise subprocess.CalledProcessError(return_code, process.args,
            #                                                 output=stdout, stderr=stderr)
            #     except subprocess.TimeoutExpired:
            #         os.killpg(process.pid, signal.SIGINT)
            #         raise

            # with open(args.working_directory + "/" + expr_fname, "w") as file:
            #     file.write(stdout.decode("UTF-8"))
            # with open(args.working_directory + "/" + expr_fname, "r") as file:
            #     # file.readline()
            #     s = file.readline()
            #     s = s[len("(define myexpr "):-1]
            #     parser = Parser(s)
            #     contract.append(parser.parse())

            LOGGER.show_contract(contract)

        print(f"[+] Final result: {result}") # (<interfaces.Run object at 0x7f11bd72ffa0>, <interfaces.Run object at 0x7f11bd72ffa0>, [(0, 0), (11, 11)])
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
