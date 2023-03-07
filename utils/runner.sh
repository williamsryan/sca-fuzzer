#!/bin/bash

INSTR_SET="x86/isa_spec/base.json"
CONFIG="tests/test-detection.yaml"

# git submodule update --init --recursive

# cd ..
# cp src/executor/x86/base.xml src/instruction_sets/x86

cd src/x86/executor 
make uninstall # the command will give an error message, but it's ok!
make clean
make
make install

# Hyperthreading disable
# sudo echo off > /sys/devices/system/cpu/smt/control

cd ../..

$SHELL
