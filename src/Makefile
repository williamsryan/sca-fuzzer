.PHONY : test clean

test :
	python3 cli.py fuzz -s x86/isa_spec/base.json -c tests/test-detection.yaml -i 50 -n 100 -w . --empty-synth

clean :
	rm -rf generated.o *.asm theory*.rkt
	git clean -f

