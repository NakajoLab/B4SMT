all: programs processor

.PHONY: check check-artifacts clean programs processor

programs:
	nix -L build -o $@

check:
	nix -L flake check

check-artifacts:
	nix -L build '.#checks.x86_64-linux.quick'

check-slow:
	nix -L build ".#packages.x86_64-linux.slowChecks"

processor:
	nix -L build '.#processor' -o $@

ip: processor
	mkdir -p ip/B4Processor_1_0/src
	cp -f processor/B4Processor.sv ip/B4Processor_1_0/src

clean:
	rm -rf programs processor result
