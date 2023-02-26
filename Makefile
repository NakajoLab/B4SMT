all: programs processor

.PHONY: check check-artifacts clean programs processor

programs:
	nix -L build -o $@

check:
	nix -L flake check

check-artifacts:
	nix -L build '.#checks.x86_64-linux.all'

processor:
	nix -L build '.#processor' -o $@

ip: processor
	mkdir -p ip/B4Processor_1_0/src
	cp -f processor/B4Processor.sv ip/B4Processor_1_0/src

#B4Processor.v:
#	sbt "runMain b4processor.B4Processor"
#
#ip/B4Processor_1_0/src/B4Processor.v: B4Processor.v
#	cp $< $@

clean:
	rm -rf programs processor result
