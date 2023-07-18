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

clean:
	rm -rf programs processor result
