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

check-format:
	nix -L build '.#format'

clean:
	rm -rf programs processor result
	rm -rf *.sv *.anno.json

clean-chisel:
	rm -rf target test_run_dir

update-hash:
	nix run ".#update-hash"
