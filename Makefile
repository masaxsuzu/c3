c3:
	cargo build

test: c3
	bash ./test.sh

.PHONY: test c3 dev