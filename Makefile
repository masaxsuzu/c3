c3:
	cargo build --release
	bash ./test.sh release

debug:
	cargo build
	bash ./test.sh debug

.PHONY: c3 debug