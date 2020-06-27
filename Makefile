c3:
	cargo build --release
	bash ./test.sh release ./tests/test.c

debug:
	cargo build
	bash ./test.sh debug ./tests/test.c

init:
	cp ./etc/config ./.git

clean:
	rm tmp* *.s *.o -f
	cargo clean

.PHONY: c3 debug init clean