c3:
	cargo build --release
	bash ./test.sh release

debug:
	cargo build
	bash ./test.sh debug

init:
	cp ./etc/config ./.git

clean:
	rm tmp* *.s *.o -f
	cargo clean

.PHONY: c3 debug init clean