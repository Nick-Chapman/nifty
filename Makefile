
BINS := \
./_build/default/images/from_assembler/stuff.exe \
./_build/default/images/collatz/collatz.exe \

default:
	jbuilder build $(BINS)

clean:
	rm -rf _build

.PHONY: default clean
