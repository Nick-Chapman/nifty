
BINS := \
./_build/default/assembler/game/blast/bin/blast.exe \
./_build/default/assembler/game/play/bin/play.exe \
./_build/default/compiler/game/collatz/bin/collatz.exe \
./_build/default/game/game0/bin/game0.exe \


default:
	jbuilder build $(BINS)

clean:
	rm -rf _build

.PHONY: default clean
