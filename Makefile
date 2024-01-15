CC=gcc
CFLAGS=-g -Werror -pedantic -fPIC
LIBS=C/fake6502.o C/fake6502host.o
FAKE6502=C/fake6502.c C/fake6502.h
SHLIB=C/fake6502.so
RESOURCES=resources/characters.bin resources/basic-kernal.bin
PRGS=examples/condensed.prg
DERIVED=C/*.o C/*.so C/LICENSE.* C/fake6502.c C/fake6502.h resources

default: $(SHLIB) $(RESOURCES)

test: default $(PRGS) FRC
	julia --project=. -e 'include("src/Fake6502.jl"); Fake6502.test()'

%.prg: %.tass
	64tass --vice-labels -a --cbm-prg -o $@ -L $(patsubst %.prg,%.list,$@) -l $(patsubst %.prg,%.labels,$@) $<

$(FAKE6502):
	git clone git@github.com:omarandlorraine/fake6502.git FAKE6502
	cp FAKE6502/fake6502.[ch] C
	cp FAKE6502/LICENSE C/LICENSE.fake6502
	rm -rf FAKE6502

c64: default $(PRGS) FRC
	julia --project=. -e 'include("src/Fake6502.jl"); Fake6502.C64.test_c64()'

resources:
	mkdir -p resources

resources/characters.bin: resources
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/characters/c64.bin > $@

resources/basic-kernal.bin: resources
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/computers/c64/64c.251913-01.bin > $@

clean: FRC
	rm -rf $(DERIVED)

$(SHLIB): $(LIBS)
	$(CC) -rdynamic -shared -o $(SHLIB) $(LIBS)

C/fake6502host.o: $(FAKE6502)
	$(CC) -c C/fake6502host.c -o $@

C/fake6502.o: $(FAKE6502)
	$(CC) -DDECIMALMODE -DNMOS6502 -c $(CFLAGS) C/fake6502.c -o $@

FRC:
