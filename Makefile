CC=gcc
CFLAGS=-g -Werror -pedantic -fPIC
FAKE6502=../../fake6502
LIBS=C/fake6502.o C/fake6502host.o
INC=C/fake6502.h
SRC=C/fake6502.c C/fake6502.h
SHLIB=C/fake6502.so
RESOURCES=resources/characters.bin resources/basic-kernal.bin
PRGS=examples/condensed.prg
OBJS = $(patsubst %.asm,%.o,$(SRCS))

default: links $(SHLIB) $(RESOURCES)

#	64tass --vice-labels -a --cbm-prg -o C/condensed.prg -L C/condensed.list -l C/condensed.labels C/condensed.tass

test: default $(PRGS) FRC
	julia --project=. -e 'include("src/Fake6502.jl"); Fake6502.test()'

%.prg: %.tass
	64tass --vice-labels -a --cbm-prg -o $@ -L $(patsubst %.prg,%.list,$@) -l $(patsubst %.prg,%.labels,$@) $<

c64: default $(PRGS) FRC
	julia --project=. -e 'include("src/Fake6502.jl"); Fake6502.C64.test_c64()'

resources:
	mkdir -p resources

resources/characters.bin: resources
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/characters/c64.bin > $@

resources/basic-kernal.bin: resources
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/computers/c64/64c.251913-01.bin > $@

links: $(SRC)

clean: FRC
	rm -f $(LIBS) $(SHLIB)

$(SRC):
	ln -s $(FAKE6502)/$@ C

$(SHLIB): $(LIBS)
	$(CC) -rdynamic -shared -o $(SHLIB) $(LIBS)

C/fake6502host.o: C/fake6502host.c $(INC)
	$(CC) -c C/fake6502host.c -o $@

C/fake6502.o: C/fake6502.c $(INC)
	$(CC) -DDECIMALMODE -DNMOS6502 -c $(CFLAGS) C/fake6502.c -o $@

FRC:
