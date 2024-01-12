CC=gcc
CFLAGS=-g -Werror -pedantic -fPIC
FAKE6502=../fake6502
LIBS=fake6502.o fake6502host.o
INC=fake6502.h
SRC=fake6502.c fake6502.h
SHLIB=fake6502.so
RESOURCES=resources/characters.bin resources/basic-kernal.bin

default: links $(SHLIB) $(RESOURCES)

test: default FRC
	64tass --vice-labels -a --cbm-prg -L condensed.list -l condensed.labels condensed.tass
	julia --project=Fake6502.jl -e 'include("Fake6502.jl/src/Fake6502.jl"); Fake6502.test()'

c64: default FRC
	64tass --vice-labels -a --cbm-prg -L condensed.list -l condensed.labels condensed.tass
	julia --project=Fake6502.jl -e 'include("Fake6502.jl/src/Fake6502.jl"); Fake6502.C64.test_c64()'

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
	ln -s $(FAKE6502)/$@

$(SHLIB): $(LIBS)
	$(CC) -rdynamic -shared -o $(SHLIB) $(LIBS)

fake6502host.o: fake6502host.c $(INC)
	$(CC) -c fake6502host.c

fake6502.o: fake6502.c $(INC)
	$(CC) -DDECIMALMODE -DNMOS6502 -c $(CFLAGS) fake6502.c -o $@

FRC:
