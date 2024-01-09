CC=gcc
CFLAGS=-g -Werror -pedantic -fPIC
FAKE6502=../fake6502
LIBS=fake6502.o fake6502host.o
SRC=fake6502.c fake6502.h
SHLIB=fake6502.so

default: links $(SHLIB)

test: default FRC
	64tass --vice-labels -a --cbm-prg -L condensed.list -l condensed.labels condensed.tass
	julia --project=Fake6502.jl -e 'include("Fake6502.jl/src/Fake6502.jl"); Fake6502.test()'

links: $(SRC)

clean: FRC
	rm -f $(LIBS) $(SHLIB)

$(SRC):
	ln -s $(FAKE6502)/$@

$(SHLIB): $(LIBS)
	$(CC) -rdynamic -shared -o $(SHLIB) $(LIBS)

fake6502host.o: fake6502host.c
	$(CC) -c fake6502host.c

fake6502.o: fake6502.c
	$(CC) -DDECIMALMODE -DNMOS6502 -c $(CFLAGS) fake6502.c -o $@

FRC:
