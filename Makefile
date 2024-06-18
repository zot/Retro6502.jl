CC=gcc
CFLAGS=-g -Werror -pedantic -fPIC
SPEEDFLAGS=-O3 -DFAKE6502_OPS_STATIC -DDECIMALMODE -DNMOS6502
#SPEEDFLAGS=-DFAKE6502_OPS_STATIC -DDECIMALMODE -DNMOS6502
LIBS=C/fake6502.o C/fake6502host.o
FAKE6502=C/fake6502.c C/fake6502.h
SHLIB=C/fake6502.so
RESOURCES=resources resources/characters.bin resources/basic-kernal.bin
PRGS=examples/condensed.prg
DERIVED=C/*.o C/*.so C/LICENSE.* C/fake6502.c C/fake6502.h resources

default: $(SHLIB) $(RESOURCES)

test: default $(PRGS) FRC
	julia --project=. -e 'using Fake6502; Fake6502.test()'

cspeed: C/speedtest
	C/speedtest

C/speedtest: default
	$(CC) $(SPEEDFLAGS) -o C/speedtest C/speedtest.c C/fake6502.c

speed: default examples/speed.prg FRC
	julia +release -O3 --project=. -e 'using Fake6502; Fake6502.speed_test()'

speed2: default examples/speed.prg FRC
	julia +release --project=. -e 'using Fake6502; Fake6502.speed_test()'

%.prg: %.tass
	64tass --vice-labels -a --cbm-prg -o $@ -L $(patsubst %.prg,%.list,$@) -l $(patsubst %.prg,%.labels,$@) $<

$(FAKE6502):
	git clone git@github.com:omarandlorraine/fake6502.git FAKE6502
	cp FAKE6502/fake6502.[ch] C
	cp FAKE6502/LICENSE C/LICENSE.fake6502
	rm -rf FAKE6502

c64: default $(PRGS) FRC
	julia -t 6 --project=. -e 'using Fake6502; Fake6502.UI.test_c64()'

c64-revise: default $(PRGS) FRC
	julia -t 6 --project=. -e 'using Revise; using Fake6502; Fake6502.UI.test_c64(; revise = true)'

worker: default $(PRGS) FRC
	julia -t 6 --project=. -e 'using Fake6502; Fake6502.Workers.test_worker()'

asm: default $(PRGS) FRC
	julia -t 6 --project=. -e 'using Fake6502; Fake6502.Asm.test()'

resources:
	mkdir -p resources

resources/characters.bin:
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/characters/c64.bin > $@

resources/basic-kernal.bin:
	curl https://www.zimmers.net/anonftp/pub/cbm/firmware/computers/c64/64c.251913-01.bin > $@

clean: FRC
	rm -rf $(DERIVED)

$(SHLIB): $(LIBS)
	$(CC) -rdynamic -shared -o $(SHLIB) $(LIBS)

C/fake6502host.o: $(FAKE6502)
	$(CC) -O3 -c C/fake6502host.c -o $@

C/fake6502.o: $(FAKE6502)
	$(CC) -O3 -DFAKE6502_OPS_STATIC -DDECIMALMODE -DNMOS6502 -c $(CFLAGS) C/fake6502.c -o $@

FRC:
