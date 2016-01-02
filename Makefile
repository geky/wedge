TARGET = $(BINDIR)/wedge

HC = ghc
HI = ghci
HFLAGS += -W
HFLAGS += -XLambdaCase -XRankNTypes

SRCDIR = src
TMPDIR = build
BINDIR = .

MAIN = $(SRCDIR)/Main.hs
SRC += $(wildcard $(SRCDIR)/*.hs)

HFLAGS += -i$(SRCDIR) -odir $(TMPDIR) -hidir $(TMPDIR)


all: $(TARGET)

.PHONY: test
test: $(TARGET)
	make --no-print-directory -C test clean
	make --no-print-directory -C test test.c
	cat test/test.h
	cat test/test.c
	make --no-print-directory -C test all
	test/test

$(TARGET): $(SRC)
	$(HC) $(HFLAGS) $(MAIN) -o $@

repl: $(SRC)
	$(HI) $(HFLAGS) $(MAIN)

clean:
	rm -rf $(TMPDIR)
	rm -f $(TARGET)
