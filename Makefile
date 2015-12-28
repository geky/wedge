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

test: $(TARGET)
	./$(TARGET) test.w
	cat test.lex
	cat test.parse
	cat test.h
	cat test.c

$(TARGET): $(SRC)
	$(HC) $(HFLAGS) $(MAIN) -o $@

repl: $(SRC)
	$(HI) $(HFLAGS) $(MAIN)

clean:
	rm -rf $(TMPDIR)
	rm -f $(TARGET)
