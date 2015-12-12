TARGET = spoopy

GG = ghc

SRC += $(wildcard *.hs)


all: $(TARGET)

test: $(TARGET)
	./$(TARGET) test.sp
	cat test.lex
	@echo ""
	cat test.parse
	@echo ""
	cat test.h
	@echo ""
	cat test.c
	@echo ""

$(TARGET): $(SRC)
	$(GG) $(GFLAGS) $^ -o $@

clean:
	rm -f $(SRC:.hs=.hi)
	rm -f $(SRC:.hs=.o)
	rm -f $(TARGET)
