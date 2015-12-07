TARGET = spoopy

GG = ghc

SRC += $(wildcard *.hs)

GFLAGS += -Wall


all: $(TARGET)

test: $(TARGET)
	./$(TARGET) test.sp
	cat test.lex
	@echo ""
	cat test.parse
	@echo ""

$(TARGET): $(SRC)
	$(GG) $(GFLAGS) $^ -o $@

clean:
	rm -f $(SRC:.hs=.hi)
	rm -f $(SRC:.hs=.o)
	rm -f $(TARGET)
