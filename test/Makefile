TARGET = test

CC = gcc
WC = ../wedge

WSRC += $(wildcard *.w)
CSRC += $(sort $(wildcard *.c) $(WSRC:.w=.c))
OBJ := $(CSRC:.c=.o)
DEP := $(CSRC:.c=.d)
ASM := $(CSRC:.c=.s)

CFLAGS += -O2
CFLAGS += -std=c99
#CFLAGS += -include stdio.h
CFLAGS += -Wall -Winline


all: $(TARGET)

asm: $(ASM)

-include $(DEP)


$(TARGET): $(OBJ)
	$(CC) $(CFLAGS) $^ $(LFLAGS) -o $@

.INTERMEDIATE: $(WSRC:.w=.c)
.PRECIOUS: $(WSRC:.w=.c)
%.c: %.w
	$(WC) $<

%.o: %.c
	$(CC) -c -MMD $(CFLAGS) $< -o $@

#%.s: %.c
#	$(CC) -S $(CFLAGS) $< -o $@


clean:
	rm -f $(TARGET)
	rm -f $(WSRC:.w=.c) $(WSRC:.w=.h)
	rm -f $(OBJ)
	rm -f $(DEP)
	rm -f $(ASM)
