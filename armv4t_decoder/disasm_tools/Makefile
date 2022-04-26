CXX = arm-none-eabi-gcc
CFLAGS = -Wall -march=armv4t -s -marm
DISASM = arm-none-eabi-objdump
DISASM_FLAGS = -s -C -j .text -d
OBJS = add.o


all: $(OBJS)

da: $(OBJS)
	$(DISASM) $(DISASM_FLAGS) $(OBJS) > da.txt

%.o: %.c
	$(CXX) $(CFLAGS) -o $@ -c $<


.PHONY: clean
clean:
	rm -f $(OBJS)