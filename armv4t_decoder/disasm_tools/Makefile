CXX = arm-none-eabi-gcc
CFLAGS = -Wall -march=armv4t -s -mthumb --specs=nosys.specs
DISASM = arm-none-eabi-objdump
DISASM_FLAGS = -s -C -j .text -d
INPUT= test
SRC = $(INPUT).c
OBJS = $(INPUT).o
OBJS_S = $(INPUT).S

all: $(OBJS) $(OBJS_S) 

$(OBJS_S):
	$(CXX) $(CFLAGS) -S -o test.S $(SRC)

da: $(OBJS_S)
	$(DISASM) $(DISASM_FLAGS) $(OBJS) > test_da.txt

# a: $(FILES)
# 	$(CXX) $(CFLAGS) $(OBJS_S) -o test.o

# %.S: %.c
# 	$(CXX) $(CFLAGS) -o $@ -S $<

%.S: %.c
	$(CXX) $(CFLAGS) -o $@ -S $<

%.o: %.S
	$(CXX) $(CFLAGS) -o $@ -c $<


.PHONY: clean
clean:
	rm -f $(OBJS) $(OBJS_S)