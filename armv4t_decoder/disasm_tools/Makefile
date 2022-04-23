CXX = arm-none-eabi-gcc
CFLAGS = -Wall -march=armv4t -s -marm
OBJS = add.o


all: $(OBJS)

%.o: %.c
	$(CXX) $(CFLAGS) -o $@ -c $<


.PHONY: clean
clean:
	rm -f $(OBJS)