SRCS=lispy.c mpc/mpc.c
TARGET=lispy

.PHONY: all clean

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

lispy: $(SRCS)
	gcc -std=c99 -Wall -ledit -lm -o $(TARGET) $^
