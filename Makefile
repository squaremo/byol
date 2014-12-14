SRCS=lispy.c mpc/mpc.c
TARGET=lispy

.PHONY: all clean test

all: $(TARGET)

clean:
	rm -f *.o $(TARGET)

lispy: $(SRCS)
	gcc -g -std=c99 -Wall -ledit -lm -o $(TARGET) $^

test: $(TARGET) assert.sh tests.sh
	sh tests.sh

assert.sh:
	curl -sS https://raw.githubusercontent.com/lehmannro/assert.sh/master/assert.sh > ./assert.sh
