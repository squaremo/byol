SRCS=lispy.c mpc/mpc.c
TARGET=lispy
DEBUG_TARGET=$(TARGET)-debug

.PHONY: all clean test

all: $(TARGET) $(DEBUG_TARGET)

clean:
	rm -f *.o $(TARGET) $(DEBUG_TARGET)

$(TARGET): $(SRCS)
	cc -g -std=c99 -Wall -ledit -lm -o $(TARGET) $^

$(DEBUG_TARGET): $(SRCS)
	cc -g -std=c99 -Wall -ledit -lm -DDEBUG -o $(DEBUG_TARGET) $^

test: $(TARGET) assert.sh tests.sh
	sh tests.sh

assert.sh:
	curl -sS https://raw.githubusercontent.com/lehmannro/assert.sh/master/assert.sh > ./assert.sh
