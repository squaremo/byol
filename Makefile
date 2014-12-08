.PHONY: all

all: lispy

lispy: lispy.c
	gcc -Wall -ledit -o $@ $^
