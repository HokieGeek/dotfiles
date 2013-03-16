##---------------------------------------------------
##
## Filename: $RCSfile: Makefile,v $
##
## Description:
##
## $Log: Makefile,v $
## Revision 1.1  2006/08/31 15:26:21  per47214c
## Added the new files
##
##---------------------------------------------------

CC = gcc
CFLAGS = -g -Wall

OBJS = lc.o queue.o vec.o vec_node.o

###

all: lc
	mv lc ../../lc

lc: ${OBJS}
	${CC} ${CFLAGS} -o $@ $^

lc.o: lc.c queue.h 
	${CC} ${CFLAGS} -c $<

queue.o: queue.c queue.h
	${CC} ${CFLAGS} -c $<

vec.o: vec.c vec.h vec_node.h
	${CC} ${CFLAGS} -c $<

vec_node.o: vec_node.c vec_node.h
	${CC} ${CFLAGS} -c $<

## SPECIAL

clean:
	rm -rf *.o core lc

test:
	@clear
	@make 
	@echo "---------------------------------------------"
	@lc -v
