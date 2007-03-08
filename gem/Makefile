CC=gcc
CFLAGS=-g -Wall -pedantic -Werror-implicit-function-declaration -fno-common
CLIBS=

SRCS =  at_exit_funcs.c \
	error.c \
	tools.c \
	reg_deps.c \
	reg_entries.c \
	reg_html.c \
	actions.c \
	main.c

HDRS =	globals.h \
	at_exit_funcs.h \
	error.h \
	tools.h \
	reg_deps.h \
	reg_entries.h \
	reg_html.h \
	actions.h

OBJS =	at_exit_funcs.o \
	error.o \
	tools.o \
	reg_deps.o \
	reg_entries.o \
	reg_html.o \
	actions.o \
	main.o

default: $(SRCS) $(HDRS) $(OBJS)
	$(CC) $(DEBUG) $(OBJS) -o gem6 $(CFLAGS) $(CLIBS) $(LDFLAGS)

at_exit_funcs.o: at_exit_funcs.c at_exit_funcs.h globals.h
	$(CC) $(DEBUG) -c at_exit_funcs.c $(CFLAGS) $(CLIBS) $(LDFLAGS)	

error.o: error.c error.h globals.h
	$(CC) $(DEBUG) -c error.c $(CFLAGS) $(CLIBS) $(LDFLAGS)		
	
tools.o: tools.c tools.h globals.h
	$(CC) $(DEBUG) -c tools.c $(CFLAGS) $(CLIBS) $(LDFLAGS)	
	
reg_deps.o: reg_deps.c reg_deps.h globals.h
	$(CC) $(DEBUG) -c reg_deps.c $(CFLAGS) $(CLIBS) $(LDFLAGS)
	
reg_entries.o: reg_entries.c reg_entries.h globals.h
	$(CC) $(DEBUG) -c reg_entries.c $(CFLAGS) $(CLIBS) $(LDFLAGS)	
	
reg_html.o: reg_html.c reg_html.h globals.h
	$(CC) $(DEBUG) -c reg_html.c $(CFLAGS) $(CLIBS) $(LDFLAGS)	

actions.o: actions.c actions.h globals.h
	$(CC) $(DEBUG) -c actions.c $(CFLAGS) $(CLIBS) $(LDFLAGS)	
	
main.o:	main.c globals.h
	$(CC) $(DEBUG) -c main.c $(CFLAGS) $(CLIBS) $(LDFLAGS)

install: gem6
	strip gem6;
	cp gem6 /usr/local/bin

clean:
	-rm -f *.o *.bak core *~ gem6

