OBJC= 	dcmp.o dblcomp.o \
	enormp.o normp.o xinormal.o \
	omnibus.o geary.o extreme.o dagstndn.o kuiprsvn.o \
	watsonun.o durbins.o andrsndn.o cvmw2n.o ksn.o \
	dmax.o dmaxe.o chisqn.o shapiro1.o shapiro2.o shapiroe.o \
	cvmw2e.o kse.o kuiprsve.o watsonue.o andrsnde.o \
	chisqe.o kotz.o \
	royston.o as181.o as177.o as241.o as66.o\
	weisberg.o

CC=gcc
CFLAGS=-DNOISY -g -Wall
ARFLAGS=ruv

libcdhc.a: $(OBJC)
#	$(RM) libcdhc.a
	$(AR) $(ARFLAGS) $@ $(OBJC)
        $(RANLIB) $@

all: cdh-f cdh-c

cdh-c :	libcdhc.a c-drive.o
	$(CC) -o $@ c-drive.o libcdhc.a -lm

cdh-f : cdh.o f-drive.o
	$(F77) -o $@ cdh.o f-drive.o -lm

bigtest: coal coalsm pine

coal: all
	-time cdh-f < data/coalash.100
	-time cdh-c < data/coalash.100

coalsm: all
	-time cdh-f < data/coalash.50
	-time cdh-c < data/coalash.50

pine: all
	-time cdh-f < data/pinetree
	-time cdh-c < data/pinetree

clean: 
	-$(RM) -rf a.out cdh-f cdh-c *.a *.o *~ *.BAK core tags LIB*

dist: clean
	-(cd ..; tar cvf cdhc.tar cdhc; gzip cdhc.tar)
