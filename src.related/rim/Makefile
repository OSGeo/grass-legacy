# definitions for LINUX rim
RANLIB=ranlib
F2CLIB=/usr/lib/f2cmain.a -lf2c
F77=g77
EXTRA=time_date.o
LIBEXTRA=time_date.o
PILE = CC_f2c=$(CC) $(F77)
LIBRIM=librim.a
AR = ar rv
RAN = ranlib
# make rim (generic instructions to be customized for each installation)
#
# Rim executable
#
rim::
	(cd src;     make rim)
	(cd src;     make rimlib)
#	$(PILE) -o rim src/rim.o src/rimlib
	$(PILE) -Bstatic -o rim src/rim.o src/rimlib
#
# Rim file editor (for debugging)
#
rime::
	(cd src;     make rime)
	(cd src;     make rimlib)
	$(PILE) -o rime src/rime.o src/rimlib
#
# Rim help database generator
#
rimh::
	(cd src;     make rimh)
	(cd src;     make rimlib)
	(cd src;     make librim.a)
	$(PILE) -o rimh src/rimh.o src/rimlib -Lsrc -lrim
#
# Program to test text i/o and parsing routines
#
lxtest::
	(cd src;     make lxtest)
	(cd src;     make rimlib)
	$(PILE) -o lxtest src/lxtest.o src/rimlib
#
# Help database
#
help::
	(make rimh)
	(rm -f rim_help.rimdb1)
	(rm -f rim_help.rimdb2)
	(rm -f rim_help.rimdb3)
	rim src/rim_help.schema
	rimh
#
# archive library for the user callable library
#
librim.a::
	(cd src;     make librim.a)
	cp src/librim.a .
#
# install everything
#
install::
	install rim_help.rimdb1 /usr/local/lib
	install rim_help.rimdb2 /usr/local/lib
	install rim_help.rimdb3 /usr/local/lib
	install rim /usr/local/bin
	install librim.a /usr/lib
	install rim.1 /usr/man/manl/rim.l
#
# clean-up
#
clean::
	rm -f rimh
	rm -f lxtest
	rm -f src/*.o
	rm -f src/rimlib
	rm -f doc/*.dvi *.log
