# definitions for SUN rim
PILE = f77
EXTRA = csystem.o system.o
LIBEXTRA = csystem.o system.o
#AR = /usr/5bin/ar cqs
#RAN = touch
AR = ar -rcv
# make rim (generic instructions)
#
# Rim executable
#
rim::
	(cd src;     make rim)
	(cd src;     make rimlib)
	$(PILE) -o rim src/rim.o src/rimlib
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
	$(PILE) -o rimh src/rimh.o src/rimlib -L. -lrim
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
	mv src/librim.a .
#
# install everything
#
install::
	install rim_help.rimdb1 /usr/local/lib
	install rim_help.rimdb2 /usr/local/lib
	install rim_help.rimdb3 /usr/local/lib
	install rim /usr/local/bin
	install librim.a /usr/lib
#
# clean-up
#
clean::
	rm -f rimh
	rm -f src/*.o
