OBJ = \
    geocen1.o\
    geocen2.o\
    spheroid.o\
    utm.o\
    format_ll.o\
    scan_ll.o

AR                  = ar ruv $@ $?; ranlib $@
LIB = libcoorcnv.a

$(LIB): $(OBJ)
	$(AR)
