
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Module.make

default: current_time_s_ms

current_time_s_ms: current_time_s_ms.o
	$(CC) $(CFLAGS) -o $@ $^ $(LDFLAGS)
	\mv current_time_s_ms $(ETC)
	\rm -f current_time_s_ms.o

current_time_s_ms.o: main.c
	$(CC) $(CFLAGS) -o $@ -c $<
