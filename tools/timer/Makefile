
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Module.make

PGM = current_time_s_ms

default: $(PGM)

$(PGM): main.c
	$(CC) $(CFLAGS) -o $(ETC)/$(PGM) -c $< $(LDFLAGS)

clean:
	\rm -f $(ETC)/$(PGM)
