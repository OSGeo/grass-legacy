MODULE_TOPDIR = ../..

PGM = db.test

include $(MODULE_TOPDIR)/include/Make/Script.make

default: script tests

tests:
	if [ ! -d $(ETC)/db.test ]; then $(MKDIR) $(ETC)/db.test; fi
	$(INSTALL) -m 644 test* $(ETC)/db.test/
