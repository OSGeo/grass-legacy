
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

ifeq ($(strip $(ODBCLIB)),)
    SUBDIRS = dbf postgres
else
    SUBDIRS = dbf odbc postgres
endif

default: subdirs

clean: cleansubdirs

