
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

ifeq ($(strip $(ODBCINC)),)
    SUBDIRS = dbf
else
    SUBDIRS = dbf odbc
endif

default: subdirs

clean: cleansubdirs

