
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

#always compile dbf driver:
SUBDIRS1 = dbf

#odbc:
ifneq ($(strip $(ODBCLIB)),)
    SUBDIRS2 = $(SUBDIRS1) odbc
else
    SUBDIRS2 = $(SUBDIRS1)
endif

#postgresql:
ifneq ($(strip $(PQLIB)),)
    SUBDIRS3 = $(SUBDIRS2) postgres
else
    SUBDIRS3 = $(SUBDIRS2)
endif

#mysql
ifneq ($(strip $(MYSQLLIB)),)
    SUBDIRS4 = $(SUBDIRS3) mysql
else
    SUBDIRS4 = $(SUBDIRS3)
endif

#add new driver here, increase SUBDIRSx number

#cp last SUBDIRSx to SUBDIRS
SUBDIRS = $(SUBDIRS3)

default: subdirs

clean: cleansubdirs
