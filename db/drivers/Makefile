
MODULE_TOPDIR = ../..

include $(MODULE_TOPDIR)/include/Make/Dir.make

#always compile dialog lib and dbf driver:
SUBDIRS1 = dbf

# add odbc:
ifneq ($(strip $(ODBCLIB)),)
    SUBDIRS2 = odbc
endif

# add postgresql:
ifneq ($(strip $(PQLIB)),)
    SUBDIRS3 = postgres
endif

# add mysql:
ifneq ($(strip $(MYSQLLIB)),)
    SUBDIRS4 = mysql
endif

# add ogr:
ifneq ($(strip $(USE_OGR)),)
    SUBDIRS5 = ogr
endif

SUBDIRS =  $(SUBDIRS1) $(SUBDIRS2) $(SUBDIRS3) $(SUBDIRS4) $(SUBDIRS5)

default: subdirs

clean: cleansubdirs
