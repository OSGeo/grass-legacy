MODULE_TOPDIR = ../..

LIB_NAME = $(GPROJ_LIBNAME)

EXTRA_CFLAGS = $(PICFLAGS)
EXTRA_INC = $(PROJINC)

LIB_OBJS = get_proj.o do_proj.o

NAD_DIR = $(GISBASE)/etc/nad

NAD_TABLES = \
	$(NAD_DIR)/FL		\
	$(NAD_DIR)/MD		\
	$(NAD_DIR)/TN		\
	$(NAD_DIR)/WI		\
	$(NAD_DIR)/WO		\
	$(NAD_DIR)/alaska	\
	$(NAD_DIR)/conus	\
	$(NAD_DIR)/hawaii	\
	$(NAD_DIR)/prvi		\
	$(NAD_DIR)/stgeorge	\
	$(NAD_DIR)/stlrnc	\
	$(NAD_DIR)/stpaul	\
	$(NAD_DIR)/ntv1_can.dat

include $(MODULE_TOPDIR)/include/Make/Lib.make 

default: lib $(NAD_TABLES) 

$(NAD_DIR):
	mkdir $(NAD_DIR)

$(NAD_DIR)/FL:		$(NAD_DIR) FL.lla
	nad2bin < FL.lla $@

$(NAD_DIR)/MD:		$(NAD_DIR) MD.lla
	nad2bin < MD.lla $@

$(NAD_DIR)/TN:		$(NAD_DIR) TN.lla
	nad2bin < TN.lla $@

$(NAD_DIR)/WI:		$(NAD_DIR) WI.lla
	nad2bin < WI.lla $@

$(NAD_DIR)/WO:		$(NAD_DIR) WO.lla
	nad2bin < WO.lla $@

$(NAD_DIR)/alaska:	$(NAD_DIR) alaska.lla
	nad2bin < alaska.lla $@

$(NAD_DIR)/conus:	$(NAD_DIR) conus.lla
	nad2bin < conus.lla $@

$(NAD_DIR)/hawaii:	$(NAD_DIR) hawaii.lla
	nad2bin < hawaii.lla $@

$(NAD_DIR)/prvi:	$(NAD_DIR) prvi.lla
	nad2bin < prvi.lla $@

$(NAD_DIR)/stgeorge:	$(NAD_DIR) stgeorge.lla
	nad2bin < stgeorge.lla $@

$(NAD_DIR)/stlrnc:	$(NAD_DIR) stlrnc.lla
	nad2bin < stlrnc.lla $@

$(NAD_DIR)/stpaul:	$(NAD_DIR) stpaul.lla
	nad2bin < stpaul.lla $@

$(NAD_DIR)/ntv1_can.dat:	$(NAD_DIR) ntv1_can.dat
	cp -f ntv1_can.dat $@

