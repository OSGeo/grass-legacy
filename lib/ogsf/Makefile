MODULE_TOPDIR = ../..

EXTRA_CFLAGS= $(XCFLAGS) $(TIFFINCPATH)

LIB_NAME = $(OGSF_LIBNAME)

LDFLAGS=

LIB_OBJS = GS2.o GSX.o GS_util.o Gs3.o gs.o gsd_wire.o gsd_surf.o gsd_views.o \
	gsd_prim.o \
	gs_bm.o gs_norms.o gs_query.o gsd_objs.o gsd_img.o gsd_img_ppm.o \
	gsd_img_tif.o gsd_cplane.o trans.o \
	GV2.o Gv3.o gv.o gvd.o gv_quick.o gsdrape.o GP2.o Gp3.o gp.o \
	gpd.o gsds.o \
	GK2.o gsdiff.o gsget.o gk.o \
	gsd_label.o gsd_fonts.o   \
 	Gvol.o gvl.o GVL2.o

include $(MODULE_TOPDIR)/include/Make/Lib.make

EXTRA_INC = $(VECT_INC)
EXTRA_CFLAGS = $(VECT_CFLAGS)

default: lib inc

inc:
	cp gsurf.h $(ARCH_INCDIR)/gsurf.h
	cp kftypes.h $(ARCH_INCDIR)/kftypes.h
	cp keyframe.h $(ARCH_INCDIR)/keyframe.h
	cp local_proto.h $(ARCH_INCDIR)/ogsf_proto.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_INCDIR)/gstypes.h

