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

default: lib inc

inc:
	cp -L gsurf.h $(ARCH_INCDIR)/gsurf.h
	cp -L kftypes.h $(ARCH_INCDIR)/kftypes.h
	cp -L keyframe.h $(ARCH_INCDIR)/keyframe.h
	cp -L local_proto.h $(ARCH_INCDIR)/ogsf_proto.h
	cat gstypes.h | sed -e 's/local_proto.h/ogsf_proto.h/' > $(ARCH_INCDIR)/gstypes.h

