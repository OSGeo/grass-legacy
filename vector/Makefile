
MODULE_TOPDIR = ..

SUBDIRS = \
	v.buffer2 \
	v.build \
	v.build.polylines \
	v.category \
	v.class \
	v.clean \
	v.convert \
	v.db.connect \
	v.db.select \
	v.delaunay2 \
	v.distance \
	v.drape \
	v.edit \
	v.extract \
	v.extrude \
	v.generalize \
	v.hull \
	v.info \
	v.in.ascii \
	v.in.db \
	v.in.dxf \
	v.in.region \
	v.in.sites \
	v.kcv \
	v.kernel \
	v.label \
	v.lrs \
	v.proj \
	v.mkgrid \
	v.neighbors \
	v.net \
	v.net.alloc \
	v.net.allpairs \
	v.net.bridge \
	v.net.centrality \
	v.net.components \
	v.net.connectivity \
	v.net.distance \
	v.net.flow \
	v.net.iso \
	v.net.path \
	v.net.salesman \
	v.net.spanningtree \
	v.net.steiner \
	v.net.timetable \
	v.net.visibility \
	v.normal \
	v.out.ascii \
	v.out.dxf \
	v.out.pov \
	v.out.svg \
	v.out.vtk \
	v.overlay \
	v.parallel2 \
	v.patch \
	v.perturb \
	v.split \
	v.qcount \
	v.random \
	v.reclass \
	v.sample \
	v.segment \
	v.select \
	v.support \
	v.surf.idw \
	v.surf.rst \
	v.transform \
	v.to.3d \
	v.to.db \
	v.to.points \
	v.to.rast \
	v.to.rast3 \
	v.type \
	v.univar \
	v.vect.stats \
	v.voronoi \
	v.what \
	v.what.rast\
	v.vol.rst \
	lidar

include $(MODULE_TOPDIR)/include/Make/Platform.make

OGRBASED = v.out.ogr v.in.ogr v.external

#compile if OGR present:
ifneq ($(USE_OGR),)
    SUBDIRS += $(OGRBASED)
endif

TCLTKBASED = v.digit

#compile if Tcl/Tk present:
ifneq ($(strip $(TCLTKLIBS)),)
    SUBDIRS += $(TCLTKBASED)
endif

REQUIRESFT = v.label.sa

ifneq ($(strip $(FTLIB)),)
    SUBDIRS += $(REQUIRESFT)
endif

OPENDWGBASED = v.in.dwg

#compile if OPENDWG present:
ifneq ($(USE_OPENDWG),)
    SUBDIRS += $(OPENDWGBASED)
endif

PGM = vectorintro

include $(MODULE_TOPDIR)/include/Make/Dir.make

default: htmldir

htmldir: parsubdirs

clean: cleansubdirs
