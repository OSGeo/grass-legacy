use ExtUtils::MakeMaker;

$grass_dir     = "/ssi0/ssi/neteler/grass61/";
$grass_arch    = 'i686-pc-linux-gnu';

$grass_inc_dir = "$grass_dir/include";
$grass_lib_dir = "$grass_dir/dist.$grass_arch/lib";

WriteMakefile(
	      NAME => 'Grass',
	      LIBS => "-L$grass_lib_dir -lgrass_datetime -lgrass_gis -lgrass_gmath -lgrass_linkm -lgrass_raster -lgrass_bitmap -lgrass_btree -lgrass_display -lgrass_D -lgrass_dbmibase -lgrass_dbmiclient -lgrass_dbstubs -lgrass_dbmidriver -lgrass_sqlp -lgrass_vask -lgrass_edit -lgrass_shape -lgrass_form -lgrass_image -lgrass_I -lgrass_gproj -lgrass_rowio -lgrass_segment -lgrass_rtree -lgrass_dgl -lgrass_dig2 -lgrass_vect -lgrass_trans -lgrass_sites -lgrass_interpdata -lgrass_qtree -lgrass_interpfl -lgrass_g3d -lgrass_dspf -lgrass_symb -lgrass_cdhc -lgrass_ogsf -lgrass_Iortho",
	      OBJECT => 'grass_wrap.o R_slope_aspect/r_slope_aspect/opennew.o R_slope_aspect/r_slope_aspect/r_slope_aspect.o',
	      INC => "-I$grass_inc_dir"
	      );

sub MY::postamble {
'
grass_wrap.c: grass.i typemaps.i
	swig -Wall -perl5 -o grass_wrap.c grass.i 
';
}
