#include <string.h>
#include "gis.h"
#include "gprojects.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"

struct pj_info iproj, oproj;

int main (int argc, char **argv)
{
    struct GModule *module;
    struct Flag *once, *decimal, *latlong, *wgs84;
    char s_names[2048];
    char *name;
    int have_spheroid = 0;
    int i;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description =
		"Identifies the geographic coordinates associated with "
		"point locations in the active frame on the graphics monitor.";

    once = G_define_flag() ;
    once->key        = '1' ;
    once->description= "one mouse click only";

    decimal = G_define_flag() ;
    decimal->key        = 'd' ;
    decimal->description= "Output lat/long in decimal degree";
   
    latlong = G_define_flag() ;
    latlong->key        = 'l' ;
    latlong->description= "Output lat/long referenced to current ellipsoid";

    wgs84 = G_define_flag() ;
    wgs84->key        = 'w' ;
    wgs84->description= "Output lat/long referenced to WGS84 ellipsoid using datum\n"
                        "       transformation parameters defined in current location if available";
     
     
    if (argc > 1 && G_parser(argc,argv))
	exit(1);

    if ( ((G_projection() == PROJECTION_LL) && wgs84->answer) || 
	 ((G_projection() != PROJECTION_LL) && (latlong->answer || wgs84->answer)) )
       have_spheroid = 1;
   
    if(have_spheroid == 1)
    {
       struct Key_Value *in_proj_info, *in_unit_info;
       struct Key_Value *out_proj_info, *out_unit_info;
       double a, es;
       char buff[100], dum[100];
       
       /* read current projection info */
       if ((in_proj_info = G_get_projinfo()) == NULL)
          G_fatal_error("Can't get projection info of current location");

       if ((in_unit_info = G_get_projunits()) == NULL)
          G_fatal_error("Can't get projection units of current location");

       if (pj_get_kv(&iproj, in_proj_info, in_unit_info) < 0)
          G_fatal_error("Can't get projection key values of current location");
	
       out_proj_info = G_create_key_value();
       out_unit_info = G_create_key_value();
       
       /* set output projection to lat/long */
       G_set_key_value("proj", "ll", out_proj_info);

       if(!wgs84->answer)
       {
	  /* Set output to same ellipsoid as input if we're not looking
	   * for the WGS84 values */
          G_get_ellipsoid_parameters(&a, &es);
          sprintf(buff, "%f", a);
          G_set_key_value("a", buff, out_proj_info);
          sprintf(buff, "%f", es);
          G_set_key_value("es", buff, out_proj_info);
       }
       else
       {
	  /* Check that datumparams are defined for this location (otherwise
	   * the WGS84 values would be meaningless), and if they are set the 
	   * output datum to WGS84 */
	  if( G_get_datumparams_from_projinfo(in_proj_info, buff, dum) < 0 )
	      G_fatal_error("WGS84 output not possible as this location does not contain\n"
			    "datum transformation parameters. Try running g.setproj.");
	  else
	      G_set_key_value("datum", "wgs84", out_proj_info);
       }
       
       G_set_key_value("unit", "degree", out_unit_info);
       G_set_key_value("units", "degrees", out_unit_info);
       G_set_key_value("meters", "1.0", out_unit_info);
       
       if (pj_get_kv(&oproj, out_proj_info, out_unit_info) < 0)
          G_fatal_error("Unable to set up lat/long projection parameters");       
		
       G_free_key_value( in_proj_info );
       G_free_key_value( in_unit_info );
       G_free_key_value( out_proj_info );
       G_free_key_value( out_unit_info );
    }

    if (R_open_driver() != 0)
	G_fatal_error ("No graphics device selected");
    D_setup(0);
    where_am_i(once->answer, have_spheroid, decimal->answer) ;
    R_close_driver();

    exit(0);
}
