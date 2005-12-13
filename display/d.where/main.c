#include <string.h>
#include "gis.h"
#include "gprojects.h"
#include "display.h"
#include "raster.h"
#include "local_proto.h"
#include "glocale.h"

struct pj_info iproj, oproj;

int main (int argc, char **argv)
{
    struct GModule *module;
    struct Flag *once, *decimal, *latlong, *wgs84, *dcoord;
    int have_spheroid = 0;

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description =
		_("Identifies the geographic coordinates associated with "
		"point locations in the active frame on the graphics monitor.");

    once = G_define_flag() ;
    once->key        = '1' ;
    once->description= _("One mouse click only");

    decimal = G_define_flag() ;
    decimal->key        = 'd' ;
    decimal->description= _("Output lat/long in decimal degree");
   
    latlong = G_define_flag() ;
    latlong->key        = 'l' ;
    latlong->description= _("Output lat/long referenced to current ellipsoid");

    wgs84 = G_define_flag() ;
    wgs84->key        = 'w' ;
    wgs84->description= _("Output lat/long referenced to WGS84 ellipsoid using datum "
                        "transformation parameters defined in current location (if available)");
     
    dcoord = G_define_flag() ;
    dcoord ->key	 = 'f';
    dcoord ->description= _("Output frame coordinates of current display monitor (percentage)");


    /* if (G_parser(argc,argv)) */
    if (argc > 1 && G_parser(argc,argv))
        exit(1);

    if(latlong->answer && wgs84->answer)
	G_fatal_error(_("Ambiguous request for lat/long ellipsoids"));

    if(decimal->answer && !(latlong->answer || wgs84->answer))
	G_fatal_error(_("Please specify a lat/long ellipsoid with -l or -w"));

    if ( ((G_projection() == PROJECTION_LL) && wgs84->answer) || 
	 ((G_projection() != PROJECTION_LL) && (latlong->answer || wgs84->answer)) )
       have_spheroid = 1;
   
    if(have_spheroid == 1)
    {
       struct Key_Value *in_proj_info, *in_unit_info;
       char buff[100], dum[100];
       
       /* read current projection info */
       if ((in_proj_info = G_get_projinfo()) == NULL)
          G_fatal_error(_("Can't get projection info of current location"));

       if ((in_unit_info = G_get_projunits()) == NULL)
          G_fatal_error(_("Can't get projection units of current location"));

       if (pj_get_kv(&iproj, in_proj_info, in_unit_info) < 0)
          G_fatal_error(_("Can't get projection key values of current location"));
	

       if(!wgs84->answer)
       {
	  /* Set output to same ellipsoid as input if we're not looking
	   * for the WGS84 values */
	  oproj.zone = 0;
	  oproj.meters = 1.;
	  sprintf(oproj.proj, "ll");
	  if ((oproj.pj = pj_latlong_from_proj(iproj.pj)) == NULL)
	     G_fatal_error(_("Unable to set up lat/long projection parameters"));            

       }
       else
       {
          struct Key_Value *out_proj_info, *out_unit_info;
	  
          out_proj_info = G_create_key_value();
          out_unit_info = G_create_key_value();
       
          /* set output projection to lat/long */
          G_set_key_value("proj", "ll", out_proj_info);
	  
	  /* Check that datumparams are defined for this location (otherwise
	   * the WGS84 values would be meaningless), and if they are set the 
	   * output datum to WGS84 */
	  if( G_get_datumparams_from_projinfo(in_proj_info, buff, dum) < 0 )
	      G_fatal_error(_("WGS84 output not possible as this location does not contain\n"
			    "datum transformation parameters. Try running g.setproj."));
	  else
	      G_set_key_value("datum", "wgs84", out_proj_info);
	  
          G_set_key_value("unit", "degree", out_unit_info);
          G_set_key_value("units", "degrees", out_unit_info);
          G_set_key_value("meters", "1.0", out_unit_info);
       
          if (pj_get_kv(&oproj, out_proj_info, out_unit_info) < 0)
             G_fatal_error(_("Unable to set up lat/long projection parameters"));
	  
          G_free_key_value( out_proj_info );
          G_free_key_value( out_unit_info );
       }
       		
       G_free_key_value( in_proj_info );
       G_free_key_value( in_unit_info );
       
    }

    if (R_open_driver() != 0)
	G_fatal_error (_("No graphics device selected"));
    D_setup(0);
    where_am_i(once->answer, have_spheroid, decimal->answer, wgs84->answer, dcoord->answer);
    R_close_driver();

    exit(0);
}
