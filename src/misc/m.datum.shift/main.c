/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       m.datum.shift
 * AUTHOR(S):    Michael Shapiro, CERL
 *               Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 Convert latitude/longitude from one datum to another
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include "gis.h"
#include "CC.h"

/* if you want support for Bursa Wolf transformation,
 * uncomment the following: */
/* #define BURSAWOLF */

int
main (int argc, char *argv[]) 
{
    double lat, lon, h, lat2, lon2, h2;
    double dx, dy, dz;
    double ia, ie;
    double oa, oe;

    struct
    {
      struct Option *lat, *lon, *h, *id, *od, *is, *os, *dx, *dy, *dz
#ifdef BURSAWOLF
      ,*rx, *ry, *rz, *m
#endif
      ;
    } parm;

    struct
    { 
      struct Flag *method_b, *method_m, *method_w;
    } flags; 

    struct GModule *module;

    enum method { NO_M, BLOCK_M, MOLOD_M, BURSA_M };

    int use_method;
    int datum_spheroid;
    int idatum = -1, odatum = -1; 

    char buf[300];
    char *sphlist, *spheroid_list(), *datumlist, *datum_list();

#ifdef BURSAWOLF
    char *isname, *osname;
    double Rx, Ry, Rz, M, Sa, Se2, Da, De2, dummy;
#endif

    (void) G_gisinit(argv[0]);

    module = G_define_module();
    module->description =
      "Datum shift program. Return geographic coordinates "
      "based on a different datum that the one used to obtain "
      "the original coordinates. "; 

    sphlist = spheroid_list();
    datumlist = datum_list();

    /* TODO: test if argv[0] is m.datum.shift, m.datum.shift3 or m.datum.shift7
     * and define flags and options accordingly. 
     * Is it possible with grass to have a program that behaves on 
     * different calling names accordingly?
     */
    
    flags.method_b = G_define_flag();
    flags.method_b->key = 'b';
    flags.method_b->description = "use block shift transformation (default)";
    flags.method_b->answer = 'b';

    flags.method_m = G_define_flag();
    flags.method_m->key = 'm';
    flags.method_m->description = "use molodensky transformation";
    flags.method_m->answer = '\0';

    flags.method_w = G_define_flag();
    flags.method_w->key = 'w';
    flags.method_w->description = "use bursa wolf 3d similarity transformation (not implemented)";
    flags.method_w->answer = '\0';

    parm.lat = G_define_option();
    parm.lat->key = "latitude";
    parm.lat->key_desc = G_lat_format_string();
    parm.lat->type = TYPE_STRING;
    parm.lat->required = YES;
    parm.lat->description = "latitude";

    parm.lon = G_define_option();
    parm.lon->key = "longitude";
    parm.lon->key_desc = G_lon_format_string();
    parm.lon->type = TYPE_STRING;
    parm.lon->required = YES;
    parm.lon->description = "longitude";

    parm.h = G_define_option();
    parm.h->key = "height";
    parm.h->type = TYPE_DOUBLE;
    parm.h->required = NO;
    parm.h->description = "height above ellipsoid";
    parm.h->answer = 0;

    parm.is = G_define_option();
    parm.is->key = "ispheroid";
    parm.is->type = TYPE_STRING;
    parm.is->required = NO;
    parm.is->options = sphlist;
    parm.is->description = "input spheroid";
    /* parm.is->answer = "wgs84"; */

    parm.os = G_define_option();
    parm.os->key = "ospheroid";
    parm.os->type = TYPE_STRING;
    parm.os->required = NO;
    parm.os->options = sphlist;
    parm.os->description = "output spheroid";
    parm.os->answer = "wgs84";

    parm.id = G_define_option();
    parm.id->key = "idatum";
    parm.id->type = TYPE_STRING;
    parm.id->required = NO;
    parm.id->options = datumlist;
    parm.id->description = "input datum";
    /* parm.id->answer = "wgs84"; */

    parm.od = G_define_option();
    parm.od->key = "odatum";
    parm.od->type = TYPE_STRING;
    parm.od->required = NO;
    parm.od->options = datumlist;
    parm.od->description = "output datum";
    parm.od->answer = "wgs84";

    parm.dx = G_define_option();
    parm.dx->key = "dxshift";
    parm.dx->type = TYPE_DOUBLE;
    parm.dx->required = NO;
    parm.dx->description = "x shift";
    parm.dx->answer = "0.0";

    parm.dy = G_define_option();
    parm.dy->key = "dyshift";
    parm.dy->type = TYPE_DOUBLE;
    parm.dy->required = NO;
    parm.dy->description = "y shift";
    parm.dy->answer = "0.0";

    parm.dz = G_define_option();
    parm.dz->key = "dzshift";
    parm.dz->type = TYPE_DOUBLE;
    parm.dz->required = NO;
    parm.dz->description = "z shift";
    parm.dz->answer = "0.0";

#ifdef BURSAWOLF
    parm.rx = G_define_option();
    parm.rx ->key= "Rx";
    parm.rx ->type= TYPE_DOUBLE;
    parm.rx ->required = NO;
    parm.rx ->description = "Rx rotational parameter";
    parm.rx ->answer = "0.0";
    
    parm.ry = G_define_option();
    parm.ry ->key= "Ry";
    parm.ry ->type= TYPE_DOUBLE;
    parm.ry ->required = NO;
    parm.ry ->description = "Ry rotational parameter";
    parm.ry ->answer = "0.0";
    
    parm.rz = G_define_option();
    parm.rz ->key= "Rz";
    parm.rz ->type= TYPE_DOUBLE;
    parm.rz ->required = NO;
    parm.rz ->description = "Rz rotational parameter";
    parm.rz ->answer = "0.0";
    
    parm.m  = G_define_option();
    parm.m ->key= "M";
    parm.m ->type= TYPE_DOUBLE;
    parm.m ->required = NO;
    parm.m ->description = "Scale factor M (in ppm)";
    parm.m ->answer = "0.0";   
#endif

    if (G_parser(argc,argv))
	exit(1);

    if (!G_scan_northing (parm.lat->answer, &lat, PROJECTION_LL))
    {
	sprintf (buf, "%s=%s - illegal latitude\n", 
		 parm.lat->key, parm.lat->answer);
	G_fatal_error(buf);
    }

    if (!G_scan_easting (parm.lon->answer, &lon, PROJECTION_LL))
    {
	sprintf (buf, "%s=%s - illegal longitude\n", 
		 parm.lon->key, parm.lon->answer);
	G_fatal_error(buf);
    }

    if (parm.h->answer != NULL) {
      sscanf (parm.h->answer, "%lf", &h);
    } else {
      h = 0.0;
    }

    /* test:    (input datum && output datum) 
     *       || (input sph && output sph && shift params) 
     */

    datum_spheroid = -1;
    use_method = BLOCK_M;
    if ( ((parm.id->answer != NULL) && (parm.od->answer != NULL)) ) 
      {
	datum_spheroid = 1;
	
	if (flags.method_b->answer)
	  use_method = BLOCK_M;
	else if (flags.method_m->answer)
	  use_method = MOLOD_M;
	else if (flags.method_w->answer)
	  use_method = BURSA_M;
	
	/* get datum names/parameters */
	if ( (idatum = CC_get_datum_by_name(parm.id->answer)) < 0)
	  {
	    sprintf (buf, "%s=%s - unknown input datum\n", 
		     parm.id->key, parm.id->answer);
	    G_fatal_error(buf);
	  }
	if ( (odatum = CC_get_datum_by_name(parm.od->answer)) < 0)
	  {
	    sprintf (buf, "%s=%s - unknown output datum\n", 
		     parm.od->key, parm.od->answer);
	    G_fatal_error(buf);
	  } 
      } else if ( (parm.is->answer != NULL) &&
		  (parm.os->answer != NULL) &&
		  (parm.dx->answer != NULL) &&
		  (parm.dy->answer != NULL) &&
		  (parm.dz->answer != NULL) )
	{
	  datum_spheroid = 2;
	  
	  sscanf (parm.dx->answer, "%lf", &dx);
	  sscanf (parm.dy->answer, "%lf", &dy);
	  sscanf (parm.dz->answer, "%lf", &dz);
	  
	  if ( CC_get_spheroid(parm.is->answer, &ia, &ie) == 0)
	    {
	      sprintf (buf, "%s=%s - unknown input spheroid\n", 
		       parm.is->key, parm.is->answer);
	      G_fatal_error(buf);
	      
	    }
	  if ( CC_get_spheroid(parm.os->answer, &oa, &oe) == 0)
	    {
	      sprintf (buf, "%s=%s - unknown output spheroid\n", 
		       parm.os->key, parm.os->answer);
	      G_fatal_error(buf);
	    } 
#ifdef BURSAWOLF
	  if ( (parm.rx->answer != NULL) &&
	       (parm.ry->answer != NULL) &&
	       (parm.rz->answer != NULL) &&
	       (parm.m->answer  != NULL))
	    {
	      datum_spheroid = 3;
	    } 
#endif
	} else {
	  /* wrong values/parameters */
	  sprintf(buf, "wrong combination of values/parameters!\n");
	  G_usage();
	  G_fatal_error(buf);
	}
    
    /* we have datum names */
    if (datum_spheroid == 1) {
      switch (use_method) 
	{
	case BLOCK_M: 
	  if (CC_datum_to_datum_shift_CC(idatum, lat, lon, h, 
					 odatum, &lat2, &lon2, &h2) == 0)
	    G_fatal_error("Error in CC blockshift routine\n");
	  break;
	case BURSA_M:
	  G_fatal_error("Bursa Wolf routine not implemented\n");
	  /*
	   * if (CC_datum_to_datum_shift_BW(idatum, lat, lon, h, 
	                                    odatum, &lat2, &lon2, &h2) == 0)
	   *  G_fatal_error("Error in Bursa Wolf routine\n");
	  */
	  break;
	case MOLOD_M:
	  if (CC_datum_to_datum_shift_M(idatum, lat, lon, h, 
					odatum, &lat2, &lon2, &h2) == 0)
	    G_fatal_error("Error in Molodensky routine\n");
	  break;
	default:
	  G_fatal_error("Unknown Error, should not get here!\n");
	}
    } else if (datum_spheroid == 2)
      {
	/* we have spheroid parameters & dx dy dz */
	if (CC_datum_shift_CC(lat, lon, h, ia, ie, 
			      &lat2, &lon2, &h2, oa, oe, dx, dy, dz) == 0)
	  G_fatal_error("Error in CC blockshift routine\n");
      } else if (datum_spheroid == 3)
	{
#ifdef BURSAWOLF	  
	  if (CC_get_spheroid_by_name(parm.is->answer, &Sa, &Se2, &dummy) == 0)
	    G_fatal_error("Unknown input spheroid");
	  if (CC_get_spheroid_by_name(parm.os->answer, &Da, &De2, &dummy) == 0)
	    G_fatal_error("Unknown output spheroid");
	  
	  sscanf (parm.rx->answer, "%lf", &Rx);
	  sscanf (parm.ry->answer, "%lf", &Ry);
	  sscanf (parm.rz->answer, "%lf", &Rz);
	  sscanf (parm.m->answer,  "%lf", &M );	 
	  
	  if (CC_datum_shift_BursaWolf(lat, lon, h, 
				       Sa, Se2, 
				       &lat2, &lon2, &h2, 
				       Da, De2, 
				       dx, dy, dz, 
				       Rx, Ry, Rz, M) == 0)
	    G_fatal_error("Error in Bursa Wolf routine\n");
#else
	  G_fatal_error("Bursa Wolf routine not implemented\n");
#endif
        } else {
	  G_usage();
	  G_fatal_error("Unknown Error, should not get here!\n");
	}
    
    G_format_northing (lat2, buf, PROJECTION_LL);
    printf ("\n lat=%s\n", buf);

    G_format_easting (lon2, buf, PROJECTION_LL);
    printf (" lon=%s\n", buf);

    printf ("(h  =%5.0f [m])\n", h2);

    exit(0);
}
