/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:      s.datum.shift
 * AUTHOR(S):   Andreas Lange - andreas.lange@rhein-main.de
 *              mainly stolen from s.proj of Bill Brown 
 * PURPOSE: 	testing program for coordinate conversion library,
 *              datum shifting part. The program can be used to shift 
 *              a site file from one datum to another. This function
 *              should later go into s.proj. 
 * COPYRIGHT:   (C) 2000 by the GRASS Development Team
 *
 *              This program is free software under the GNU General Public
 *   	    	License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	for details.
 *
 *****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "projects.h"
#include "CC.h"
#include "local_proto.h"


FILE *infile, *outfile;

int
main(int argc, char **argv)
{

  char *me;

  struct GModule *module;

  char errbuf[256];          /* buffer for error messages */

  char *datumlist, *mapset, 
    *name, *outdatum;

  struct Option *in, *out, 
    *idatum, *odatum;        /* option structures */

  struct Flag *molod;        /* flag for use of molodensky formula */

  struct pj_info pjmap, 
    pjll;                    /* projection structure for map and lat/lon*/

  struct Key_Value 
    *proj_info_map, *unit_info_map; 
                             /* proj and unit information */
  
  Site *si;                  /* site structure */

  int proj, id, od; 
  double xcoord, ycoord, dummy;


  int (*funcp)();             /* define a pointer to a function 
			       * (which returns an integer) 
			       * for choosing formula */
  /* (int, double, double, double, double, int, double*, double*, double*); */
  /* pheew, that would the correct type be */

  G_gisinit(me = argv[0]);

  datumlist = datum_list();
  proj = G_projection();
  /* mapset = G_store(G_mapset()); */

  module = G_define_module();

  module->description = 
    "Shift the coordinates of a site file from one "
    "map datum to another map datum. "
    "This does a datum transformation within the "
    "same mapset. ";

  in = G_define_option();
  in->key = "input";
  in->type = TYPE_STRING;
  in->required = YES;
  in->gisprompt = "old,site_lists,sites";
  in->description = "input sites list";
  
  out = G_define_option();
  out->key = "output";
  out->type = TYPE_STRING;
  out->required = YES;
  out->gisprompt ="new,site_lists,sites";
  out->description = "output sites list";
  
  idatum = G_define_option();
  idatum->key = "idatum";
  idatum->type = TYPE_STRING;
  idatum->options = datumlist;
  idatum->required = YES;
  idatum->description = "map datum of input file";
  
  odatum = G_define_option();
  odatum->key = "odatum";
  odatum->type = TYPE_STRING;
  odatum->options = datumlist;
  odatum->required = NO;
  odatum->description = "map datum of output file (default: of location)";

  molod = G_define_flag();
  molod->key = 'm';
  molod->description = "Use Molodensky formula for datum shift";

  if (G_parser(argc, argv))
    exit(-1);

  if(out->answer)
      outfile = G_sites_open_new (out->answer);

  if (odatum->answer == NULL) 
    {
      if (G_database_datum_name() == NULL) 
	G_fatal_error("No map datum for database");
      outdatum = G_store(G_database_datum_name());
      if ( (od = CC_get_datum_by_name(outdatum)) < 0)
	G_fatal_error("Error output datum");
    } else {
      if ( (od = CC_get_datum_by_name(odatum->answer)) < 0)
	G_fatal_error("Error output datum");
    }
  
  if (idatum->answer == NULL) 
    {
      G_fatal_error("Error input datum");
    } else {
      if ( (id = CC_get_datum_by_name(idatum->answer)) < 0 )
	G_fatal_error("Error input datum");
    }
  
  name = in->answer;
  mapset = G_find_file("site_lists", name, "");
  if (mapset == NULL) 
    {
      sprintf(errbuf,"Couldn't find sites file [%s]", name);
      G_fatal_error(errbuf);
    } 
  
  infile = G_sites_open_old(name, mapset);
  if (infile == NULL)
    {
      sprintf (errbuf, "Can't open sites file [%s]", name);
      G_fatal_error (errbuf);
    }

  if (!proj) 
    G_fatal_error("Can't work with xy data");
  
  if (proj != PROJECTION_LL)	/* lat/lon needs no reprojection */ 
    {
      if ((proj_info_map = G_get_projinfo()) == NULL)
	G_fatal_error("Can't get projection info of map");
      if ((unit_info_map = G_get_projunits()) == NULL)
	G_fatal_error("Can't get projection units of map");
      if (pj_get_kv(&pjmap,proj_info_map,unit_info_map) < 0)
	G_fatal_error("Can't get projection key values of output map");

      /* set up projection for lat/long reprojection */
      (void) pj_zero_proj(&pjll);
      sprintf(pjll.proj, "ll");
    }
  
  if (molod->answer) {
    /* use molodensky formula */
    funcp = (int (*)(int,double,double,double,int,double*,double*,double*))CC_datum_to_datum_shift_M;
    
  } else {
    /* use blockshift formula */
    funcp = (int (*)(int, double, double, double, int, double*, double*, double*))CC_datum_to_datum_shift_CC;
  }
  
  if(outfile) { 
    Site_head sh;
    char buf[80];

    fprintf(outfile,"# %s\n", G_recreate_command());

    G_site_get_head (infile, &sh);
    G_site_put_head (outfile, &sh);
  }

  {
    int nstr, ndim, ndec;
    RASTER_MAP_TYPE rtype;
    
    rtype = -1;
    G_site_describe (infile, &ndim, &rtype, &nstr, &ndec);
    si = G_site_new_struct(rtype, ndim, nstr, ndec); 
  }
  
  /* process sites file */
  while (G__site_get (infile, si, proj) >= 0) {
    
    xcoord = si->north;
    ycoord = si->east;

    if (proj != PROJECTION_LL) {
      /* do reproject to lat/lon */
      if (pj_do_proj(&xcoord, &ycoord, &pjmap, &pjll) < 0) {
	G_fatal_error("Error in pj_do_proj\n");
      }
    }

    /* apply datum shift */
    if( (*funcp)(id, xcoord, ycoord, 0.0, od, &xcoord, &ycoord, &dummy) < 0 ) {
      G_fatal_error("Error in Datum Shift routine\n");
    }

    if (proj != PROJECTION_LL) {
      /* do project back to map projection */
      if (pj_do_proj(&xcoord, &ycoord, &pjll, &pjmap) < 0) {
	G_fatal_error("Error in pj_do_proj\n");
      }
    }
    
    si->north = xcoord;
    si->east = ycoord;

    G__site_put (outfile, si, proj);
    
  }
  
  fclose(infile);
  fclose(outfile);
  return(0);		/* OK */
}
