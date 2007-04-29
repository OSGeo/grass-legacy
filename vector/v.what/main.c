/*
****************************************************************************
*
* MODULE:       v.what  - for GRASS 6.1 - 
*
* AUTHOR(S):    Trevor Wiens - derived from d.what.vect - 15 Jan 2006
*
* PURPOSE:     To select and report attribute information for objects at a
*               user specified location. This replaces d.what.vect by removing
*               the interactive component to enable its use with the new 
*               gis.m and future GUI.
*
* COPYRIGHT:    (C) 2006 by the GRASS Development Team
*
*              This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#define MAIN
#include <stdlib.h>
#include <string.h>
#include <grass/glocale.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/raster.h>
#include <grass/display.h>
#include "what.h"
#include <grass/dbmi.h>
#include <grass/glocale.h>
#include <grass/config.h>

/* Vector map grabbing taken from d.zoom */

int
main(int argc, char **argv)
{
  struct Flag *printattributes, *topo_flag, *stdin_flag;
  struct Option *opt1, *opt4, *maxdistance;
  struct Cell_head window;
  struct GModule *module;
  char *mapset;
  char *str;
  char buf[2000];
  int i, j, level, width = 0, mwidth = 0, ret;
  double xval, yval, xres, yres, maxd, x;
  double EW_DIST1, EW_DIST2, NS_DIST1, NS_DIST2;
  char nsres[30], ewres[30];
  char ch;
  
    /* Initialize the GIS calls */
    G_gisinit (argv[0]) ;

    /* Conditionalize R_open_driver() so "help" works, open quiet as well */
    R__open_quiet();
    if (R_open_driver() == 0)
    {
        if (D_get_dig_list (&vect, &nvects) < 0)
            vect = NULL;
        else
        {
            vect = (char **)G_realloc(vect, (nvects+1)*sizeof(char *));
            vect[nvects] = NULL;
        }

        R_close_driver();
   }

    module = G_define_module();
    module->keywords = _("vector");
    module->description = 
	_("Queries a vector map layer at given locations");

    opt1 = G_define_standard_option(G_OPT_V_MAP);
    opt1->multiple   = YES;
    if (vect)
          opt1->answers = vect;

    opt4 = G_define_option() ;
    opt4->key        = "east_north";
    opt4->type       = TYPE_DOUBLE;
    opt4->key_desc   = "east,north";
    opt4->required   = NO;
    opt4->multiple   = YES;
    opt4->description= _("Coordinates for query");

    maxdistance = G_define_option();
    maxdistance->type = TYPE_DOUBLE;
    maxdistance->key = "distance";
    maxdistance->answer = "0";
    maxdistance->multiple = NO;
    maxdistance->description = _("Query threshold distance");

    topo_flag = G_define_flag();
    topo_flag->key = 'd';
    topo_flag->description = _("Print topological information (debugging)");

    printattributes = G_define_flag();
    printattributes->key = 'a';
    printattributes->description = _("Print attribute information");
  
    stdin_flag = G_define_flag();
    stdin_flag->key = 's';
    stdin_flag->description = _("Read coordinates from standard input");

    if(!vect)
        opt1->required = YES;

    if((argc > 1 || !vect) && G_parser(argc,argv))
        exit(EXIT_FAILURE);

    if (opt1->answers && opt1->answers[0])
        vect = opt1->answers;

   maxd = atof(maxdistance->answer);

/*  
*  fprintf(stdout, maxdistance->answer);
*  fprintf(stdout, "Maxd is %f", maxd);
*  fprintf(stdout, xcoord->answer);
*  fprintf(stdout, "xval is %f", xval);
*  fprintf(stdout, ycoord->answer);
*  fprintf(stdout, "yval is %f", yval);
*/
  
    if (maxd == 0.0) 
    {
        G_get_window (&window);
        x = window.proj;
        G_format_resolution  (window.ew_res,  ewres,  x);
        G_format_resolution  (window.ns_res,  nsres,  x);
        EW_DIST1 = G_distance(window.east, window.north, window.west, window.north);
        /* EW Dist at South Edge */
        EW_DIST2 = G_distance(window.east, window.south, window.west, window.south);
        /* NS Dist at East edge */
        NS_DIST1 = G_distance(window.east, window.north, window.east, window.south);
        /* NS Dist at West edge */
        NS_DIST2 = G_distance(window.west, window.north, window.west, window.south);
        xres = ((EW_DIST1 + EW_DIST2) / 2) / window.cols;
        yres = ((NS_DIST1 + NS_DIST2) / 2) / window.rows;
        if (xres > yres) maxd = xres; else maxd = yres;
    }


  /* Look at maps given on command line */

    if(vect)
    {
        for(i=0; vect[i]; i++);
        nvects = i;

        Map = (struct Map_info *) G_malloc(nvects * sizeof(struct Map_info));
    
        width = mwidth = 0;
        for(i=0; i<nvects; i++)
        {
            str = strchr(vect[i], '@');
            if (str) j = str - vect[i];
            else j = strlen(vect[i]);
            if (j > width) width = j;
    
            mapset = G_find_vector2(vect[i],"");
            j = strlen(mapset);
            if (j > mwidth)
                mwidth = j;
    
            level = Vect_open_old (&Map[i], vect[i], mapset);
            if (level < 0) 
                G_fatal_error ( _("Vector file [%s] not available"), vect[i]);
    
            if (level < 2) 
                G_fatal_error ( _("%s: You must build topology on vector map"), vect[i]);

            /* G_message ("Building spatial index ..."); */
            Vect_build_spatial_index ( &Map[i], NULL );
       }
    }

  if( stdin_flag->answer ) {
      setvbuf(stdin,  NULL, _IOLBF, 0);
      setvbuf(stdout, NULL, _IOLBF, 0);
      while ( fgets (buf, sizeof(buf), stdin) != NULL ) { 
	  ret=sscanf( buf, "%lf%c%lf", &xval, &ch, &yval);
	  if(ret == 3 && (ch == ',' || ch == ' ' || ch == '\t') ) {
		what(xval, yval, maxd, width, mwidth, topo_flag->answer,printattributes->answer);
	  } else {
	        G_warning ( _("Unknown input format, skipping: %s"), buf);
		continue;
	  }
      };

  } else {
	/* take coords from the command line */
      if( !opt4->answer)
	G_fatal_error(_("No input coordinates provided"));

      for(i=0; opt4->answers[i] != NULL; i+=2) {
	xval = atof(opt4->answers[i]);
	yval = atof(opt4->answers[i+1]);
	what(xval, yval, maxd, width, mwidth, topo_flag->answer,printattributes->answer); 
      }
  }

  for(i=0; i<nvects; i++)
      Vect_close (&Map[i]);

  R_pad_freelist(vect, nvects);

  /* fprintf(stderr, _("\n")); */
  exit(EXIT_SUCCESS);
}



