/*-s.sv
** Copyright (c) 1994, 1995. James Darrell McCauley
** Author: James Darrell McCauley
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Modification History:
** 0.1B <15 Oct 1994> pieced together first version from s.semivar (jdm)
** 0.2B <24 Oct 1994> enabled reading from stdin (jdm)
** 0.3B <02 Jan 1995> cleaned Gmakefile, man page, added html (jdm)
** 0.4B <25 Feb 1995> cleaned 'gcc -Wall' warnings (jdm)
** 0.5B <13 Apr 1995> added POINTSSTYLE & LINESTYLE to sv.h (jdm)
** 0.6B <25 Jun 1995> new sites API (jdm)
**
**/

#pragma ident "s.sv v 0.6B <25 Jun 1995>; Copyright (c) 1994-1995. James Darrell McCauley"

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "raster.h"
#include "sv.h"

char *plot_file, *data_file;

struct Cell_head window;

int 
main (int argc, char **argv)
{
  char *mapset, *sitefile, *graphfile, errmsg[200], sep;
  int i, j, k, nh, nsites, once=0, tick;
  int verbose, plot, field;
  double h, htol, a, atol;
  int omnidirectional;
  double distance, direction, diffsq;
  Z *z;
  HGN *list;
  FILE *fdsite;
  struct
  {
    struct Flag *q, *p;
  } flag;
  struct
  {
    struct Option *input, *lag, *lagtol, *angle, *angtol, *save, *dfield;
  } parm;
  struct GModule *module;
  extern struct Cell_head window;
    
  G_gisinit (argv[0]);
  module = G_define_module();
  module->description =      
                      "Sample semivariogram of a GRASS sites list.";

  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = NO;
  parm.input->description = "name of a sites file";
  parm.input->gisprompt = "old,site_lists,sites,input";

  parm.lag = G_define_option ();
  parm.lag->key = "lag";
  parm.lag->type = TYPE_DOUBLE;
  parm.lag->required = YES;
  parm.lag->description = "nominal lag distance";

  parm.lagtol = G_define_option ();
  parm.lagtol->key = "lagtol";
  parm.lagtol->type = TYPE_DOUBLE;
  parm.lagtol->required = NO;
  parm.lagtol->description = "lag tolerance";

  parm.angle = G_define_option ();
  parm.angle->key = "direction";
  parm.angle->type = TYPE_DOUBLE;
  parm.angle->required = NO;
  parm.angle->description = "direction of semivariogram";

  parm.angtol = G_define_option ();
  parm.angtol->key = "angtol";
  parm.angtol->type = TYPE_DOUBLE;
  parm.angtol->required = NO;
  parm.angtol->description = "direction tolerance";

  parm.save = G_define_option ();
  parm.save->key = "graph";
  parm.save->type = TYPE_STRING;
  parm.save->required = NO;
  parm.save->description = "basename of a graphing data/commands files (implies -p)";

  parm.dfield = G_define_option ();
  parm.dfield->key = "field";
  parm.dfield->type = TYPE_INTEGER;
  parm.dfield->answer = "1";
  parm.dfield->multiple = NO;
  parm.dfield->required = NO;
  parm.dfield->description = "which decimal attribute (if multiple)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.p = G_define_flag ();
  flag.p->key = 'p';
  flag.p->description = "Plot sample semivariogram";

  if (G_parser (argc, argv))
    exit (1);
  G_sleep_on_error (0);

  /* Process arguments */
  verbose = (!flag.q->answer);
  sscanf(parm.dfield->answer,"%d", &field);

  if ((i = sscanf (parm.lag->answer, "%lf", &h)) != 1)
    G_fatal_error ("error scanning lag");
  if (parm.lagtol->answer)
  {
    if ((i = sscanf (parm.lagtol->answer, "%lf", &htol)) != 1)
      G_fatal_error ("error scanning lag tolerance");
    if (htol > 0.5*h)
      G_fatal_error ("lag tolerance must be less than half nominal lag");
  }
  else
    htol=0.5*h;

  if (parm.angle->answer) 
  {
    omnidirectional=0;
    if ((i = sscanf (parm.angle->answer, "%lf", &a)) != 1)
      G_fatal_error ("error scanning angle");
    if (parm.angtol->answer)
    {
        if ((i = sscanf (parm.angtol->answer, "%lf", &atol)) != 1)
	  G_fatal_error ("error scanning anglular tolerance");
    }
    else  
     atol=0.0;
  }
  else
    omnidirectional=1;

  plot = (flag.p->answer);
  graphfile=parm.save->answer;
  if (parm.save->answer)
    plot=1;
    
  if (field < 1)
  {
    sprintf (errmsg, "Decimal attribute field 0 doesn't exist.");
    G_fatal_error (errmsg);
  }

  /* need graphics. program will exit here if driver is not available */
  if (plot)
  { 
    R_open_driver ();
    R_close_driver ();
  }

  /* Find sites file and read it */
  if (sitefile = parm.input->answer)
  {

  mapset = G_find_file ("site_lists", sitefile, "");

  if (mapset == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", sitefile);
    G_fatal_error (errmsg);
  }

  G_get_window (&window);
  fdsite = G_fopen_sites_old (parm.input->answer, mapset);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }
  
  nsites = G_readsites (fdsite, 0, verbose, field, &z);
  }
  else
  {
    fdsite = stdin;
    nsites = G_readsites (fdsite, 0, verbose, field, &z);
  }
  if (nsites==0)
    G_fatal_error ("No sites found. Check your region.");
  else if (verbose)
    fprintf(stderr,"%d sites found\n",nsites);


  /* calculate maximum number of possible lag distances and allocate
     enough memory to hold them */
  nh = nbins (h);
  if ((list = (HGN *) G_malloc (nh * sizeof (HGN))) == NULL)
    G_fatal_error ("Ran out of memory; try reducing your region size");
  for (i = 0; i < nh; ++i)
  {
    list[i].h = (i + 1) * h;
    list[i].g = 0.0;
    list[i].n = 0;
  }

  /* the question here is the calculation of the angle. do we use
     azimuth angle? */
  if (G_begin_distance_calculations () > 1)
    G_warning ("I'm really not smart enough to deal with your projection");

  /* now for the heart of the matter */
  if (verbose)
    fprintf (stderr, "Computing sample semivariogram ...  ");
  for (i = 0, k = 0; i < nsites; ++i)
  {
    /* for (j = i + 1; j < nsites; ++j) /* 0<=angle<=180 */
    for (j = 0; j < nsites; ++j)/* 0<=angle<=360 */
    {
      if (i != j)		/* 0<=angle<=360 */
      {
	/* calculate separation distance and angle */
	distance = hypot (z[i].x - z[j].x, z[i].y - z[j].y);
	direction = 180.0 / 3.14159265359
	  * atan ((z[i].x - z[j].x) / (z[i].y - z[j].y));

	/*-
        distance = G_distance (z[i].x, z[i].y, z[j].x, z[j].y);
        direction = G_azimuth (z[i].x, z[i].y, z[j].x, z[j].y);
         */

	/* find the bin (k) that it fits in */
	if (omnidirectional || angle_ok (direction, a, atol))
	{
	  if ((k = (int) floor (distance / h) -1 ) > nh)
	    G_fatal_error ("oops, not enough bins");
	  if (distance <= (list[k].h + htol)
	      && distance >= (list[k].h - htol))
	  {
	    list[k].g += (z[i].z - z[j].z) * (z[i].z - z[j].z);
	    list[k].n++;
	  }
	  else if (distance <= (list[k + 1].h + htol)
		   && distance >= (list[k + 1].h - htol))
	  {
	    list[k+1].g += (z[i].z - z[j].z) * (z[i].z - z[j].z);
	    list[k+1].n++;
	  }
          else if (!once)
          {
            sprintf(errmsg,"Some pairs of data ignored");
            G_warning(errmsg);
            once=1;
          }
	}
      }
    }
    if (verbose)
      G_percent (i, nsites , 5);
  }
  for (k = 0; k < nh; ++k)
    if (list[k].g > 0)
      list[k].g /= 2.0 * list[k].n;
  if (verbose)
    G_percent (1, 1, 1);
  free (z);

  if (plot)
  {
    if (verbose)
      fprintf (stderr, "Plotting ...                        ");
    plot_hg_points (list,nh, verbose, graphfile);
    if (verbose)
      G_percent (1, 1, 1);
  }
  else
    for (k = 0; k < nh; ++k)
      if (list[k].g > 0)
        fprintf (stdout,"%g %g %d\n", list[k].h, list[k].g, list[k].n);

  free (list);

  exit (0);
}
