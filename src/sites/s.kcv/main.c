/*
 * s.kcv
 * Copyright (C) 1993-1994. James Darrell McCauley.
 *
 * Author: James Darrell McCauley darrell@mccauley-usa.com
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * $Id$
 *
 * Modification History:
 * 4.2B <27 Jan 1994>  fixed RAND_MAX for Solaris 2.3
 * <13 Sep 2000> released under GPL
 */

#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "kcv.h"
#include "version.h"

#ifndef RAND_MAX 
#define RAND_MAX (pow(2.0,31.0)-1) 
#endif
#ifdef __CYGWIN__
double drand48()
{
	return(rand()/32767.0);
}
#define srand48(sv) (srand((unsigned)(sv)))
#else
double drand48 ();
void srand48 ();
#endif
int 
main (int argc, char **argv)
{
  char siteslist[256], errmsg[256], *mapset;
  double east, north, (*rng) (), max, myrand ();
  int i, j, k, m, n, a, b, nsites, verbose, np, *p, dcmp ();
  FILE *fdsite, *fdtest, *fdtrain;
  D *d;
  Z *z;
  struct Cell_head window;
  struct GModule *module;
  struct
  {
    struct Option *output, *npartitions;
  } parm;
  struct
  {
    struct Flag *drand48, *q;
  } flag;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                "Randomly partition sites into test/train sets.";
                
  parm.npartitions = G_define_option ();
  parm.npartitions->key = "k";
  parm.npartitions->type = TYPE_INTEGER;
  parm.npartitions->required = YES;
  parm.npartitions->description = "number of partitions";
  parm.npartitions->options = "1-32767";

  parm.output = G_define_option ();
  parm.output->key = "sites";
  parm.output->type = TYPE_STRING;
  parm.output->required = YES;
  parm.output->description = "sites list to be sampled";
  parm.output->gisprompt = "old,site_lists,sites";

  flag.drand48 = G_define_flag ();
  flag.drand48->key = 'd';
  flag.drand48->description = "Use drand48()";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";


  if (G_parser (argc, argv))
    exit (1);

  G_strcpy (siteslist, parm.output->answer);
  verbose = (!flag.q->answer);
  np = atoi (parm.npartitions->answer);
  b = (flag.drand48->answer == '\0') ? 0 : 1;

  if (b)
  {
    rng = drand48;
    max = 1.0;
    srand48 ((long) getpid ());
  }
  else
  {
    rng = myrand;
    max = RAND_MAX;
    srand (getpid ());
  }

  mapset = G_find_file ("site_lists", siteslist, "");

  if (mapset == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", siteslist);
    G_fatal_error (errmsg);
  }

  G_get_window (&window);
  fdsite = G_fopen_sites_old (siteslist, mapset);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", siteslist);
    G_fatal_error (errmsg);
  }

  nsites = readsites (fdsite, verbose, &z, window);

  if (nsites < np)
    G_fatal_error ("More partitions than sites");

  G_begin_distance_calculations ();


  /*
   * make histogram of number sites in each test partition since the
   * number of sites will not always be a multiple of the number of
   * partitions. make_histo() returns the maximum bin size.
   */
  n = make_histo (&p, np, nsites);

  d = (D *) G_malloc (n * nsites * sizeof (D));
  if (d == NULL)
    G_fatal_error ("Memory allocation error");

  if (verbose)
    fprintf (stderr, "Writing files ...                   ");

  for (i = 0; i < np; ++i)	/* for each partition */
  {
    d_reset (&d, n * np);
    fdtest = opensites (siteslist, i + 1, "test");
    for (j = 0; j < p[i]; ++j)	/* create p[i] random points */
    {
      east = rng () / max * (window.west - window.east) + window.east;
      north = rng () / max * (window.north - window.south) + window.south;
      /* for each random point */
      for (m = 0, k = 0; m < nsites; ++m)
      {
	if (!z[m].partition)	/* if the site hasn't been taken out */
	{
	  /* calculate the distance to this site */
	  /* d[k].dist = G_distance (z[m].x, z[m].y, east, north); */
	  d[k].dist = hypot (z[m].x - east, z[m].y - north);
	  /* remember where we got the value from */
	  d[k].i = m;
	  k++;
	}
      }

      /* now sort these distances */
      qsort (d, k, sizeof (D), dcmp);
      z[d[0].i].partition = i + 1;
      G_put_site (fdtest, z[d[0].i].x, z[d[0].i].y, z[d[0].i].desc);
    }
    fclose (fdtest);

    fdtrain = opensites (siteslist, i + 1, "train");
    for (j = 0; j < nsites; ++j)
      if (z[j].partition != i + 1)
	G_put_site (fdtrain, z[j].x, z[j].y, z[j].desc);
    fclose (fdtrain);
    if (verbose)
      G_percent (i, np, 1);
  }

  if (verbose)
    G_percent (1, 1, 1);
  return (0);
}
