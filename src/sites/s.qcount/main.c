/*-
 * s.qcount - GRASS program to sample a raster file at site locations.
 * Copyright (C) 1993-1995. James Darrell McCauley.
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
 *  $Id$ 
 *
 * Modification History:
 * <03 Mar 1993> - began coding (jdm)
 * <11 Jan 1994> - announced version 0.3B on pasture.ecn.purdue.edu (jdm)
 * <14 Jan 1994> - v 0.4B, corrected some spelling errors (jdm)
 * <02 Jan 1995> - v 0.5B, clean Gmakefile, man page, added html (jdm)
 * <25 Feb 1995> - v 0.6B, cleaned 'gcc -Wall' warnings (jdm)
 * <25 Jun 1995> - v 0.7B, new site API (jdm)
 * <13 Sep 2000> - released under GPL
 *
 */

#pragma ident "s.qcount v 0.7B <25 Jun 1995>; Copyright (c) 1993-1995. James Darrell McCauley"

#include <math.h>
#include "gis.h"
#include "s_struct.h"
#include "quaddefs.h"

int main (argc, argv)
  char **argv;
  int argc;
{
  char *siteslist, errmsg[256], *mapset;
  double radius;
  double fisher, david, douglas, lloyd, lloydip, morisita;
  int i, do_indices, do_counts, nsites, nquads, verbose, *counts;

  struct Cell_head window;
  struct
  {
    struct Option *input, *n, *r;
  } parm;
  struct
  {
    struct Flag *c, *i, *q;
  } flag;
  FILE *fdsite = NULL;
  Z *z, *quads;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.n = G_define_option ();
  parm.n->key = "n";
  parm.n->type = TYPE_INTEGER;
  parm.n->required = YES;
  parm.n->description = "number of quadrats";
  parm.n->options = NULL;

  parm.r = G_define_option ();
  parm.r->key = "r";
  parm.r->type = TYPE_DOUBLE;
  parm.r->required = YES;
  parm.r->description = "quadrat radius";
  parm.r->options = NULL;

  flag.c = G_define_flag ();
  flag.c->key = 'c';
  flag.c->description = "print count data";

  flag.i = G_define_flag ();
  flag.i->key = 'i';
  flag.i->description = "suppress reporting of indices (implies -c)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  siteslist=parm.input->answer;
  
  do_counts = (flag.c->answer == (char) NULL) ? 0 : 1;
  verbose = (flag.q->answer == (char) NULL) ? 1 : 0;
  do_indices = (flag.i->answer == (char) NULL) ? 1 : 0;
  
  do_counts = (do_indices == 0) ? 1 : 0;

  sscanf(parm.n->answer,"%d",&nquads);
  sscanf(parm.r->answer,"%lf",&radius);

  G_get_window (&window);

  if ((mapset = G_find_file ("site_lists", siteslist, "")) == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", siteslist);
    G_fatal_error (errmsg);
  }
  if ((fdsite = G_fopen_sites_old (siteslist, mapset)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", siteslist);
    G_fatal_error (errmsg);
  }


  nsites = readsites (fdsite, verbose, &z, window);

  if (nsites <= 0)
    G_fatal_error ("No sites found");

  quads = find_quadrats (nquads, radius, window,verbose);

  counts = count (quads, nquads, radius, z, nsites, verbose);

  if (do_counts)
  {
    fprintf (stdout, "name|\n");
    fprintf(stdout,"desc|output of s.qcount -c sites=%s n=%d r=%g)\n",
	siteslist,nquads,radius);
    for(i=0;i<nquads;++i)
      fprintf(stderr,"%g|%g|#%d %d\n",quads[i].x,quads[i].y,i+1,counts[i]);
  }

  if (do_indices)
  {
  qindices (counts, nquads, &fisher, &david, &douglas, &lloyd, &lloydip, &morisita);

fprintf(stdout,"-----------------------------------------------------------\n");
fprintf(stdout,"Index \t\t\t\t\t Realization\n");
fprintf(stdout,"-----------------------------------------------------------\n");
fprintf(stdout,"Fisher el al (1922) Relative Variance            %g\n",fisher);
fprintf(stdout,"David & Moore (1954) Index of Cluster Size       %g\n",david);
fprintf(stdout,"Douglas (1975) Index of Cluster Frequency        %g\n",douglas);
fprintf(stdout,"Lloyd (1967) \"mean crowding\"                     %g\n",lloyd);
fprintf(stdout,"Lloyd (1967) Index of patchiness                 %g\n",lloydip);
fprintf(stdout,"Morisita's (1959) I (variability b/n patches)    %g\n",morisita);
fprintf(stdout,"-----------------------------------------------------------\n");
fprintf(stdout,"sites_list: %s n=%d (%d quadrats of radius %g)\n",siteslist,nsites,nquads,radius);
}

  exit (0);
}
