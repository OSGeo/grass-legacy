/*-
 * s.windavg - GRASS program to window averages of sites
 * Copyright (C) 1994. James Darrell McCauley.
 *
 * Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
 *         USDA Fellow
 *         Department of Agricultural Engineering
 *         Purdue University
 *         West Lafayette, Indiana 47907-1146 USA
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for non-commercial purposes is hereby granted. This 
 * software is provided "as is" without express or implied warranty.
 *
 * JAMES DARRELL MCCAULEY (JDM) MAKES NO EXPRESS OR IMPLIED WARRANTIES
 * (INCLUDING BY WAY OF EXAMPLE, MERCHANTABILITY) WITH RESPECT TO ANY
 * ITEM, AND SHALL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL
 * OR CONSEQUENTAL DAMAGES ARISING OUT OF THE POSSESSION OR USE OF
 * ANY SUCH ITEM. LICENSEE AND/OR USER AGREES TO INDEMNIFY AND HOLD
 * JDM HARMLESS FROM ANY CLAIMS ARISING OUT OF THE USE OR POSSESSION 
 * OF SUCH ITEMS.
 *
 * Modification History:
 * <08 Jan 1994> - began coding (jdm)
 * <06 Jan 1994> - announced version 0.1B on pasture.ecn.purdue.edu (jdm)
 *
 */

#include <math.h>
#include "gis.h"
#include "s_struct.h"

int main (argc, argv)
  char **argv;
  int argc;
{
  char *isiteslist, *ositeslist, errmsg[256], *mapset;
  double avg,east,north,N,S,E,W;
  int i, j, k, n, nsites, verbose, sitedesc, suppress_zeros,gnuplot;
  struct Cell_head window;
  struct Categories cats;
  struct
  {
    struct Option *input, *output;
  } parm;
  struct
  {
    struct Flag *z, *l, *p, *q;
  } flag;
  FILE *fdisite = NULL, *fdosite = NULL;
  Z *z;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "sites list to store means";
  parm.output->gisprompt = "new,site_lists,sites";

  flag.l = G_define_flag ();
  flag.l->key = 'l';
  flag.l->description = "Use site description (if format is E|N|#n desc)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.z = G_define_flag ();
  flag.z->key = 'z';
  flag.z->description = "Suppress windows with zero averages";

  flag.p = G_define_flag ();
  flag.p->key = 'p';
  flag.p->description = "Output in g.gnuplot format (overrides -z and writes to stdout)";

  if (G_parser (argc, argv))
    exit (1);

  isiteslist=parm.input->answer;
  ositeslist=parm.output->answer;
  
  sitedesc = (flag.l->answer == NULL) ? 0 : 1;
  suppress_zeros = (flag.z->answer == NULL) ? 0 : 1;
  gnuplot = (flag.p->answer == NULL) ? 0 : 1;
  verbose = (flag.q->answer == NULL) ? 1 : 0;
  if (gnuplot) suppress_zeros=0;

  G_get_window (&window);

  if ((mapset = G_find_file ("site_lists", isiteslist, "")) == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", isiteslist);
    G_fatal_error (errmsg);
  }
  if ((fdisite = G_fopen_sites_old (isiteslist, mapset)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", isiteslist);
    G_fatal_error (errmsg);
  }

  if (ositeslist == NULL || gnuplot)
    fdosite = stdout;
  else
  {
    if ((fdosite = G_fopen_sites_new (ositeslist)) == NULL)
    {
      sprintf (errmsg, "can't create sites file [%s]\nUsing standard output",
	       ositeslist);
      G_warning (errmsg);
      fdosite = stdout;
    }
    else
    {
      fprintf (fdosite, "name|%s\n", ositeslist);
      fprintf (fdosite, "desc|moving average of %s\n", isiteslist);
    }
  }

  nsites = readsites (fdisite, verbose, &z, window);

  if (nsites <= 0)
    G_fatal_error ("No sites found");

  for (i = 0; i < nsites; ++i)	/* for each partition */
  {
    if (sitedesc)
        sscanf (z[i].desc, "#%*d %lf", &z[i].z);
    else
        sscanf (z[i].desc, "%lf", &z[i].z);
  }

  if (verbose)
    fprintf (stderr, "Averaging ...                       ");
 
  for(i=0;i<window.cols;++i)
  {
    W=G_col_to_easting((double)i, window);
    east=G_col_to_easting((double)i+0.5, window);
    E=G_col_to_easting((double)i+1.0, window);
    for(j=0;j<window.rows;++j)
    {
      S=G_row_to_northing((double)j+1.0, window);
      north=G_row_to_northing((double)j+0.5, window);
      N=G_row_to_northing((double)j, window);
      avg=0.0;
      n=0;
      for (k = 0; k < nsites; ++k)
      {
        if (z[k].x <= E && z[k].x >= W && z[k].y <= N && z[k].y >= S)
        {
          n++;
          avg+=z[k].z;
        }
      }
      if (n>0)
        avg/=n;
      else
        avg=0.0;

      if (gnuplot)
        printf("%g %g %g\n",east,north,avg);
      else if (n>0)
      {
        sprintf (errmsg, "%g", avg);
        G_put_site (fdosite, east, north, errmsg);
      }
      else if (!suppress_zeros)
      {
        sprintf (errmsg, "%g", 0.0);
        G_put_site (fdosite, east, north, errmsg);
      }
    }
    if (verbose)
      G_percent (i, window.cols, 1);
    if (gnuplot)
      printf("\n");
  }
  fclose (fdosite);

  if (verbose)
    G_percent (1, 1, 1);
  exit (0);
}
