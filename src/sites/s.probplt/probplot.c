/*-s.probplot
**
** Author: James Darrell McCauley darrell@mccauley-usa.com
** 	                          http://mccauley-usa.com/
**
** This program is free software; you can redistribute it and/or
** modify it under the terms of the GNU General Public License
** as published by the Free Software Foundation; either version 2
** of the License, or (at your option) any later version.
**
** This program is distributed in the hope that it will be useful,
** but WITHOUT ANY WARRANTY; without even the implied warranty of
** MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
** GNU General Public License for more details.
**
** You should have received a copy of the GNU General Public License
** along with this program; if not, write to the Free Software
** Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
**
** $Id$
**
** Modification History:
** 0.1B <14 Oct 1994> first version
** 0.2B <17 Oct 1994> corrected small error in Gmakefile (jdm)
**      Thanks to E.A. Koster <ekoster@let.rug.nl> for noticing the problem.
** 0.3B <02 Jan 1995> added version.h, cleaned man page, added html (jdm)
** 0.4B <25 Feb 1995> cleaned 'gcc -Wall' warnings (jdm)
** <13 Sep 2000> released under GPL
**/

#pragma ident "s.probplt v 0.4B <25 Feb 1995>; Copyright (c) 1994-1995. James Darrell McCauley"

#include <string.h>
#include "raster.h"
#include <math.h>
#include <stdlib.h>
#include "gis.h"
#include "probplot.h"

int nsites;
char *plot_file, *data_file;
struct Cell_head window;

int main ( int argc, char **argv)
{
  char *mapset, *sitefile, *graphfile, errmsg[200];
  int field, all, i, log, verbose, dcmp (), pltsqq();

  extern struct Cell_head window;
  struct GModule *module;
  double factor;
  double  *z;
  FILE *fdsite;
  struct
  {
    struct Flag *all, *l, *q;
  } flag;
  struct
  {
    struct Option *input, *width, *save, *dfield;
  } parm;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "Normal probability plot of a GRASS site list.";
                  
  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "name of a sites file to be plotted";
  parm.input->gisprompt = "old,site_lists,sites,input";

  parm.width = G_define_option ();
  parm.width->key = "width";
  parm.width->type = TYPE_DOUBLE;
  parm.width->required = YES;
  parm.width->description = "width of bins";

  parm.save = G_define_option ();
  parm.save->key = "graph";
  parm.save->type = TYPE_STRING;
  parm.save->required = NO;
  parm.save->description = "basename of a graphing data/commands files";

  parm.dfield = G_define_option ();
  parm.dfield->key = "field";
  parm.dfield->type = TYPE_INTEGER;
  parm.dfield->answer = "1";
  parm.dfield->multiple = NO;
  parm.dfield->required = NO;
  parm.dfield->description = "which decimal attribute (if multiple)";

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";

  flag.l = G_define_flag ();
  flag.l->key = 'l';
  flag.l->description = "lognormal plot";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  /* need graphics. program will exit here if driver is not available */
  R_open_driver ();
  R_close_driver ();

  all = flag.all->answer;
  if (!all)
    G_get_window (&window);
  else
    G_get_default_window (&window);

  verbose = (!flag.q->answer);
  log = (!flag.l->answer);
  sitefile= parm.input->answer;
  sscanf(parm.dfield->answer,"%d", &field);
  graphfile= parm.save->answer;
  if ((i=sscanf(parm.width->answer,"%lf",&factor))!=1)
      G_fatal_error("error scanning arguments");

  mapset = G_find_file ("site_lists", sitefile, "");

  if (mapset == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", sitefile);
    G_fatal_error (errmsg);
  }

  if (field < 1)
  {
     sprintf (errmsg, "Decimal attribute field 0 doesn't exist.");
     G_fatal_error (errmsg);
  }

  fdsite = G_fopen_sites_old (parm.input->answer, mapset);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }

  /* read sites */
  if ((nsites = G_readsites (fdsite, all, verbose, field, &z))==0)
    G_fatal_error("No sites found. Check your region.");

  if (verbose)
    fprintf (stderr, "Sorting sites list ...              ");
  qsort (z, nsites, sizeof (double), dcmp);
  if (verbose)
    G_percent (1, 1, 1);

  if (verbose)
    fprintf (stderr, "Plotting ...                        ");
  pltsqq (z, factor, log, graphfile, verbose);
  if (verbose)
    G_percent (1, 1, 1);

  exit(0);
}

int dcmp (i, j)
    double *i, *j;
{
    if (*i < *j)
        return  -1;
 
    if (*i > *j)
        return 1;
 
    return 0;
}

