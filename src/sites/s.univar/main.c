/*-s.univar
** Copyright (c) 1993-1995. James Darrell McCauley
**
** Author: James Darrell McCauley 
**         mccauley@ecn.purdue.edu
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Modification History:
** <01 Sep 1993> added ms, mas, kur, and skw (jdm)
** <01 Sep 1993> added -g flag and change default output format (jdm)
** v4.2 <07 May 1994> fixed readsite() to do re-allocation (jdm)
** v4.3 <02 Jan 1995> clean Gmakefile, man page. added html (jdm)
** v4.4 <25 Feb 1995> clean 'gcc -Wall' warnings (jdm)
** v5.0 <25 Jun 1995> use new sites API (jdm)
**/

#pragma ident "s.univar v 5.0B <25 Jun 1995>; Copyright (c) 1993-1995. James Darrell McCauley"

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "univar.h"

struct Cell_head window;

int main ( int argc, char **argv)
{
  char *mapset, *sitefile, errmsg[200];
  int all, verbose, field, nsites, scriptstyle, zcmp ();
  Z *z;
  FILE *fdsite;
  UNIV stats;
  struct
  {
    struct Flag *all, *g, *l, *q;
  } flag;
  struct
  {
    struct Option *input, *dfield;
  } parm;
  extern struct Cell_head window;
  struct GModule *module;
  
  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "Univariate statistics for a GRASS sites list.";
                  
  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "name of a sites file to be input";
  parm.input->gisprompt = "old,site_lists,sites,input";

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

  flag.g = G_define_flag ();
  flag.g->key = 'g';
  flag.g->description = "Print the stats in shell script style";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  all = flag.all->answer;
  verbose = (!flag.q->answer);
  sitefile = parm.input->answer;
  scriptstyle = (flag.g->answer);
  sscanf(parm.dfield->answer,"%d", &field);

  mapset = G_find_file ("site_lists", sitefile, "");

  if (mapset == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", sitefile);
    G_fatal_error (errmsg);
  }

  if (!all)
    G_get_window (&window);
  else
    G_get_default_window (&window);
  fdsite = G_fopen_sites_old (sitefile, mapset);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }

  nsites = readsites (fdsite, all, verbose, field, &z);

  stats = univariate (z, nsites, verbose);

  if (scriptstyle)
  {
    fprintf (stdout, "n=%d\n", stats.n);
    fprintf (stdout, "m=%g\n", stats.m);
    fprintf (stdout, "s=%g\n", stats.s);
    fprintf (stdout, "cv=%g\n", stats.cv);
    fprintf (stdout, "skw=%g\n", stats.skw);
    fprintf (stdout, "kur=%g\n", stats.kur);
    fprintf (stdout, "ms=%g\n", stats.mse);
    fprintf (stdout, "mav=%g\n", stats.mav);
    fprintf (stdout, "min=%g\n", stats.min);
    fprintf (stdout, "q1=%g\n", stats.q1);
    fprintf (stdout, "med=%g\n", stats.med);
    fprintf (stdout, "q3=%g\n", stats.q3);
    fprintf (stdout, "max=%g\n", stats.max);
  }
  else
  {
    fprintf (stdout, "        number of points %d\n", stats.n);
    fprintf (stdout, "                    mean %g\n", stats.m);
    fprintf (stdout, "      standard deviation %g\n", stats.s);
    fprintf (stdout, "coefficient of variation %g\n", stats.cv);
    fprintf (stdout, "                skewness %g\n", stats.skw);
    fprintf (stdout, "                kurtosis %g\n", stats.kur);
    fprintf (stdout, "         mean of squares %g\n", stats.mse);
    fprintf (stdout, " mean of absolute values %g\n", stats.mav);
    fprintf (stdout, "                 minimum %g\n", stats.min);
    fprintf (stdout, "          first quartile %g\n", stats.q1);
    fprintf (stdout, "                  median %g\n", stats.med);
    fprintf (stdout, "          third quartile %g\n", stats.q3);
    fprintf (stdout, "                 maximum %g\n", stats.max);
  }

  return 0;
}

int zcmp (void *a,void *b)
{
  int result = 0;		/* integer to be returned */
  double diff;

  if ((diff = ((Z *) a)->z - ((Z *) b)->z) < 0.0)
    result = -1;
  else if (diff > 0.0)
    result = 1;
  return result;
}
