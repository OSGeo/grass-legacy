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
 * <30 Oct 2000> - Updated for GRASS 5.0 (Eric G. Miller)
 */

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include "gis.h"

int main (argc, argv)
  char **argv;
  int argc;
{
  char *isiteslist, *ositeslist, errmsg[256], *mapset;
  double avg,N,S,E,W;
  int i, j, k, n, verbose, suppress_zeros,gnuplot;
  struct Cell_head window;
  struct Categories cats;
  struct
  {
    struct Option *input, *output, *attr, *index;
  } parm;
  struct
  {
    struct Flag *z, *p, *q;
  } flag;
  FILE *fdisite = NULL, *fdosite = NULL;
  SITE_XYZ xyz;
  int ftype, findex;
  Site *theSite = G_site_new_struct(CELL_TYPE, 2, 0, 1);

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.attr = G_define_option();
  parm.attr->key = "attr";
  parm.attr->type = TYPE_STRING;
  parm.attr->required = YES;
  parm.attr->description = "Attribute to average values over";
  parm.attr->options = "dim,cat,decimal,string";
  parm.attr->answer = "cat";
  
  parm.index = G_define_option();
  parm.index->key = "index";
  parm.index->type = TYPE_INTEGER;
  parm.index->required = NO;
  parm.index->description = "Index of attribute type (ignored for cat)";
  parm.index->answer = "1";
  
  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "sites list to store means";
  parm.output->gisprompt = "new,site_lists,sites";

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
  findex = atoi(parm.index->answer);
  if (strncmp(parm.attr->answer,"cat",3) == 0)
  {
    ftype = SITE_COL_NUL;
  }
  else if (strncmp(parm.attr->answer,"dim",3) == 0)
  {
    ftype = SITE_COL_DIM;
    if (findex < 3)
      G_fatal_error("%s: Index for dimension must be greater than 2", 
          G_program_name());
    findex -= 2;
  }
  else if (strncmp(parm.attr->answer,"decimal",7) == 0)
  {
    ftype = SITE_COL_DBL;
    if (findex < 1)
      G_fatal_error("%s: Index for decimal attribute must be greater than 0",
          G_program_name());
  }
  else if (strncmp(parm.attr->answer,"string",6) == 0)
  {
    ftype = SITE_COL_STR;
    if (findex < 1)
      G_fatal_error("%s: Index for string attribute must be greater than 0",
          G_program_name());
  }
  else /* G_parser() shouldn't allow this ... */
  {
    G_fatal_error("%s: Unknown attribute type %s", G_program_name(),
        parm.attr->answer);
  } 
  
  suppress_zeros = (flag.z->answer) ? 1 : 0;
  gnuplot = (flag.p->answer) ? 1 : 0;
  verbose = (flag.q->answer) ? 0 : 1;
  if (gnuplot) suppress_zeros=0;

  G_get_window (&window);

  if ((mapset = G_find_file ("site_lists", isiteslist, "")) == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", isiteslist);
    G_fatal_error (errmsg);
  }
  if ((fdisite = G_sites_open_old (isiteslist, mapset)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", isiteslist);
    G_fatal_error (errmsg);
  }

  if (ositeslist == NULL || gnuplot)
    fdosite = stdout;
  else
  {
    if (G_find_file("site_lists", ositeslist, G_mapset()) != NULL)
      G_fatal_error("%s: Output site_lists %s exists",
          G_program_name(), ositeslist);
    if ((fdosite = G_sites_open_new (ositeslist)) == NULL)
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

  if (verbose)
    fprintf (stderr, "Averaging ...                       ");
 
  k = 1;
  for(i=0 ;i<window.cols;++i)
  {
    W=G_col_to_easting((double)i, &window);
    theSite->east=G_col_to_easting((double)i+0.5, &window);
    E=G_col_to_easting((double)i+1.0, &window);
    for(j=0;j<window.rows;++j)
    {
      S=G_row_to_northing((double)j+1.0, &window);
      theSite->north=G_row_to_northing((double)j+0.5, &window);
      N=G_row_to_northing((double)j, &window);
      avg=0.0;
      n=0;
      rewind(fdisite); /* Make sure were at the beginning */
      while (G_readsites_xyz(fdisite, ftype, findex, 1, &window, &xyz) == 1)
      {
        if (xyz.x <= E && xyz.x >= W && xyz.y <= N && xyz.y >= S)
        {
          n++;
          if (ftype == SITE_COL_NUL) /* Use cat */
            switch (xyz.cattype) {
              case CELL_TYPE:
                avg+=(double)xyz.cat.c; break;
              case FCELL_TYPE:
                avg+=(double)xyz.cat.f; break;
              case DCELL_TYPE:
                avg+=xyz.cat.d; break;
              default: /* Programmer Error ?? */
                G_fatal_error("%s: No cat values exist in sites_list",
                    G_program_name());
            }
          else
            avg+=xyz.z;
        }
      }
      if (n>0)
        avg/=n;
      else
        avg=0.0;

      theSite->ccat = k++;
      theSite->dbl_att[0] = avg;
      if (gnuplot)
        printf("%g %g %g\n",theSite->east,theSite->north,avg);
      else if (n>0)
      {
        G_site_put (fdosite, theSite);
      }
      else if (!suppress_zeros)
      {
        G_site_put (fdosite, theSite);
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

/* vim: set softtabstop=2 shiftwidth=2 expandtab: */
