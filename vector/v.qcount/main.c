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

/* #pragma ident "s.qcount v 0.7B <25 Jun 1995>; Copyright (c) 1993-1995. James Darrell McCauley" */

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "quaddefs.h"

int main ( int argc, char **argv)
{
  char *mapset;
  double radius;
  double fisher, david, douglas, lloyd, lloydip, morisita;
  int i, nquads, verbose, *counts;

  struct Cell_head window;
  struct GModule *module;
  struct
  {
    struct Option *input, *output, *n, *r;
  } parm;
  struct
  {
    struct Flag *q;
  } flag;
  COOR *quads;

  struct Map_info Map;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description = "indices for quadrat counts of sites lists";
                  
  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "vector of points defining sample points";
  parm.input->gisprompt = "old,vector,vector";

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "output quadrant centres, number of points is written as category";
  parm.output->gisprompt = "new,vector,vector";

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

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  verbose = (flag.q->answer) ? 0 : 1;

  sscanf(parm.n->answer,"%d",&nquads);
  sscanf(parm.r->answer,"%lf",&radius);

  G_get_window (&window);

  /* Open input */
  if ((mapset = G_find_vector2 (parm.input->answer, "")) == NULL) {
    G_fatal_error ( "Could not find input map <%s>\n", parm.input->answer);
  }
  Vect_set_open_level (2);
  Vect_open_old (&Map, parm.input->answer, mapset);

  /* Get the quadrats */
  if (verbose)
      fprintf (stderr, "Finding quadrats ...                ");
  
  quads = find_quadrats (nquads, radius, window,verbose);

  if (verbose)
      fprintf (stderr, "\n");
 
  /* Get the counts per quadrat */
  if (verbose)
    fprintf(stderr, "Counting sites in quadrats ...      ");

  counts = (int *) G_malloc (nquads * (sizeof(int)));
  count_sites (quads, nquads, counts, radius, &Map);

  if (verbose)
      fprintf (stderr, "\n");

  Vect_close ( &Map );

  /* output if requested */
  if ( parm.output->answer )
  {
    struct Map_info Out;
    struct line_pnts *Points;
    struct line_cats *Cats;

    Points = Vect_new_line_struct ();
    Cats = Vect_new_cats_struct ();

    Vect_open_new (&Out, parm.output->answer, 0);
    Vect_hist_command ( &Out );

    for (i = 0; i < nquads; i++) {
      Vect_reset_line ( Points );
      Vect_reset_cats ( Cats );
      
      Vect_append_point ( Points, quads[i].x, quads[i].y, 0.0 );
      Vect_cat_set ( Cats, 1, counts[i] ); 

      Vect_write_line ( &Out, GV_POINT, Points, Cats );
    }

    Vect_build ( &Out, stderr );
    Vect_close ( &Out );
    
  }

  /* Indices if requested */
  qindices (counts, nquads, &fisher, &david, &douglas, &lloyd, &lloydip, &morisita);

  fprintf(stdout,"-----------------------------------------------------------\n");
  fprintf(stdout,"Index                                           Realization\n");
  fprintf(stdout,"-----------------------------------------------------------\n");
  fprintf(stdout,"Fisher el al (1922) Relative Variance            %g\n",fisher);
  fprintf(stdout,"David & Moore (1954) Index of Cluster Size       %g\n",david);
  fprintf(stdout,"Douglas (1975) Index of Cluster Frequency        %g\n",douglas);
  fprintf(stdout,"Lloyd (1967) \"mean crowding\"                     %g\n",lloyd);
  fprintf(stdout,"Lloyd (1967) Index of patchiness                 %g\n",lloydip);
  fprintf(stdout,"Morisita's (1959) I (variability b/n patches)    %g\n",morisita);
  fprintf(stdout,"-----------------------------------------------------------\n");

  exit (0);
}
