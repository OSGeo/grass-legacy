/*-
 * s.perturb
 * Copyright (C) 1994. James Darrell McCauley.
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
 * Modification History:
 * 0.1B <18 Feb 1994>  first version (jdm)
 * 0.2B <02 Jan 1995>  clean man page, added html page (jdm)
 * 0.3B <25 Feb 1995>  cleaned up 'gcc -Wall' warnings (jdm)
 * <13 Sept 2000>      released under GPL
 */

/* #pragma ident "s.perturb v 0.3B <25 Feb 1995>; Copyright (c) 1994-1995. James Darrell McCauley" */

#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "perturb.h"
#include "zufall.h"

int 
main (int argc, char **argv)
{
  char *mapset;
  double p1, p2, numbers[1000];
  int (*rng) (); 
  int i, verbose, seed=0;
  int line, nlines, ttype, n, ret;
  struct field_info *Fi, *Fin;

  struct Map_info In, Out;

  struct line_pnts *Points;
  struct line_cats *Cats;

  struct Cell_head window;
  struct GModule *module;
  struct
  {
    struct Option *in, *out, *dist, *pars;
  } parm;
  struct
  {
    struct Flag *q;
  } flag;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "Random location perturbations of GRASS sites.";
                  
  parm.in = G_define_option ();
  parm.in->key = "input";
  parm.in->type = TYPE_STRING;
  parm.in->required = YES;
  parm.in->description = "vector points to be perturbed";
  parm.in->gisprompt = "old,vector,vector";

  parm.out = G_define_option ();
  parm.out->key = "output";
  parm.out->type = TYPE_STRING;
  parm.out->required = YES;
  parm.out->description = "vector to save output";
  parm.out->gisprompt = "new,vector,vector";

  parm.dist = G_define_option() ;
  parm.dist->key        = "distribution";
  parm.dist->type       = TYPE_STRING;
  parm.dist->required   = NO;
  parm.dist->options    = "uniform,normal";
  parm.dist->answer     = "uniform";
  parm.dist->description= "Distribution of perturbation";
 
  parm.pars = G_define_option() ;
  parm.pars->key        = "parameters";
  parm.pars->type       = TYPE_DOUBLE;
  parm.pars->required   = YES;
  parm.pars->multiple   = YES;
  parm.pars->description= "Parameter(s) of distribution. If the distribution "
                          "is uniform, only one parameter, the maximum, is needed. "
                          "For a normal distribution, two parameters, the mean and "
                          "standard deviation, are required.";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  verbose = (!flag.q->answer);

  switch(parm.dist->answer[0])
  {
    case 'u':
	rng=zufall;
	break;
    case 'n':
	rng=normalen;
	break;
  }
  if (rng==zufall) 
  {
    i=sscanf(parm.pars->answer,"%lf",&p1);
    if (i != 1 )
    {
      G_fatal_error("error scanning arguments");
    }
    else if (p1 <= 0)
      G_fatal_error("maximum of uniform distribution must be >= zero");
  }
  else
  {
    if ((i=sscanf(parm.pars->answer,"%lf,%lf",&p1,&p2))!=2)
    {
      G_fatal_error("error scanning arguments");
    }
    if (p2 <= 0)
      G_fatal_error("standard deviation of normal distribution must be >= zero");
  }

  G_get_window (&window);

  /* Open input */
  if ((mapset = G_find_vector2 (parm.in->answer, "")) == NULL) {
    G_fatal_error ( "Could not find input map <%s>\n", parm.in->answer);
  }
  Vect_set_open_level (2);
  Vect_open_old (&In, parm.in->answer, mapset);

  /* Open output */
  Vect_open_new (&Out, parm.out->answer, 0);

  Vect_hist_copy (&In, &Out);
  Vect_hist_command ( &Out );

  /* Generate a bunch of random numbers */
  zufalli(&seed);
  myrng(numbers,1000,rng,p1,p2);

  Points = Vect_new_line_struct ();
  Cats = Vect_new_cats_struct ();

  nlines = Vect_get_num_lines ( &In );

  i = 0;
  for( line = 1; line <= nlines ; line++) {
      int type;
      
      type = Vect_read_line ( &In, Points, Cats, line );
      
      if ( type & GV_POINT ) {
	  if (i>=997)
	  {
	    /* Generate some more random numbers */
	    myrng(numbers,1000,rng,p1,p2);
	    i=0;
	  }

	  Points->x[0] += numbers[i++];
	  Points->y[0] += numbers[i++];
      }
	  
      Vect_write_line ( &Out, type, Points, Cats );
  }

  /* Copy tables */
  n = Vect_get_num_dblinks ( &In );
  ttype = GV_1TABLE;
  if ( n > 1 ) ttype = GV_MTABLE;

  for ( i = 0; i < n; i++ ) {
    Fi = Vect_get_dblink ( &In, i );
    if ( Fi == NULL ) {
	G_fatal_error ( "Cannot get db link info" );
    }
    Fin = Vect_default_field_info ( &Out, Fi->number, Fi->name, ttype );
    Vect_map_add_dblink ( &Out, Fi->number, Fi->name, Fin->table, Fi->key, Fin->database, Fin->driver);
    
    ret = db_copy_table ( Fi->driver, Fi->database, Fi->table, 
		Fin->driver, Vect_subst_var(Fin->database,&Out), Fin->table );
    if ( ret == DB_FAILED ) {
	G_warning ( "Cannot copy table" );
    }
  }
  
  Vect_close ( &In );

  Vect_build ( &Out, stderr );
  Vect_close ( &Out );

  return (0);
}

