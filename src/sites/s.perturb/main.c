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

#pragma ident "s.perturb v 0.3B <25 Feb 1995>; Copyright (c) 1994-1995. James Darrell McCauley"

#include <stdlib.h>
#include <math.h>
#include "gis.h"
#include "perturb.h"
#include "zufall.h"

int 
main (int argc, char **argv)
{
  char isiteslist[256], ositeslist[256], errmsg[256], *mapset, dist;
  double p1, p2, numbers[1000];
  int (*rng) (); 
  int i, verbose, seed=0;
  int dims,cat,strs,dbls;
  Site *mysite;
  Site_head shead;

  FILE *fdisite, *fdosite;
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
  parm.in->description = "sites to be perturbed";
  parm.in->gisprompt = "old,site_lists,sites";

  parm.out = G_define_option ();
  parm.out->key = "output";
  parm.out->type = TYPE_STRING;
  parm.out->required = YES;
  parm.out->description = "sites list to save output";
  parm.out->gisprompt = "new,site_lists,sites";

  parm.dist = G_define_option() ;
  parm.dist->key        = "distribution";
  parm.dist->type       = TYPE_STRING;
  parm.dist->required   = YES;
  parm.dist->description= "uniform or normal";
 
  parm.pars = G_define_option() ;
  parm.pars->key        = "parameters";
  parm.pars->type       = TYPE_DOUBLE;
  parm.pars->required   = YES;
  parm.pars->multiple   = YES;
  parm.pars->description="parameter(s) of distribution selected";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  G_strcpy (isiteslist, parm.in->answer);
  G_strcpy (ositeslist, parm.out->answer);
  G_strcpy (errmsg, parm.dist->answer);
  verbose = (!flag.q->answer);
  switch(errmsg[0])
  {
    case 'u':
    case 'U':
	rng=zufall;
        dist='U';
	break;
/*-
    case 'p':
    case 'P':
        rng=fische;
	break;
*/
    case 'n':
    case 'N':
        dist='N';
	rng=normalen;
	break;
    default :
	G_fatal_error("distribution must be normal or uniform");
	break;
  }
  if (rng==zufall) /* || rng==fische) */
  {
    i=sscanf(parm.pars->answer,"%lf",&p1);
    if (i != 1 )
    {
      fprintf(stderr,"p1=%g buf=%s i=%d\n",p1, errmsg,i);
      G_fatal_error("error scanning arguments");
    }
    else if (p1 <= 0)
      G_fatal_error("maximum of uniform distribution must be >= zero");
  }
  else
  {
    if ((i=sscanf(parm.pars->answer,"%lf,%lf",&p1,&p2))!=2)
    {
      fprintf(stderr,"p1=%g buf=%s i=%d\n",p1, errmsg,i);
      G_fatal_error("error scanning arguments");
    }
    if (p2 <= 0)
      G_fatal_error("standard deviation of normal distribution must be >= zero");
  }

  if ((mapset = G_find_file ("site_lists", isiteslist, "")) == NULL)
  {
    sprintf (errmsg, "%s sites file [%s] not found",  
             G_program_name (), isiteslist);
    G_fatal_error (errmsg);
  }

  G_get_window (&window);
  if ((fdisite = G_fopen_sites_old (isiteslist, mapset)) == NULL)
  {
    sprintf (errmsg, "%s can't open sites file [%s]", 
             G_program_name (), isiteslist);
    G_fatal_error (errmsg);
  }
  if ( (fdosite = G_fopen_sites_new (ositeslist)) == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
             G_program_name (), ositeslist);
    G_fatal_error (errmsg);
  }

  if (G_site_describe (fdisite, &dims, &cat, &strs, &dbls)!=0)
    G_fatal_error("failed to guess format");
  if ((mysite=G_site_new_struct(cat,dims,strs,dbls))==NULL)
    G_fatal_error("memory error");

  G_site_get_head(fdisite, &shead);
  if (verbose)
    fprintf (stderr, "Writing output ...                  ");
  shead.name=ositeslist;
  G_site_put_head(fdosite, &shead);
  fprintf (fdosite, "#%s locations perturbed using %c(%s)\n", isiteslist,dist,
    parm.pars->answer);


  /* Generate a bunch of random numbers */
  zufalli(&seed);
  myrng(numbers,1000,rng,p1,p2);

  while (G_site_get (fdisite, mysite) == 0 )
  {
      if (i>=997)
      {
        /* Generate some more random numbers */
        myrng(numbers,1000,rng,p1,p2);
        i=0;
      }
      if (G_site_in_region(mysite, &window))
      {
        mysite->east+=numbers[i++];
        mysite->north+=numbers[i++];
        G_site_put (fdosite, mysite);
      }

    if (verbose)
      G_clicker();
  }

  if (verbose)
    G_percent (1, 1, 1);
  return (0);
}
