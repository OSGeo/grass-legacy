/*
 * s.rand
 * Copyright (C) 1993-1995. James Darrell McCauley.
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
 *
 * $Id$
 *
 * s.rand v 0.5B <25 Jun 1995> Copyright (c) 1993-1995. James Darrell McCauley
 * <?? ??? 1993> - began coding and released test version (jdm)
 * <10 Jan 1994> - changed RAND_MAX for rand(), since it is different for
 *                 SunOS 4.1.x and Solaris 2.3. stdlib.h in the latter defines
 *                 RAND_MAX, but it doesn't in the former. v0.2B (jdm)
 * <02 Jan 1995> - clean Gmakefile, man page. added html v0.3B (jdm)
 * <25 Feb 1995> - cleaned 'gcc -Wall' warnings (jdm)
 * <25 Jun 1995> - new site API (jdm)
 */

#include "gis.h"
#include <stdlib.h>
#include <math.h>
#include <sys/types.h>
#include <unistd.h>
#include <string.h>

#ifndef RAND_MAX 
#define RAND_MAX (pow(2.0,31.0)-1) 
#endif
double myrand(void);

int main (int argc, char *argv[])
{
  char *output, errmsg[256];
  double (*rng) (), max;
  int i, c, n, b;
  Site *s;
  FILE *fd;
  struct Cell_head window;
  struct
  {
    struct Option *output, *nsites;
  } parm;
  struct
  {
    struct Flag *rand, *drand48;
  } flag;

  G_gisinit (argv[0]);

  parm.nsites = G_define_option ();
  parm.nsites->key = "n";
  parm.nsites->type = TYPE_INTEGER;
  parm.nsites->required = YES;
  parm.nsites->description = "number of sites to be created";
  parm.nsites->options = "1-32767";

  parm.output = G_define_option ();
  parm.output->key = "sites";
  parm.output->type = TYPE_STRING;
  parm.output->required = YES;
  parm.output->description = "sites file to be created";
  parm.output->gisprompt = "any,site_lists,sites";

  flag.drand48 = G_define_flag ();
  flag.drand48->key = 'd';
  flag.drand48->description = "Use drand48() [default=rand()]";

  if (G_parser (argc, argv))
    exit (1);

  output = parm.output->answer;
  n = atoi(parm.nsites->answer);
  b = (flag.drand48->answer == '\0') ? 0 : 1;

  s = G_site_new_struct (c, 2, 0, 1);

  if (strcmp(output,"stdout")==0 || strcmp(output,"-")==0)
    fd=stdout;
  else
    fd = G_fopen_sites_new (output);
  if (fd == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
	     G_program_name (), output);
    G_fatal_error(errmsg);
  }
  else if (n <= 0)
  {
    sprintf (errmsg, "%s given an illegal number of sites [%d]",
	     G_program_name (), n);
    G_fatal_error(errmsg);
  }

  if (b)
  {
    rng=drand48;
    max=1.0;
    srand48 ((long)getpid ()); 
  }
  else  /* default is rand() */
  {
    rng=myrand;
    max=RAND_MAX;
    srand (getpid ()); 
  }

  G_get_window (&window);
  for(i=0; i<n; ++i)
  {
    s->east=rng()/max*(window.west-window.east)+window.east;
    s->north=rng()/max*(window.north-window.south)+window.south;
    s->ccat=(CELL)i+1;
    s->dbl_att[0]=i+1;
    G_site_put (fd, s);
  }
  fclose (fd);
  return (0);
}

double myrand()
{
  return (double) rand();
}
