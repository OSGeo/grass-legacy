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

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "quaddefs.h"

#define LOOP_SIZE 100
 
int main (argc, argv)
  char **argv;
  int argc;
{
  char *siteslist, errmsg[256], *mapset, *ctmp;
  double radius;
  double fisher, david, douglas, lloyd, lloydip, morisita;
  int i, sites_read, do_indices, do_counts, nsites, nquads, verbose, *counts;

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
  SITE_XYZ *xyz, *quads;
  Site     *theSite;
  Site_head sHead;
  char *date_str;
  RASTER_MAP_TYPE map_type;
  int strs, dims, dbls;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "input";
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
  
  do_counts = (flag.c->answer) ? 1 : 0;
  verbose = (flag.q->answer) ? 0 : 1;
  do_indices = (flag.i->answer) ? 0 : 1;
  
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

  /* Need to know how many sites for verbose */
  if (G_site_describe(fdsite, &dims, &map_type, &strs, &dbls) != 0)
    G_fatal_error("Unable to read sites file format");
  
  if (NULL == (theSite = G_site_new_struct(map_type, dims, strs, dbls)))
    G_fatal_error("Unable to allocate memory for Site struct");
  
  i = 0;
  while (G_site_get(fdsite, theSite) == 0)
      if (G_site_in_region(theSite, &window))
        i++;
  
  /* Free unused, rewind file */
  G_site_free_struct(theSite);
  rewind(fdsite);

  /* Get the quadrats */
  quads = find_quadrats (nquads, radius, window,verbose);
 
  /* Do the G_readsites_xyz and counts in a loop of LOOP_SIZE at a time */
  if (NULL == (xyz = G_alloc_site_xyz(LOOP_SIZE)))
      G_fatal_error("%s: Out of Memory", G_program_name());
 
  counts = NULL;
  sites_read = 0;
  if (verbose)
    fprintf(stderr, "Counting sites in quadrats ...      ");
  while ((nsites = 
        G_readsites_xyz(fdsite, SITE_COL_NUL, 0, LOOP_SIZE, &window, xyz)) > 0) {
    /* Get the counts per quadrat */
    counts = count_sites (quads, nquads, counts, radius, xyz, nsites);
    if (counts == NULL)
      G_fatal_error("Problem counting sites in quadrats");
    sites_read += nsites;
    if (verbose) {
      if (sites_read < i) {
        G_percent(sites_read, i, 1);
      }
      else {
        G_percent(1, 1, 1);
      }
    }
  }
  if (nsites < 0 && nsites != EOF)
    G_fatal_error ("Error reading sites_list");
  
  G_free_site_xyz(xyz); 

  /* Site output if requested */
  if (do_counts)
  {
    if (NULL == (ctmp = (char *)G_malloc(sizeof(char) * 1)))
      G_fatal_error("Memory Exhausted");
    *ctmp = '\0';
    sHead.name = ctmp;
    if (NULL == (ctmp = (char *) G_malloc(sizeof(char) * 512)))
      G_fatal_error("Memory exhausted!");
    snprintf(ctmp, 512, "output of s.qcount -c sites=%s n=%d r=%g", 
        siteslist, nquads, radius);
    sHead.desc = ctmp;
    if (NULL == (ctmp = (char *) G_malloc(sizeof(char) * 80)))
      G_fatal_error("Memory exhausted!");
    strncpy(ctmp, "east|north|#quadrat %count", 80);
    sHead.labels = ctmp;
    if (NULL == (ctmp = (char *) G_malloc(sizeof(char) * 80)))
      G_fatal_error("Memory exhausted!");
    strncpy(ctmp, "123.456|987.654|#5 %17", 80);
    sHead.form = ctmp;
    /* date/time not being output ?? */
    date_str = G_date();
    sHead.stime = date_str;
    G_site_put_head(stdout, &sHead);
    if (NULL == (theSite = G_site_new_struct(CELL_TYPE, 2, 0, 1)))
      G_fatal_error("Unable to allocate memory for Site struct");
    for (i = 0; i < nquads; i++) {
      theSite->ccat = i + 1;
      theSite->east = quads[i].x;
      theSite->north = quads[i].y;
      theSite->dbl_att[0] = counts[i];
      G_site_put(stdout, theSite);
    }
    G_site_free_struct(theSite);
  }

  /* Indices if requested */
  if (do_indices)
  {
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
fprintf(stdout,"sites_list: %s n=%d (%d quadrats of radius %g)\n",siteslist,sites_read,nquads,radius);
}

  exit (0);
}
/* vim: softtabstop=2 shiftwidth=2 expandtab */
