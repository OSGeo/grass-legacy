/*-s.medpolish
**
** Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
**         USDA Fellow
**         Department of Agricultural Engineering
**         Purdue University
**         West Lafayette, Indiana 47907-1146 USA
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Modification History:
** 21 May 92 - began coding <mccauley@ecn.purdue.edu>
**
**/

#define MAIN
#define		DIG_DIR		"dig"
#define		PLUS_DIR	"dig_plus"
#define		ATT_DIR		"dig_att"
#define		DIG_CAT_DIR	"dig_cats"
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "polish.h"
#include "gis.h"
#include "Vect.h"
#include "dig_structs.h"

struct Cell_head window;
float *datax;
float *datay;
int *cats;
char **desc;

main (argc, argv)
  char **argv;
  int argc;
{
  int readsites ();
  int c, n = 0, sid, vid, plen, ntris, tid, all, points, delaunay, label;
  int verbose, doreport;
  char *mapset, *sitefile, ebuf[128], nbuf[128], errmsg[200];
  static char dig_file[128];
  static char att_file[128];
  static char cat_file[128];
  double atof ();
  FILE *fdsite, *fdcats, *fp_plus, *rep;
  struct Map_info Mapi, Mapo;
  struct line_pnts *Points;
  struct
  {
    struct Flag *all, *q, *points, *effects;
  } flag;
  struct
  {
    struct Option *site, *vect, *output, *thresh, *report;
  } parm;
  extern struct Cell_head window;

  G_gisinit (argv[0]);

  Points = Vect_new_line_struct ();

  parm.site = G_define_option ();
  parm.site->key = "sites";
  parm.site->type = TYPE_STRING;
  parm.site->required = YES;
  parm.site->description = "sites file for polishing";
  parm.site->gisprompt = "old,site_lists,sites";

  parm.vect = G_define_option ();
  parm.vect->key = "vect";
  parm.vect->type = TYPE_STRING;
  parm.vect->required = YES;
  parm.vect->description = "vector file that defines grid";
  parm.vect->gisprompt = "old,dig,vector";

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = YES;
  parm.output->description = "file for residuals";
  parm.output->gisprompt = "new,dig,vector";

  parm.report = G_define_option ();
  parm.report->key = "report";
  parm.report->type = TYPE_STRING;
  parm.report->required = NO;
  parm.report->description = "ascii file for report";

  parm.thresh = G_define_option ();
  parm.thresh->key = "thresh";
  parm.thresh->type = TYPE_DOUBLE;
  parm.thresh->required = NO;
  parm.thresh->description = "threshold index";
  parm.thresh->options = "0-10000"; 
  parm.thresh->answer = "1";

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet; No chatter";
  /* flag.q->answer = 1; */

  flag.points = G_define_flag ();
  flag.points->key = 's';
  flag.points->description = "Make output a sites file (default=vect)";

  flag.effects = G_define_flag ();
  flag.effects->key = 'e';
  flag.effects->description = "Store row, column, and all effects";
  /* flag.effect->answer = 1; */


  if (G_parser (argc, argv))
    exit (1);
  all = flag.all->answer;
  points = (!flag.points->answer);
  verbose=1;
  if(flag.q->answer) verbose=0;

  if (parm.report->answer != NULL)
  {
    doreport=1;
    if (!strcmp(parm.report->answer, "stdout"))
      rep=stdout;
    else
    {
      if( (rep=fopen(parm.report->answer,"w")) == NULL)
      {
	sprintf(errmsg, "couldn't open %s, using stdout", parm.report->answer);
	G_warning(errmsg);
	rep=stdout;
      }
    }
  }
  else
    doreport=0;
  mapset = G_store (G_mapset ());

  /* open all input files... sites first, then vector grid */
  sitefile = parm.site->answer;
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

  /* get fullpath to vector file names */
  G__file_name (att_file, ATT_DIR, parm.vect->answer, mapset);
  G__file_name (cat_file, DIG_CAT_DIR, parm.vect->answer, mapset);
  Mapi.att_file = att_file;

  /* start opening vector files */
  mapset = G_find_file (DIG_DIR, parm.vect->answer, "");
  if (0 > Vect_open_old (&Mapi, parm.vect->answer, mapset))
  {
    sprintf (errmsg, "Not able to open old vector file [%s]\n",
	     parm.vect->answer);
    G_fatal_error (errmsg);
  }

/*-
  if ((Mapi.att_fp = fopen (Map.att_file, "r")) == NULL)
  {
    sprintf (errmsg, "Can't open attribute file [%s]\n", att_file);
    G_fatal_error (errmsg);
  }
*/

  /* Input files opened. Now try to open output file (either vector or site) */
  if (points)
  {
    /* check for existence of support directories  */
    G__make_mapset_element (ATT_DIR);
    G__make_mapset_element (DIG_CAT_DIR);

    /* get fullpath to file names */
    G__file_name (att_file, ATT_DIR, parm.output->answer, mapset);
    G__file_name (cat_file, DIG_CAT_DIR, parm.output->answer, mapset);
    Mapo.att_file = att_file;

    /* start opening files */
    if (0 > Vect_open_new (&Mapo, parm.output->answer))
    {
      sprintf (errmsg, "Not able to open vector file <%s>\n",
	       parm.output->answer);
      G_fatal_error (errmsg);
    }
    if ((Mapo.att_fp = fopen (Mapo.att_file, "w+")) == NULL)
    {
      sprintf (errmsg, "Can't create attribute file [%s]\n", att_file);
      G_fatal_error (errmsg);
    }
    if ((fdcats = fopen (cat_file, "w+")) == NULL)
    {
      sprintf (errmsg, "Can't create category file [%s]\n", att_file);
      G_fatal_error (errmsg);
    }
  }
  else
  {
    /* open site file */
    G_fatal_error ("[-s] not implemented yet");
  }

  /*
   * all I/O files opened. now verify that parm.vect->answer is a grid
   * (created by v.mkgrid, possibly rotated)
   */

  /* read sites into global variables and close file */

  n = readsites (fdsite, all, verbose);

  dig_load_plus (&Mapi, fp_plus, 0);

  /* don't know what this does:  clean_atts_file (Map.att_file); */

  /*
   * now that we have all of that nasty initialization of streams out of the
   * way, we pass control over to handle()
   */

  n=handle (&Mapi,n,verbose,atof(parm.thresh->answer), flag.effects->answer,
	    doreport, rep);

  init_header (Mapo.dig_fp, &window, &Mapo.head);

  write_points (&Mapo, n, verbose);

  write_cats_att (Mapo.att_fp, fdcats, n, verbose);

  /* close the binary vector file */
  Vect_close (&Mapo);
  Vect_close (&Mapi);
}
