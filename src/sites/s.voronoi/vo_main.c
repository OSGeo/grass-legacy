/*
* $Id$
*
****************************************************************************
*
* MODULE:       s.voronoi
* AUTHOR(S):    James Darrell McCauley (mccauley@ecn.purdue.edu)
*               USDA Fellow
*               Department of Agricultural Engineering
*               Purdue University
*               West Lafayette, Indiana 47907-1146 USA
*
*               Aime Andrea
*               Italy - aaime@libero.it
* PURPOSE:      To generate a voronoi diagram starting from a site file
* HISTORY:      09 Apr 92 - James Darrell McCauley <mccauley@ecn.purdue.edu> pieced
*               this together from stuff he found on netlib (see the manpage)
*               22 Apr 01 - Aime Andrea <aaime@libero.it> module rewrite to get good
*               memory management, support GRASS 5 site API, handle some degenerate
*               case (such as horizontal and vertical lines in voronoi diagram) and
*               get a file ready for v.support to build (without the intervention of
*               v.spag). Also removed some silly output that was needed only for
*               debug purposes.
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*               License (>=v2). Read the file COPYING that comes with GRASS
*               for details.
*
*****************************************************************************/


#define MAIN
#define		DIG_DIR		"dig"
#define		PLUS_DIR	"dig_plus"
#define		ATT_DIR		"dig_att"
#define		DIG_CAT_DIR	"dig_cats"
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "vo_defs.h"
#include "site.h"

struct Cell_head window;
float **data;
int *cats;
char **desc;

int 
main (int argc, char **argv)
{
  int all, generate, labels;
  int verbose;
  int ret;
  char *mapset, *sitefile, buf[128], errmsg[200];
  char *tmpfile;
  static char att_file[128];
  static char cat_file[128];
  extern struct Cell_head window;
  struct Map_info Map;
  FILE *fdsite;
  FILE *Att;
  struct
  {
    struct Flag *all, *q;
  } flag;
  struct
  {
    struct Option *input, *output, *labels;
  } parm;

  struct GModule *module;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description = 
    "Create a Voronoi diagram (Thiessen polygon) from a sites list "
    "in a GRASS binary vector file. ";

  parm.input = G_define_option ();
  parm.input->key = "sites";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "name of a sites file to be input";
  parm.input->gisprompt = "old,site_lists,sites,input";

  parm.output = G_define_option ();
  parm.output->key = "vect";
  parm.output->type = TYPE_STRING;
  parm.output->required = YES;
  parm.output->description = "name of a vector file to be output";
  parm.output->gisprompt = "new,dig,binary file,output";

  parm.labels = G_define_option();
  parm.labels->key = "labels";
  parm.labels->type = TYPE_STRING;
  parm.labels->options = "no,generate,keep";
  parm.labels->answer = "keep";
  parm.labels->description = "no labels, generate a serial category number or keep category from input file";

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);
  all = flag.all->answer;
  verbose = (!flag.q->answer);
  generate = !strcmp(parm.labels->answer, "generate");
  labels = strcmp(parm.labels->answer, "no");

  mapset = G_store (G_mapset ());

  /* check for existance of support directories  */
  G__make_mapset_element (ATT_DIR);
  G__make_mapset_element (DIG_CAT_DIR);

  /* get fullpath to file names -> it seems unnecessary to me */
  /* G__file_name (att_file, ATT_DIR, parm.output->answer, mapset);
     G__file_name (cat_file, DIG_CAT_DIR, parm.output->answer, mapset);
     Map.att_file = att_file; */

  /* start opening files */
  if (0 > Vect_open_new (&Map, parm.output->answer))
  {
    sprintf (errmsg, "Not able to open vector file <%s>\n",
             parm.output->answer);
    G_fatal_error (errmsg);
  }

  /* dig_att file for label points */
  Att = G_fopen_new ("dig_att", parm.output->answer);
  if(Att == NULL) {
    sprintf (errmsg, "Can't open attribute file for write: %s\n", att_file);
    G_fatal_error (errmsg);
  }

  /* open input site file */
  sitefile = parm.input->answer;
  mapset = G_find_file ("site_lists", sitefile, "");
  if (mapset == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", sitefile);
    G_fatal_error (errmsg);
  }

  fdsite = G_sites_open_old (sitefile, mapset);
  if (fdsite == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", sitefile);
    G_fatal_error (errmsg);
  }

  /* initialize working region */
  if (!all)
    G_get_window (&window);
  else
    G_get_default_window (&window);

  init_header (Map.dig_fp, &window, &Map.head);
  tmpfile = G_tempfile ();
  if(verbose) fprintf(stdout, "Voronoi diagram calculation\n");
  sprintf(buf, "s.out.ascii %s | $GISBASE/etc/s.sweep -d > %s", sitefile, tmpfile);
  G_system(buf);
  write_polygons (&Map, tmpfile, verbose);
  if(labels)
  {
	  if(verbose) fprintf(stdout, "Generating labels\n");
	  if((ret = write_cats_att (parm.output->answer, Att, fdsite,
	                            Map.head.map_name, window, generate)) < 0)
	  {
        	switch(ret) {
	            case -1:
        	        G_warning("Failed to guess format");
	                break;
        	    case -2:
		        G_warning("Category numbers are not of integer type");
	                break;
        	    case -3:
                	G_warning("Memory allocation failure");
	                break;
        	    case -4:
                	G_warning("Failed to write out label points");
	        }
        	G_fatal_error("\nLabel points have not been generated");
	  }
   }

  Vect_close (&Map);
  unlink(tmpfile);

  return 0;
}
