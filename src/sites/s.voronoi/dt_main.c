/*-s.delaunay
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
** 06 Feb 93 - James Darrell McCauley <mccauley@ecn.purdue.edu> pieced
**             this together from stuff he found on netlib (see the manpage).
**/

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
  int all;
  int verbose;
  int lineType;
  char *mapset, *sitefile, buf[1024], errmsg[200];
  char *tmpfilet;
  char aflag[3];
  extern struct Cell_head window;
  struct Map_info Map;
  struct
  {
    struct Flag *all, *q, *line;
  } flag;
  struct
  {
    struct Option *input, *output;
  } parm;
  struct GModule *module;
  FILE *fdsite;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description = 
    "Create a Delaunay triangulation from a sites list "
    "in a GRASS binary vector file.";

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

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.line = G_define_flag ();
  flag.line->key = 'l';
  flag.line->description = "Output triangulation as a graph, not areas";

  if (G_parser (argc, argv))
    exit (1);
  all = flag.all->answer;
  verbose = (!flag.q->answer);
  lineType = flag.line->answer ? LINE : AREA;

  mapset = G_store (G_mapset ());

  /* open files */
  if (0 > Vect_open_new (&Map, parm.output->answer))
  {
    sprintf (errmsg, "Not able to open vector file <%s>\n",
	     parm.output->answer);
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

  tmpfilet = G_tempfile ();
  sprintf(buf, "s.out.ascii %s sites=%s | $GISBASE/etc/s.sweep -t > %s", aflag,
          sitefile, tmpfilet);
  G_system(buf);
  write_triangles (&Map, fdsite, tmpfilet, lineType, verbose);
  Vect_close (&Map);
  unlink(tmpfilet);

  return 0;
}
