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
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "vo_defs.h"
#include "dt_defs.h"
#include "site.h"

struct Cell_head window;
float **data;
int *cats;
char **desc;

int extractFieldIndex(char*, int);

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
  static char att_file[128];
  static char cat_file[128];
  struct Map_info Map;
  struct
  {
    struct Flag *all, *q, *line, *area;
  } flag;
  struct
  {
    struct Option *input, *output;
  } parm;
  struct GModule *module;
  FILE *fdsite;
  FILE *Att;

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

  flag.area = G_define_flag ();
  flag.area->key = 's';
  flag.area->description = "Store area sizes of triangles as category instead of triangle number";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.line = G_define_flag ();
  flag.line->key = 'l';
  flag.line->description = "Output triangulation as a graph (lines), not areas";

  if (G_parser (argc, argv))
    exit (1);
  all = flag.all->answer;
  verbose = (!flag.q->answer);
  lineType = flag.line->answer ? LINE : AREA;

  if (flag.line->answer && flag.area->answer)
     G_fatal_error("Either define -l or -s!");
       
  mapset = G_store (G_mapset ());

  /* check for existance of support directories  */
  G__make_mapset_element (ATT_DIR);
  G__make_mapset_element (DIG_CAT_DIR);

  /* get fullpath to file names -> it seems unnecessary to me */
  /* G__file_name (att_file, ATT_DIR, parm.output->answer, mapset);
     G__file_name (cat_file, DIG_CAT_DIR, parm.output->answer, mapset);
     Map.att_file = att_file; */

  /* open files */
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

  tmpfilet = G_tempfile ();
  sprintf(buf, "s.out.ascii %s sites=%s | $GISBASE/etc/s.sweep -t > %s", aflag,
          sitefile, tmpfilet);
  G_system(buf);
  dt_write_triangles (&Map, fdsite, Att, parm.output->answer, tmpfilet, lineType, verbose, flag.area->answer);

  Vect_close (&Map);
  unlink(tmpfilet);

  return 0;
}

int extractFieldIndex(char *str, int offset) {
  char *numPtr, errmsg[255];
  int numCharToCopy;
  int labelIndex;

  numCharToCopy = strlen(str) - offset;
  if(numCharToCopy < 0) {
    sprintf (errmsg, "Field index not specified\n");
    G_fatal_error (errmsg);
  }

  numPtr = str + offset; /* move after comma */
  labelIndex = atoi(numPtr);
  if(labelIndex <= 0) {
    sprintf (errmsg, "Field index not specified or incorrect value (must be > 0)\n");
    G_fatal_error (errmsg);
  }

  return labelIndex;
}
