/*-s.voronoi
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
** 09 Apr 92 - James Darrell McCauley <mccauley@ecn.purdue.edu> pieced
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

struct Cell_head window;
float **data;
int *cats;
char **desc;

int 
main (int argc, char **argv)
{
  int all, label;
  int verbose;
  char *mapset, *sitefile, buf[128], errmsg[200];
  char *tmpfile;
  static char att_file[128];
  static char cat_file[128];
  extern struct Cell_head window;
  struct Map_info Map;
  FILE *fdsite;
  struct
  {
    struct Flag *all, *label, *q;
  } flag;
  struct
  {
    struct Option *input, *output;
  } parm;


  G_gisinit (argv[0]);

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

  flag.label = G_define_flag ();
  flag.label->key = 'l';
  flag.label->description = "Create dig_att and dig_cat files";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);
  all = flag.all->answer;
  label = flag.label->answer;
  verbose = (!flag.q->answer);

  mapset = G_store (G_mapset ());

  /* check for existance of support directories  */
  G__make_mapset_element (ATT_DIR);
  G__make_mapset_element (DIG_CAT_DIR);

  /* get fullpath to file names */
  G__file_name (att_file, ATT_DIR, parm.output->answer, mapset);
  G__file_name (cat_file, DIG_CAT_DIR, parm.output->answer, mapset);
  Map.att_file = att_file;

  /* start opening files */
  if (0 > Vect_open_new (&Map, parm.output->answer))
  {
    sprintf (errmsg, "Not able to open vector file <%s>\n",
	     parm.output->answer);
    G_fatal_error (errmsg);
  }

  if (label)
  {
    if ((Map.att_fp = fopen (Map.att_file, "w+")) == NULL)
    {
      sprintf (errmsg, "Can't open attribute file for write: %s\n", att_file);
      G_warning (errmsg);
      G_warning ("Attributes will be left off\n");
      label = 0;
    }
/*
    if (label && (fdcats = fopen (cat_file, "w+")) == NULL)
    {
      sprintf (errmsg, "Can't open category file for write: %s\n", att_file);
      G_warning (errmsg);
      G_warning ("Attributes will be left off\n");
      label = 0;
      fclose (Map.att_fp);
    }
*/
  }

  sitefile = parm.input->answer;
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

  init_header (Map.dig_fp, &window, &Map.head);
  tmpfile = G_tempfile ();
  sprintf(buf, "s.out.ascii %s | $GISBASE/etc/s.sweep -d > %s", sitefile, tmpfile);
  G_system(buf);
  write_polygons (&Map, tmpfile, verbose);
  if (label)
    write_cats_att (parm.output->answer, Map.att_fp, fdsite, all, verbose); 
  Vect_close (&Map);
  unlink(tmpfile);

  return 0;
}
