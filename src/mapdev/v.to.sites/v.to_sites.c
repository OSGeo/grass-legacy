/*-
**  v.to_sites.c:  source for command line version
**
**  Written by Dave Gerdes  6/89
**
**  GRASS4.0 - converted for new parser - DKS 12/90
**  GRASS4.2 - added -d option - BB 07/98
**
**  US Army Construction Engineering Research Lab
**
*/
#include    <stdio.h>
#include    "gis.h"
#include    "site.h"
#include    "Vect.h"
#include    <math.h>

#define MAIN
#define  USAGE  "v.to.sites [-a][-c][-i][-d] input=dig file input output=site file output [dmax=value]\n"

/*
 * #define DEBUG
 */
/* command line args */
static char *dig_name = NULL;
static char *site_name = NULL;

int debugf(char *, int, int, int, int, int, int, int, int, int, int, int, int);
int export(char *, char *, char *, struct Categories *, int, int);
int doit(struct Map_info *, FILE *, struct Categories *, int, int);
int bin_to_asc(struct Map_info *, FILE *, struct Categories *, double, int, int);

int main (int argc, char **argv)
{
  char *mapset;
  char errmsg[100];
  char dmaxchar[100];
  struct Map_info Map;
  struct Categories cats;
  double dist;
  FILE *out;

  struct Cell_head cellhd;
  struct GModule *module;
  struct Option *old, *new, *dmax;
  struct Flag *cat, *Cat, *all, *interp, *dubl;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =
	"Converts point data in a binary GRASS vector map "
	"layer into a GRASS site_lists file.";

  /* get default for min distance */
  if (G_get_window (&cellhd) == -1)
    exit (0);
  if (cellhd.ew_res < cellhd.ns_res)
    dist = cellhd.ew_res;
  else
    dist = cellhd.ns_res;
  sprintf (dmaxchar, "%f", dist);

  /* check args and set flags  */

  old = G_define_option ();
  old->key = "input";
  old->type = TYPE_STRING;
  old->required = YES;
  old->multiple = NO;
  old->gisprompt = "old,dig,vector";
  old->description = "vector file to be converted to sites";

  new = G_define_option ();
  new->key = "output";
  new->type = TYPE_STRING;
  new->required = YES;
  new->multiple = NO;
  new->gisprompt = "new,site_lists,sites";
  new->description = "sites file to be made from vector file";

  all = G_define_flag ();
  all->key = 'a';
  all->description = "Output all verticies to site file";

  cat = G_define_flag ();
  cat->key = 'c';
  cat->description = "Use Category NUMERIC data instead of attribute";

  Cat = G_define_flag ();
  Cat->key = 'C';
  Cat->description = "Use Category TEXT data instead of attribute";

  interp = G_define_flag ();
  interp->key = 'i';
  interp->description = "Interpolate points along lies (for -a only)";

  dubl = G_define_flag ();
  dubl->key = 'd';
  dubl->description = "Write attribute as double instead of cat.";

  dmax = G_define_option ();
  dmax->key = "dmax";
  dmax->type = TYPE_DOUBLE;
  dmax->required = NO;
  dmax->answer = dmaxchar;
  dmax->description = "Maximum distance between points (for -a -i only) ";



  if (G_parser (argc, argv))
    exit (-1);

  dig_name = old->answer;
  site_name = new->answer;

  sscanf (dmax->answer, "%lf", &dist);

  if (dig_name == NULL || site_name == NULL)
  {
    fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
    G_usage ();
    exit (-1);
  }

  if (!*dig_name || !*site_name)
  {
    fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
    G_usage ();
    exit (-1);
  }


  /* Show advertising */
  fprintf (stdout,"\n\n   %s:\n\n", G_program_name ());
  if ((mapset = G_find_vector2 (dig_name, "")) == NULL)
  {
    sprintf (errmsg, "Could not find vector file %s\n", dig_name);
    G_fatal_error (errmsg);
  }

  if (cat->answer || Cat->answer)
  {
    if (G_read_vector_cats (dig_name, mapset, &cats) < 0)
    {
      sprintf (errmsg, "Could not find category file for %s\n", dig_name);
      G_fatal_error (errmsg);
    }
  }

  if (all->answer)
  {
    if ((Vect_open_old (&Map, old->answer, mapset)) < 2)
    {
      sprintf (errmsg, "Could not open vector file <%s> (run v.support to build topology)\n", dig_name);
      G_fatal_error (errmsg);
    }
    out = G_fopen_sites_new (site_name);
    bin_to_asc (&Map, out, (cat->answer || Cat->answer) ? &cats : NULL, 
	    interp->answer ? dist : 0., (cat->answer != '\0'), dubl->answer);
    Vect_close (&Map);
  }
  else
  {
    export (dig_name, mapset, site_name, (cat->answer || Cat->answer) ? &cats : NULL, (cat->answer != '\0'), dubl->answer);
  }
  exit (0);
}

#ifdef DEBUG
int 
debugf (char *format, int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l)
{
  fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif


int export (char *dig_name, char *mapset, char *site_name,
  struct Categories *cats, int num,int dubl)
{
  FILE *out;
  int level;
  struct Map_info Map;

  if (!mapset)
  {
    G_fatal_error ("No mapset specified.\n");
  }

  /*
   * dig_P_init (dig_name, mapset, &Map);
   */
  level = Vect_open_old (&Map, dig_name, mapset);
  if (level < 0)
    G_fatal_error ("Could not open vector file");

  if (level < 2)
    G_fatal_error ("Could not open file at Level 2 access, run v.support");

  out = G_fopen_sites_new (site_name);

  doit (&Map, out, cats, num,dubl);

  fclose (out);

  Vect_close (&Map);

  return (0);
}

int doit (struct Map_info *map, FILE *out, struct Categories *cats, int num, int dubl)
{
  P_LINE *Line;
  P_ATT *Att;
  struct line_pnts *Points;
  register int line, ret;
  int hits = 0;
  int labels = 0;
  int binary;
  char *p;
  Site *site;

  site = G_site_new_struct (CELL_TYPE, 2, 1, 1);

  /* make a quick pass through checking for labelled sites */
  for (line = 1; line <= map->n_lines; line++)
  {
    Line = &(map->Line[line]);
    if (Line->type != DOT)
      continue;
    hits++;
    if (Line->att)
      labels++;
  }
  if (hits)
  {
    binary = 0;
    if (!labels)
    {
      binary = 1;
      fprintf (stdout,"Creating a BINARY (0/1) site file\n");
    }
    else
    {
      fprintf (stdout,"Creating site file with category information\n");
      if (labels < hits)
      {
	fprintf (stdout,"Note: %d sites were not labeled\n", hits - labels);
      }
    }
  }
  else
  {
    fprintf (stdout,"No SITES found in vector file\n");
    return (0);
  }

  Points = Vect_new_line_struct ();

  for (line = 1; line <= map->n_lines; line++)
  {
    Line = &(map->Line[line]);
    if (Line->type != DOT)
      continue;

    site->ccat = 0;
    site->dbl_att[0] = 0.0;

    if (Line->att > 0)
    {
      Att = &(map->Att[Line->att]);

      if (!binary)
      {
	if (cats)
	{
	  p = G_get_cat (Att->cat, cats);
	  if (num)
	  {
	    sscanf (p, "%lf", &site->dbl_att[0]);
	  }
	  else
          {
	    site->str_att[0] = p;
	    site->dbl_alloc = 0;
          }
	}
	else
	{
	  if (dubl){
	      site->dbl_att[0] = Att->cat;
       	      site->cattype=-1;
	  }
	  else{
	      site->ccat = Att->cat;
	      site->dbl_alloc = 0;
	  }
	}
      }

      site->ccat = Att->cat;
      site->east = Att->x;
      site->north = Att->y;
      G_site_put (out, site);
      site->dbl_alloc = 1;
      site->str_alloc = 1;
    }
    else
    {
      if (0 > (ret = Vect__Read_line (map, Points, Line->offset)))
      {
	if (ret == -1)
	{
	  fprintf (stderr, "Out of memory on line %d\n", line);
	  return (-1);
	}
	else
	{
	  /* end of file */
	  fprintf (stderr, "Premature EOF. Dig_Plus file probably bad\n");
	  return (0);
	}
      }
      site->east = Points->x[0];
      site->north = Points->y[0];
      site->ccat = 0;
      site->dbl_alloc = 0;
      site->str_alloc = 0;
      /* mark it as label 0 (unlabelled) */
      if (binary || !num)
	/* G_put_site (out, Points->x[0], Points->y[0], ""); */
        site->cattype=-1;
      else
        site->cattype=CELL_TYPE;
      G_site_put (out, site);
      site->cattype = -1;
      site->dbl_alloc = 1;
      site->str_alloc = 1;
    }
  }

  Vect_destroy_line_struct (Points);

  fprintf (stdout,"\nFound %d sites\n", hits);

  return 0;
}

int 
bin_to_asc (struct Map_info *Map, FILE *ascii, struct Categories *cats, double dmax, int num, int dubl)
{
  register int i;
  double *xptr, *yptr, xprev, yprev, x, y, d, xt, yt;
  static struct line_pnts *Points;
  char *p;
  int att, n_points, times, j, k, ind1, ind2;
  int prev = 0;
  int isnode = 0;
  Site *site;

  site = G_site_new_struct (CELL_TYPE,2, 1, 1);	/* init site struct */
  Points = Vect_new_line_struct ();	/* init line_pnts struct */
  Vect_set_constraint_type (Map, LINE | DOT);


  for (i = 1; i <= Map->n_lines; i++)
  {
    if (Map->Line[i].att > 0)
    {
      if (0 > V2_read_line (Map, Points, i))
	G_fatal_error ("Read error");

      xptr = Points->x;
      yptr = Points->y;
      prev = 0;
      n_points = Points->n_points;

      while (n_points--)
      {
	if ((n_points == Points->n_points - 1) || (n_points == 0))
	  isnode = 1;
	else
	  isnode = 0;
	att = Map->Att[Map->Line[i].att].cat;
	site->ccat = att;
	site->cattype = CELL_TYPE;
        site->dbl_att[0]=0.0;
	site->dbl_alloc = 1;
	if (dubl){
	    site->dbl_att[0] = att;
       	    site->cattype=-1;
	}

	if (cats)
	{
	  p = G_get_cat (att, cats);
	  if (num)
	  {
	    sscanf(p,"%lf",&site->dbl_att[0]);
	    site->str_alloc = 0;
	  }
	  else
	  {
	    site->str_att[0] = p;
	    site->dbl_alloc = 0;
	  }
	}
        else
        {
          if (!dubl) site->dbl_alloc = 0;
          site->str_alloc = 0;
        }

	if (prev == 0)
	{
	  xprev = *xptr;
	  yprev = *yptr;
	  prev = 1;
	  if (!isnode)
	  {
	    /* G_put_site (ascii, *xptr++, *yptr++, descp); */
	    site->east = *xptr++;
	    site->north = *yptr++;
	    G_site_put (ascii, site);
	    /*  use same
       	    site->cattype=-1;
            site->dbl_att[0] = 0;
	    site->str_alloc = 1;
	    site->dbl_alloc = 1;
	    */
	  }
	}
	else
	{
	  /* compare the distance between current and previous */
	  x = *xptr;
	  y = *yptr;
	  xt = fabs (x - xprev);
	  yt = fabs (y - yprev);
	  d = sqrt (xt * xt + yt * yt);
	  if ((d > dmax) && (dmax != 0.))
	  {
	    times = (int) (d / dmax + 0.5);
	    for (j = 0; j < times; j++)
	    {
	      xt = x - j * ((x - xprev) / times);
	      yt = y - j * ((y - yprev) / times);
	      if ((!isnode) || (j != 0))
	      {
		site->east = xt;
		site->north = yt;
                /* site->cattype=CELL_TYPE; */
		G_site_put (ascii, site);
		/*  use same
	        site->cattype = -1;
                site->dbl_att[0] = 0;
	        site->str_alloc = 1;
	        site->dbl_alloc = 1;
		*/
	      }
	      isnode = 0;

	    }
	    xptr++;
	    yptr++;
	  }
	  else
	  {
	    if (!isnode)
	    {
	      /* G_put_site (ascii, *xptr++, *yptr++, descp); */
	      site->east = *xptr++;
	      site->north = *yptr++;
              /* site->cattype=CELL_TYPE; */
	      G_site_put (ascii, site);
	      /*  use same
	      site->cattype = -1;
              site->dbl_att[0] = 0;
	      site->str_alloc = 1;
	      site->dbl_alloc = 1;
	      */
	    }
	  }
	  xprev = x;
	  yprev = y;
	}

      }
    }
  }

  fprintf (stderr, "\nNumber of nodes = %d\n", Map->n_nodes);
  for (k = 1; k <= Map->n_nodes; k++)
  {
    x = Map->Node[k].x;
    y = Map->Node[k].y;

    if (Map->Node[k].lines != NULL)
    {
      ind1 = abs (Map->Node[k].lines[0]);
      ind2 = Map->Line[ind1].att;
      att = Map->Att[ind2].cat;

      site->cattype=CELL_TYPE;
      site->ccat = att;
      site->dbl_att[0]=0.0;
      if (dubl){
  	  site->dbl_att[0] = att;
	  site->cattype=-1;
      }

      if (cats)
      {
        p = G_get_cat (att, cats);
        if (num)
        {
          sscanf(p,"%lf",&site->dbl_att[0]);
          site->str_alloc = 0;
        }
        else
        {
          site->str_att[0] = p;
          site->dbl_alloc = 0;
        }
      }
      else
      {
        if (!dubl) site->dbl_alloc = 0;
        site->str_alloc = 0;
      }

      site->east = x;
      site->north = y;
      G_site_put (ascii, site);
      site->cattype = -1;
      site->dbl_att[0] = 0;
      site->dbl_alloc = 1;
      site->str_alloc = 1;
    }
  }

  fclose (ascii);
  return (0);
}
