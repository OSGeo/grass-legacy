/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       s.in.ascii
 * AUTHOR(S):    Original author unknown - probably CERL
 *               Markus Neteler - neteler@geog.uni-hannover.de
 * PURPOSE:      Import ASCII sites lists and their descriptions into
 *               a GRASS sites list file. 
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <string.h>
#include "gis.h"
#include "local_proto.h"

/* 
 * $Id$ */

/* 12/99 removed elev data flag. MN. not required any more */

static int loop; /* added #cat support for site_list 11/99 M. Neteler
                  * required for s.to.vect and s.to.rast */

int 
main (int argc, char *argv[])
{
    char me;
    char *output, *input;
    char *fs;
    int dims, i, has_cat;
    struct GModule *module;
    FILE *in_fd, *out_fd;
    Site *site;
    struct
    {
	struct Option *input, *output, *dims, *fs;
    } parm;
    struct Flag *elev;

    G_gisinit (me = argv[0]);

    module = G_define_module();

    module->description = 
      "Convert an ASCII listing of site locations "
      "into a GRASS site list file.";

    parm.output = G_define_option();
    parm.output->key = "sites";
    parm.output->type = TYPE_STRING;
    parm.output->required = YES;
    parm.output->description = "sites file to be created";
    parm.output->gisprompt = "any,site_lists,sites";

    parm.input = G_define_option();
    parm.input->key = "input";
    parm.input->type = TYPE_STRING;
    parm.input->required = NO;
    parm.input->description = "unix file containing sites";

    parm.dims = G_define_option();
    parm.dims->key = "d";
    parm.dims->type = TYPE_INTEGER;
    parm.dims->required = NO;
    parm.dims->description = "number of dimensions (default=2)";

    parm.fs = G_define_option();
    parm.fs->key = "fs";
    parm.fs->key_desc = "character|space|tab";
    parm.fs->type = TYPE_STRING;
    parm.fs->required = NO;
    parm.fs->description = "input field separator";
    parm.fs->answer = "space";

    if (G_parser(argc,argv))
	exit(1);
    if((input = parm.input->answer))
    {
	in_fd = fopen (input, "r");
	if (NULL == in_fd)
	{
	    fprintf (stderr, "%s - ", me);
	    perror (input);
	    exit(1);
	}
    }
    else
	in_fd = stdin;

    output = parm.output->answer;

    dims=2;
    loop=1; /* added 11/99 MNeteler*/

    if (parm.dims->answer!=NULL)
      if ((i = sscanf (parm.dims->answer, "%d", &dims)) != 1)
        G_fatal_error ("error scanning number of dimensions");
    if (dims<2)
      G_fatal_error ("number of dimensions must be greater than 1");

    fs = parm.fs->answer;
    if (fs != NULL)
    {
	if(strcmp (fs, "space") == 0)
	    fs = NULL;
	else if(strcmp (fs, "tab") == 0)
	    fs = NULL;
    }

    out_fd = G_fopen_sites_new (output);
    if (out_fd == NULL)
    {
	fprintf (stderr, " %s - can't create sites file [%s]",
		me, output);
	exit(1);
    }

    while ((site = get_site (in_fd, dims, fs, &has_cat)))
      G_site_put_new (out_fd, site, has_cat);
    fclose (out_fd);
    exit(0);
}
static int 
format_double (double value, char *buf)
{
  int G_trim_decimal ();
  sprintf (buf, "%.8f", value);
  G_trim_decimal (buf);
  return 0;
}

#define DQUOTE '"'
#define SPACE ' '
#define BSLASH 92
#define PIPE '|'

#define ispipe(c) (c==PIPE)
#define isnull(c) (c==(char)NULL)
#define isquote(c) (c==DQUOTE)

int 
G_site_put_new (FILE *fptr, Site *s, int has_cat)

/* Writes a site to file open on fptr. */
{
  char ebuf[MAX_SITE_STRING], nbuf[MAX_SITE_STRING];
  char xbuf[MAX_SITE_STRING], buf[MAX_SITE_LEN];
  static int format_double ();
  int fmt, i, j, k;
  int G_format_northing(), G_format_easting(), G_projection();

  fmt = G_projection ();

  G_format_northing (s->north, nbuf, fmt);
  G_format_easting (s->east, ebuf, fmt);
  sprintf (buf, "%s|%s|", ebuf, nbuf);
  for (i = 0; i < s->dim_alloc; ++i)
  {
    format_double (s->dim[i], nbuf);
    sprintf (xbuf, "%s|", nbuf);
    G_strcat (buf, xbuf);
  }

 if (has_cat)  
  {
    switch(s->cattype)
    {
     case CELL_TYPE:
      sprintf (xbuf, "#%d ", s->ccat);
      G_strcat (buf, xbuf);
      break;
     case FCELL_TYPE:
      sprintf (xbuf, "#%g ", s->fcat);
      G_strcat (buf, xbuf);
      break;
     case DCELL_TYPE:
      sprintf (xbuf, "#%g ", s->dcat);
      G_strcat (buf, xbuf);
      break;
    }
  }                                                    
  else /* no cat there, so data in plain x,y,z format will be imported   12/99 MN */
  {
     /* we create a #cat entry in site_list from the current site number 11/99 */
     sprintf (xbuf, "#%d ", loop);
     loop++;
     G_strcat (buf, xbuf);
  }

 /* now import attributes */
  for (i = 0; i < s->dbl_alloc; ++i)
  {
    format_double (s->dbl_att[i], nbuf);
    sprintf (xbuf, "%%%s ", nbuf);
    G_strcat (buf, xbuf);
  }
  
  for (i = 0; i < s->str_alloc; ++i)
  {
    if (strlen (s->str_att[i]) != 0)
    {
      /* escape double quotes */
      j = k = 0;
      if (G_index (s->str_att[i], DQUOTE) != (char *) NULL)
      {
	while (!isnull(s->str_att[i][j]))
	{
	  if (isquote(s->str_att[i][j]))
	  {
	    xbuf[k++] = BSLASH;
	    xbuf[k++] = DQUOTE;
	  }
	  else
	    xbuf[k++] = s->str_att[i][j];
	  j++;
	}
	xbuf[k] = (char) NULL;
      }
      else
	G_strcpy (xbuf, s->str_att[i]);

      G_strcpy (s->str_att[i], xbuf);

      if (G_index (s->str_att[i], SPACE) != (char *) NULL)
	  sprintf (xbuf, "@\"%s\" ", s->str_att[i]);
        else
	  sprintf (xbuf, "@%s ", s->str_att[i]);

      G_strcat (buf, xbuf);
    }
  }
  fprintf (fptr, "%s\n", buf);
  return 0;
}
