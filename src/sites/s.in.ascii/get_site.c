#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "local_proto.h"
#include <ctype.h>

#define isnull(c) (c==(char)NULL)

static char *my_next_att (char *);

Site *get_site (FILE *fd, int dims, char *fs, int *has_cat)
{
  static char *ibuf = NULL, *buf = NULL, *save = NULL;
  char *b;
  char ebuf[256], nbuf[256];
  static int first = 1;
  static int line = 0;
  static int tty;
  static int proj;
  int i, err,itmp;
  int n = 0;			/* number of categories */
  int d = 0;			/* number of floating point attributes */
  int c = 0;			/* number of string attributes */
  float ftmp;
  static Site *site;

  if (first)
  {
    site = G_site_new_struct (-1, dims, 0, 0);
  
    ibuf = G_malloc (1024 * sizeof (char));
    buf = G_malloc (1024 * sizeof (char));
    save = buf;
    if (ibuf == NULL || buf == NULL)
      G_fatal_error ("memory allocation errory");
    tty = isatty (fileno (fd));
    proj = G_projection ();
    if (tty)
    {
      fprintf (stdout,"Enter sites, one per line, in the format:\n");
      fprintf (stdout,"east north ");
      for (i = 3; i <= dims; ++i)
	fprintf (stdout,"dim%d ", i);
      fprintf (stdout,"attributes\n");
      fprintf (stdout,"When finished, type: end\n");
    }
    first = 0;
  }
  err = 1;
  while (err)
  {
    if (tty)
      fprintf (stdout,"location attributes> ");
    else
      line++;

    if (!G_getl (ibuf, 1024, fd))
      return (Site *) NULL;
    buf = save;

    strcpy (buf, ibuf);
    G_squeeze (buf);
    if (*buf == 0)
      return (Site *) NULL;
    if (strcmp (buf, "end") == 0)
      return (Site *) NULL;
    if (fs)
    {
      for (b = buf; *b; b++)
	if (*b == *fs)
	  *b = ' ';
    }
    if (sscanf (buf, "%s %s", ebuf, nbuf) != 2
	|| !G_scan_easting (ebuf, &site->east, proj)
	|| !G_scan_northing (nbuf, &site->north, proj))
    {
      if (!tty)
      {
	fprintf (stderr, "%s - line %d ",
		 G_program_name (), line);
      }
      fprintf (stderr, "** invalid format **\n");
      if (!tty)
	fprintf (stderr, "<%s>\n", ibuf);
      /* return (Site *) NULL; */
    }
    else
      err = 0;
  }

  buf += strlen (ebuf) + strlen (nbuf) + 2;
  dims -= 2;
  for (i = 0; i < dims; ++i)
  {
    sscanf (buf, "%s", ebuf);
    sscanf (ebuf, "%lf", &(site->dim[i]));
    buf += strlen (ebuf) + 1;
  }

  /* no more dimensions-now we parse attribute fields */
  while (buf != NULL)
  {
    switch (*buf)
    {
    /* check for prefixed atts first, then without prefix */
    case '#':			/* category field */
      if (n == 0)
      {
        sscanf(buf,"#%s ",ebuf);
        if(G_strstr(ebuf,".")==NULL && sscanf(ebuf,"%d", &itmp) ==1)
        {
          site->cattype=CELL_TYPE;
          site->ccat=itmp;
	  n++;
        }
        else if(G_strstr(ebuf,".")!=NULL && sscanf(ebuf,"%f", &ftmp) ==1)
        {
          site->cattype=FCELL_TYPE;
          site->fcat=ftmp;
	  n++;
        }
        else 
          site->cattype=-1;
      }
      else
	G_warning ("Only one category attribute allowed per record; ignoring");

      /* move to beginning of next attribute */
      buf = my_next_att (buf);
      break;
    case '%':			/* decimal attribute */
      if (d >= site->dbl_alloc)
      {
	site->dbl_alloc++;
	site->dbl_att = (double *) G_realloc (site->dbl_att,
				      site->dbl_alloc * sizeof (double));
      }
      if ((err = sscanf (buf, "%%%lf", &(site->dbl_att[d++]))) < 1)
	G_warning ("error scanning floating point attribute");
      buf = my_next_att (buf);
      break;
    case '@':			/* string attribute */
      if (isnull (*buf) || isnull (*(buf + 1)))
      {
	*buf = '\0';
	break;
      }
      else
	buf++;
    default: /* changed to unprefixed decimals */
   /* commented 12/99: default shall be decimal field! M.N.*/
                    /* defaults to string attribute */
      /* allow both prefixed and unprefixed strings */
   /* 
      if (c >= site->str_alloc)
      {
	site->str_alloc++;
	site->str_att = (char **) G_realloc (site->str_att,
				      site->str_alloc * sizeof (char *));
	if (site->str_att == NULL)
	  G_fatal_error ("memory allocation error");
	site->str_att[site->str_alloc - 1] = (char *)
	  G_malloc (MAX_SITE_STRING * sizeof (char));
	if (site->str_att[site->str_alloc - 1] == NULL)
	  G_fatal_error ("memory allocation error");
      }

      if ((err = cleanse_string (buf)) > 0)
      {
	G_strncpy (site->str_att[c++], buf, err);
	buf += err;
      }
      else
	*buf = '\0';
      buf = my_next_att (buf);
      break;
     ***/ /* end of comment (default=strings) */
     
     			/* default is unprefixed decimal attribute */
      if (d >= site->dbl_alloc)
      {
	site->dbl_alloc++;
	site->dbl_att = (double *) G_realloc (site->dbl_att,
				      site->dbl_alloc * sizeof (double));
      }
      if ((err = sscanf (buf, "%lf", &(site->dbl_att[d++]))) < 1)
	G_warning ("error scanning floating point attribute");
      buf = my_next_att (buf);
      break;
    }
  }
  *has_cat = n;
  return site;
}


static char *my_next_att (char *buf)
{
  while (!isspace (*buf) && !isnull (*buf))
    buf++;
  if (isnull (*buf) || isnull (*(buf + 1)))
    return (char *) NULL;
  else
    while (isspace (*(buf + 1)) && !isnull (*(buf + 1)))
      buf++;
  buf++;
  return buf;
}
