/*-
 * These functions and definitions support the site format for 5.0
 * (format proposed by Dave Gerdes):
 *
 * easting|northing|[z|[d4|]...][#category_int] [ [@attr_text OR %flt] ... ]
 *
 * to allow multidimensions (everything preceding the last '|') and any
 * number of text or numeric attribute fields.
 *
 * Author: James Darrell McCauley <mccauley@ecn.purdue.edu>
 * 31 Jan 1994
 */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "gis.h"
#include "site.h"

#define DQUOTE '"'
#define SPACE ' '
#define BSLASH 92
#define PIPE '|'

#define ispipe(c) (c==PIPE)
#define isnull(c) (c==(char)NULL)
#define isquote(c) (c==DQUOTE)
#define isbslash(c) (c==BSLASH)

static int format_double ( double , char *);
char *next_att (char *);

void G_site_free_struct (Site *s)
/* Free memory for a Site struct */
{
  if(s->dim_alloc)
      G_free(s->dim);
  if(s->str_alloc)
      G_free(s->str_att);
  if(s->dbl_alloc)
      G_free(s->dbl_att);
  G_free(s);

  return;
}

Site *G_site_new_struct (RASTER_MAP_TYPE cattype,
  int n_dim,int n_s_att,int n_d_att)
/* Allocate memory for a Site struct. Returns a properly allocated
   site struct or NULL on error. 
   cattype= -1 (no cat), CELL_TYPE, FCELL_TYPE, or DCELL_TYPE 
*/
{  
  int i;
  Site *s;

  if (n_dim < 2 || n_s_att < 0 || n_d_att < 0)
    G_fatal_error ("G_oldsite_new_struct: invalid # dims or fields\n");

  if ((s = (Site *) G_malloc (sizeof (Site))) == NULL)
    return (Site *) NULL;

  s->cattype=cattype;
  s->ccat=s->fcat=s->dcat=0;

  if (n_dim > 2)
  {
    if ((s->dim =
	 (double *) G_malloc ((n_dim - 2) * sizeof (double))) == NULL) {
      G_free(s);
      return (Site *) NULL;
    }
  }
  s->dim_alloc = n_dim - 2;

  if (n_d_att > 0)
  {
    if ((s->dbl_att =
	 (double *) G_malloc (n_d_att * sizeof (double))) == NULL) {
      if (n_dim > 2) G_free(s->dim);
      G_free(s);
      return (Site *) NULL;
    }
  }
  s->dbl_alloc = n_d_att;

  if (n_s_att > 0)
  {
    if ((s->str_att =
	 (char **) G_malloc (n_s_att * sizeof (char *))) == NULL) {
      if (n_d_att > 0) G_free(s->dbl_att);
      if (n_dim > 2) G_free(s->dim);
      G_free(s);
      return (Site *) NULL;
    }
    else
      for (i = 0; i < n_s_att; ++i)
	if ((s->str_att[i] =
	  (char *) G_malloc (MAX_SITE_STRING * sizeof (char))) == NULL) {
          while (--i) G_free(s->str_att[i]);
          G_free(s->str_att);
          if (n_d_att > 0) G_free(s->dbl_att);
          if (n_dim > 2) G_free(s->dim);
          G_free(s);
	  return (Site *) NULL;
        }
  }
  s->str_alloc = n_s_att;

  return s;
}

#define FOUND_ALL(s,n,dim,c,d) (((s->cattype != -1 && !n) || \
				 (dim < s->dim_alloc) || \
				 (c < s->str_alloc) || \
				 (d < s->dbl_alloc))?0:1)

int G_oldsite_get ( FILE *fptr, Site *s)
/* Writes a site to file open on fptr. */
{
    return G__oldsite_get (fptr, s, G_projection() );
}


int G__oldsite_get ( FILE *ptr, Site *s, int fmt)
/*-
 * Reads ptr and returns 0 on success,
 *                      -1 on EOF,
 *                      -2 on other fatal error or insufficient data,
 *                       1 on format mismatch (extra data)
 */
{
  char sbuf[MAX_SITE_LEN], *buf, *last, *p1, *p2;
  char ebuf[128], nbuf[128];
  int n = 0, d = 0, c = 0, dim = 0, err = 0, tmp;

  buf = sbuf;

  if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
    return EOF;

  while ((*buf == '#' || !isdigit(*buf)) && *buf != '-' && *buf != '+')
    if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
      return EOF;

  if (buf[strlen (buf) - 1] == '\n')
    buf[strlen (buf) - 1] = (char) NULL;

  if (sscanf (buf, "%[^|]|%[^|]|%*[^\n]", ebuf, nbuf) < 2)
  {
    fprintf (stderr, "ERROR: ebuf %s nbuf %s\n", ebuf, nbuf);
    return -2;
  }

  if (!G_scan_northing (nbuf, &(s->north), fmt) ||
      !G_scan_easting (ebuf, &(s->east), fmt))
  {
    fprintf (stderr, "ERROR: ebuf %s nbuf %s\n", ebuf, nbuf);
    return -2;
  }

  /* move pointer past easting and northing fields */
  if (NULL == (buf = G_index(buf, PIPE)))
    return -2;
  if (NULL == (buf = G_index(buf+1, PIPE)))
    return -2;

  /* check for remaining dimensional fields */
  do {
    buf++;
    if (isnull(*buf)) return (FOUND_ALL(s,n,dim,c,d)? 0: -2);
    last = buf;
    if (dim < s->dim_alloc)  /* should be more dims to read */
    {
      if (sscanf (buf, "%lf|", &(s->dim[dim++])) < 1)
	return -2;   /* no more dims, though expected */ 
    }
    else if (NULL != (p1 = G_index(buf, PIPE))) 
    {
	if (NULL == (p2 = G_index(buf, DQUOTE)))
	    err = 1;  /* more dims, though none expected */
	else if (strlen(p1) > strlen(p2))
	    err = 1;  /* more dims, though none expected */
    }
  } while ((buf = G_index (buf, PIPE)) != NULL);
  buf = last;

  /* no more dimensions-now we parse attribute fields */
  while (!isnull(*buf))
  {
    switch (*buf)
    {
    case '#':			/* category field */
      if (n == 0)
      {
        switch(s->cattype)
        {
          case CELL_TYPE:
          if(sscanf (buf, "#%d", &s->ccat)==1)
	    n++;
          break;
          case FCELL_TYPE:
          if(sscanf (buf, "#%f", &s->fcat)==1)
	    n++;
          break;
          case DCELL_TYPE:
          if(sscanf (buf, "#%lf", &s->dcat)==1)
	    n++;
          break;
	  default:
	  err = 1; /* has cat, none expected */
	  break;
        }
      }
      else{
	err = 1;  /* extra cat */
      }

      /* move to beginning of next attribute */
      if ((buf = next_att (buf)) == (char *) NULL)
	return (FOUND_ALL(s,n,dim,c,d)? err: -2);
      break;

    case '%':			/* decimal attribute */
      if (d < s->dbl_alloc) {
	p1 = ++buf;
        errno = 0;
	s->dbl_att[d++] = strtod(buf, &p1);
	if (p1 == buf || errno == ERANGE) {
		/* replace with:
		 * s->dbl_att[d - 1] = NAN
		 * when we add NULL attribute support
		 */
		return -2;
	}
	/* err = 0; Make sure this is zeroed */
      } else {
	 err = 1;  /* extra decimal */
      }

      if ((buf = next_att (buf)) == (char *) NULL) {
	return (FOUND_ALL(s,n,dim,c,d)) ? err : -2;
      }
      break;
    case '@':			/* string attribute */
      if (isnull(*buf) || isnull(*(buf + 1)))
	return (FOUND_ALL(s,n,dim,c,d)? err: -2);
      else
	buf++;
    default:			/* defaults to string attribute */
      /* allow both prefixed and unprefixed strings */
      if (c < s->str_alloc)
      {
	if ((tmp = cleanse_string (buf)) > 0)
	{
	  G_strncpy (s->str_att[c++], buf, tmp);
	  buf += tmp;
	}
	else
	  return (FOUND_ALL(s,n,dim,c,d)? err: -2);
      }
      if ((buf = next_att (buf)) == (char *) NULL) {
	return (FOUND_ALL(s,n,dim,c,d)? err: -2);
      }
      break;
    }
  }

  return (FOUND_ALL(s,n,dim,c,d)? err: -2);
}

int G_oldsite_describe ( FILE *ptr,
  int *dims,int *cat,int *strs,int *dbls)
/*-
 * Tries to guess the format of a sites list (the dimensionality,
 * the presence/type of a category, and the number of string and decimal
 * attributes) by reading the first record in the file.
 * Reads ptr and returns 0 on success,
 *                      -1 on EOF,
 *                      -2 for other error.
 */
{
  char sbuf[MAX_SITE_LEN], *buf;
  char ebuf[128], nbuf[128];
  int err;
  int itmp;
  float ftmp;

  if (ftell (ptr) != 0L)
  {
    fprintf (stderr, "\nPROGRAMMER ERROR: G_oldsite_describe() must be called\n");
    fprintf (stderr, "        immediately after G_fopen_sites_old()\n");
    return -2;
  }

  *dims = *strs = *dbls = 0;
  *cat = -1;
  buf = sbuf;

  if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
  {
    rewind (ptr);
    return EOF;
  }
  /* skip over comment & header lines */	
  while ((*buf == '#' || !isdigit(*buf)) && *buf != '-' && *buf != '+')
    if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
    {
      rewind (ptr);
      return EOF;
    }

  if (buf[strlen (buf) - 1] == '\n')
    buf[strlen (buf) - 1] = '\0' ;

  if ((err = sscanf (buf, "%[^|]|%[^|]|%*[^\n]", ebuf, nbuf)) < 2)
  {
    fprintf (stderr, "ERROR: ebuf %s nbuf %s\n", ebuf, nbuf);
    rewind (ptr);
    return -2;
  }
  *dims = 2;

  /* move pointer past easting and northing fields */
  while (!ispipe(*buf) && !isnull(*buf))
    buf++;
  if (!isnull(*buf) && !isnull(*(buf + 1)))
    buf++;
  else
  {
    rewind (ptr);
    return -2;
  }
  while (!ispipe(*buf) && !isnull(*buf))
    buf++;
  if (!isnull(*buf) && !isnull(*(buf + 1)))
    buf++;
  else
  {
    rewind (ptr);
    return 0;
  }

  /* check for remaining dimensional fields */
  while (G_index (buf, PIPE) != (char *) NULL)
  {
    (*dims)++;
    while (!ispipe(*buf) && !isnull(*buf))
      buf++;
    if (isnull(*buf) || isnull(*(buf+1)))
    {
      rewind (ptr);
      return 0;
    }
    if (!isnull(*(buf + 1)))
      buf++;
    else
    {
      rewind (ptr);
      return -2;
    }
  }

  /* no more dimensions-now we parse attribute fields */
  while (!isnull(*buf))
  {
    switch (*buf)
    {
    case '#':			/* category field */
      sscanf(buf,"#%s ",ebuf);
      if(G_strstr(ebuf,".")==NULL && sscanf(ebuf,"%d", &itmp) ==1)
        *cat=CELL_TYPE;
      else if(G_strstr(ebuf,".")!=NULL && sscanf(ebuf,"%f", &ftmp) ==1)
        *cat=FCELL_TYPE;
      else 
        *cat=-1;

      /* move to beginning of next attribute */
      while (!isspace (*buf) && !isnull(*buf))
	buf++;
      if (isnull(*buf) || isnull(*(buf + 1)))
      {
	rewind (ptr);
	return 0;
      }
      else
	buf++;
      break;
    case '%':			/* decimal attribute */
      (*dbls)++;
      /* move to beginning of next attribute */
      while (!isspace (*buf) && !isnull(*buf))
	buf++;
      if (isnull(*buf) || isnull(*(buf + 1)))
      {
	rewind (ptr);
	return 0;
      }
      else
	buf++;
      break;
    case '@':			/* string attribute */
      if (isnull(*buf) || isnull(*(buf + 1)))
      {
	rewind (ptr);
	return 0;
      }
      else
	buf++;
    default:			/* defaults to string attribute */
      /* allow both prefixed and unprefixed strings */
      if ((err = cleanse_string (buf)) > 0)
      {
	(*strs)++;
	buf += err;
      }

      /* move to beginning of next attribute */
      while (!isspace (*buf) && !isnull(*buf))
	buf++;
      if (isnull(*buf) || isnull(*(buf + 1)))
      {
	rewind (ptr);
	return 0;
      }
      else
	buf++;
      break;
    }
  }

  rewind (ptr);
  return 0;
}

int G_site_in_region ( Site *site, struct Cell_head *region)
/* returns 1 if site is contained within region, 0 otherwise */
{
/* northwest corner is in region, southeast corner is not. */
double e_ing;
double G_adjust_easting();

  e_ing = G_adjust_easting (site->east, region);
  if (e_ing >= region->west &&
      e_ing < region->east &&
      site->north <= region->north &&
      site->north > region->south)
    return 1;

  return 0;
}

static int format_double ( double value, char *buf)
{
  sprintf (buf, "%.8f", value);
  G_trim_decimal (buf);
  return 0;
}

int cleanse_string (char *buf)
{
  char *stop, *p, *p2;

  p = buf;

  /*
   * get rid of any SPACEs at beginning while ( !isspace(*buf) && *buf !=
   * (char) NULL) buf++; if (*buf == (char) NULL) return -1;
   */

  /* find where this string terminates */
  if ( *buf != DQUOTE)	/* if no DQUOTEs, */
  {
    stop = G_index (buf, SPACE);/* then SPACE separates */
    if (stop == (char *) NULL)
      return strlen (buf);
    else
      return (int) (stop - buf);
  }
  else				/* otherwise string is in DQUOTEs */
  {				/* but we must skip over escaped */
    /* (BSLASHed) DQUOTEs */
    if (*p == DQUOTE)
    {
      while (*p != (char) NULL)	/* get rid of first DQUOTE */
      {
	*p = *(p + 1);
	p++;
      }
      p = buf;
      stop = G_index (p + 1, DQUOTE);
      while (*(stop - 1) == BSLASH)
	stop = G_index (++stop, DQUOTE);
    }
  }
  /* remove backslashes between buf and stop */
  p = buf;
  while ((p = G_index (p, BSLASH)) != (char *) NULL && p <= stop)
  {
    p2 = p + 1;
    if (*p2 != (char) NULL && (*p2 == DQUOTE || *p2 == BSLASH))
    {
      while (*p != (char) NULL)
      {
	*p = *(p + 1);
	p++;
      }
      stop--;
    }
    p = p2;
  }
  return (int) (stop - buf);
}

char *next_att (char *buf)
{
  while (!isspace (*buf) && !isnull(*buf))
    buf++;
  if (isnull(*buf) || isnull(*(buf + 1)))
    return NULL;
  else
    while (isspace (*(buf + 1)) && !isnull(*(buf + 1)))
      buf++;
  buf++;
  return buf;
}

int G_site_c_cmp (const void *a, const void *b)
/* qsort() comparison function for sorting an array of
   site structures by category. */
{
  int result = 0;               /* integer to be returned */
  double diff = 0;
 
  switch((*(Site **)a)->cattype)
  {
    case CELL_TYPE:
      diff = (double) (*(Site **) a)->ccat - (*(Site **) b)->ccat;
      break;
    case FCELL_TYPE:
      diff = (double) (*(Site **) a)->fcat - (*(Site **) b)->fcat;
      break;
    case DCELL_TYPE:
      diff = (double) (*(Site **) a)->dcat - (*(Site **) b)->dcat;
      break;
  }
  if (diff < 0.0 )
    result = -1;
  else if (diff > 0.0)
    result = 1;
  return result;
}

int G_site_d_cmp (const void *a, const void *b)
/* qsort() comparison function for sorting an array of
   site structures by first decimal attribute. */
{
  int result = 0;               /* integer to be returned */
  double diff;
 
  diff = (*(Site **) a)->dbl_att[0] - (*(Site **) b)->dbl_att[0];
  if (diff < 0.0 )
    result = -1;
  else if (diff > 0.0)
    result = 1;
  return result;
}

int G_oldsite_s_cmp (const void *a, const void *b)
/* qsort() comparison function for sorting an array of
   site structures by first decimal attribute. */
{
  return strcmp((*(char **)((*(Site **) a)->str_att)), 
                (*(char **)((*(Site **) b)->str_att)));
}

/*-************************************************************************
 *
 *  FILE *
 *  G_oldsites_open_old (name, mapset)
 *      opens the existing site list file 'name' in the 'mapset'
 *
 *  parms
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 **********************************************************************/

FILE * G_oldsites_open_old (char *name,char *mapset)
{
  return G_fopen_old ("site_lists", name, mapset);
}

FILE * G_oldsites_open_new (char *name)
{
  return G_fopen_new ("site_lists", name);
}

/*********************************************/
/* The following functions are obsolete.     */
/* They are retained here only for backwards */
/* compatability while porting applications  */
/*********************************************/

char *G_site_format (Site *s, char *fs, int id)
/* sprintf analog to G_site_put with the addition of a field seperator fs 
   and option of printing site attribute identifiers
 */
{
  char ebuf[MAX_SITE_STRING], nbuf[MAX_SITE_STRING];
  char xbuf[MAX_SITE_STRING];
  char *nfs, *buf;
  int fmt, i, j, k;

  buf=(char *)G_malloc(MAX_SITE_LEN*sizeof(char));

  fmt = G_projection ();

  G_format_northing (s->north, nbuf, fmt);
  G_format_easting (s->east, ebuf, fmt);

  nfs = (char *) ((fs == (char *) NULL) ? "|" : fs);

  sprintf (buf, "%s%s%s", ebuf, nfs, nbuf);

  for (i = 0; i < s->dim_alloc; ++i)
  {
    format_double (s->dim[i], nbuf);
    sprintf (xbuf, "%s%s", nfs, nbuf);
    G_strcat (buf, xbuf);
  }

  nfs= (fs == NULL) ? " " : fs;

  switch(s->cattype)
  {
    case CELL_TYPE:
    sprintf (xbuf, "%s%s%d ", nfs, ((id==0) ? "" : "#"), (int)s->ccat);
    G_strcat (buf, xbuf);
    break;
    case FCELL_TYPE:
    case DCELL_TYPE:
    sprintf (xbuf, "%s%s%g ", nfs, ((id==0) ? "" : "#"), (float)s->fcat);
    G_strcat (buf, xbuf);
    break;
  }

  for (i = 0; i < s->dbl_alloc; ++i)
  {
    format_double (s->dbl_att[i], nbuf);
    sprintf (xbuf, "%s%s%s", nfs, ((id==0) ? "" : "%"), nbuf);
    G_strcat (buf, xbuf);
  }

  for (i = 0; i < s->str_alloc; ++i)
  {
    if (strlen (s->str_att[i]) != 0)
    {
      /* escape double quotes */
      j = k = 0;

      /* do not uncomment this code because sites file was created
       * as we want. So it's enough to print them out as it is.
       *
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
      */

	G_strcpy (xbuf, s->str_att[i]);

      G_strcpy (s->str_att[i], xbuf);

      if (G_index (s->str_att[i], SPACE) != (char *) NULL)
	sprintf (xbuf, "%s%s\"%s\"", nfs, ((id==0) ? "" : "@"), s->str_att[i]);
      else
	sprintf (xbuf, "%s%s%s", nfs, ((id==0) ? "" : "@"), s->str_att[i]);
      G_strcat (buf, xbuf);
    }
  }
  return buf;
}

