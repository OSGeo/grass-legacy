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

/*-
 * $Log$
 * Revision 1.8  2001-01-12 08:16:22  justin
 * Added site.h since it was removed from gis.h
 *
 * Revision 1.7  2000/10/15 04:02:59  eric
 * Undo breakage I introduced.
 *
 * Revision 1.6  2000/10/14 01:36:54  eric
 * Fix bug I introduced to sites.c; Fix the real bug in s.to.rast --
 * was creating a site struct with the *wrong* cattype.
 *
 * Revision 1.5  2000/10/13 00:54:52  eric
 * Small modifications to G__site_get() to fix missing the first attribute
 * when no cat exists and returning a spurious error code when everything
 * was okay.  s.to.rast should now work fine with sites like
 * east|north|%double
 *
 * Revision 1.4  2000/06/30 11:33:12  markus
 * Bill Brown: fix to parse multiple strings in site_list properly
 *
 * Revision 1.3  1999/12/30 10:24:19  markus
 * G__site_get, G__site_put: added return statements
 *
 * Revision 1.2  1999/12/30 10:20:40  markus
 * added several G_free statements
 *
 * Revision 1.1.1.1  1999/12/29 15:10:31  markus
 * initial CVS import
 *
 * Revision 1.41  1999/10/18  brown
 * Created G__site_put as G_site_put with format option and changed
 * G_site_put into a wrapper (for s.proj).
 *
 * Revision 1.40a  1997/05/07  brown
 * changed G_site_get to return 1 on "format mismatch (extra data)" 
 * instead of error
 *
 * Revision 1.40  1996/05/23  brown
 * changed DateTime stuff to use TimeStamp instead
 *
 * Revision 1.39  1995/08/17  21:03:29  mccauley
 * fixed bug related to time to stime conversion
 *
 * Revision 1.38  1995/07/25  15:14:45  mccauley
 * fixed error for cat in G_site_describe
 *
 * Revision 1.37  1995/07/17  20:55:48  mccauley
 * change G_strchr to G_strstr
 *
 * Revision 1.36  1995/07/17  11:14:43  mccauley
 * took out has_cat and made part of struct
 * support for floating point categories
 *
 * Revision 1.35  1995/07/14  14:04:14  mccauley
 * renamed G_site_get_fmt to G_site_describe
 *
 * Revision 1.34  1995/07/14  11:39:49  mccauley
 * changed G_site_destroy_struct to G_site_free_struct
 *
 * Revision 1.33  1995/07/13  19:58:28  mccauley
 * added G_site_destroy_struct
 *
 * Revision 1.32  1995/07/05  12:55:40  mccauley
 * added warning flags for old API usage
 *
 * Revision 1.31  1995/07/05  12:25:48  mccauley
 * modified G_site_put to use (Site *) instead of (Site)
 *
 * Revision 1.30  1995/07/05  12:07:17  mccauley
 * removed extra spaces in G_site_puts output
 *
 * Revision 1.29  1995/07/05  12:02:48  mccauley
 * added G_site_puts (analog to sprintf for G_site_put)
 *
 * Revision 1.28  1995/07/05  12:01:20  mccauley
 * fixed bug in G_sites_get_fmt
 *
 * Revision 1.27  1995/06/29  16:27:00  mccauley
 * fixed malloc error in G_site_new_struct.
 *
 * Revision 1.26  1995/06/29  12:08:25  mccauley
 * added G_site_in_region.
 *
 * Revision 1.25  1995/06/20  16:08:24  mccauley
 * first version put in 4.2 release
 *
 * Revision 1.24  1995/05/24  00:36:27  mccauley
 * added old functions for bc while porting
 *
 * Revision 1.23  1995/05/24  00:05:31  mccauley
 * added DateTime stuff
 *
 * Revision 1.22  1995/04/17  22:56:59  mccauley
 * added G_site_put_hdr/G_site_get_hdr. Header records include
 * "name|", "desc|", "labels|", and "time|".
 *
 * Revision 1.21  1995/02/22  03:23:19  mccauley
 * more ctype-like changes.
 *
 * Revision 1.20  1995/02/22  03:14:23  mccauley
 * changed conditionals to use ctype-like defines (e.g., ispipe).
 *
 * Revision 1.19  1995/02/22  02:53:48  mccauley
 * skip over header lines in G_site_get and G_site_get_fmt.
 *
 * Revision 1.18  1995/02/22  02:45:54  mccauley
 * added sites functions from 4.1 that we'll keep.
 *
 * Revision 1.17  1995/02/22  02:25:03  mccauley
 * changed names of functions to G_site_xxx().
 *
 * Revision 1.16  1995/02/21  07:53:48  mccauley
 * changed "static char sites_api_rcsid" to pragma ident.
 * added function declarations for those int functions that
 * should be defined in gis.h. fixed pointer-int comparison
 * in next_att().
 * No warnings (from this code) for "gcc -Wall."
 *
 * Revision 1.15  1995/02/21  07:27:32  mccauley
 * added qsort comparison functions.
 * still need to re-write G_guess_site_fmt to reflect changes in
 * G_new_get_site.
 *
 * Revision 1.14  1995/02/21  07:21:49  mccauley
 * replaced repititive code with next_att()
 * removed erroneous comments. ran indent.
 *
 * Revision 1.13  1995/02/21  02:33:14  mccauley
 * changes to ignore superflous spaces not contained within quotes.
 * changes to use ctyle functions and to do selective reads.
 *
 * Revision 1.12  1995/02/18  22:34:37  mccauley
 * a few changes to read selective fields (according to what
 * was allocated for).
 *
 * Revision 1.11  1995/02/09  02:49:20  mccauley
 * Added check for dim_alloc in G_new_get_site.
 * Removed erroneous check in G_new_put_site.
 * Still have error in selective reads.
 *
 * Revision 1.10  1995/02/09  00:07:03  mccauley
 * Debugged G_guess_site_fmt and ran indent.
 * Still need to rewrite G_guess_site_fmt and G_new_get_site so
 * that we don't have multiple exits points (too many 'return 0's).
 *
 * Revision 1.9  1995/02/08  23:12:20  mccauley
 * Added first version of G_guess_site_fmt (untested).
 *
 * Revision 1.8  1995/02/08  01:26:27  mccauley
 * Changed return vals of G_new_get_site for easier diagnostics.
 * For category and decimal attributes, on possible reason for a
 * non zero return value is the value does not immediately follow
 * the attribute tag (# or %). E.g., "# 2" or "% 23.4" will cause
 * an error. Remember to put this in programmers' docs!
 *
 * Revision 1.7  1995/02/08  00:54:49  mccauley
 * Added a few more checks. Fixed a bug in cleanse_string.
 *
 * Revision 1.6  1995/02/08  00:06:24  mccauley
 * Added a few checks, combined a few, and changed do/while to while after #.
 *
 * Revision 1.5  1995/02/07  23:33:07  mccauley
 * fixed bug with G_new_put_site [.dim_alloc already decremented by 2]
 *
 * Revision 1.4  1995/02/07  23:24:06  mccauley
 * added comments and ran 'indent'
 *
 * Revision 1.3  1995/02/07  23:16:04  mccauley
 * if length of string attribute is zero, it is not printed. if there
 * are no non-zero string attributes, no string attributes are printed.
 *
 * Revision 1.2  1995/02/07  21:15:13  mccauley
 * recompiled with 'gcc -Wall' and got rid of "unused var" warnings.
 * added return values to some functions.
 *
 * Revision 1.1  1995/02/07  21:01:12  mccauley Initial revision
 */

#include <ctype.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "site.h"

#define DQUOTE '"'
#define SPACE ' '
#define BSLASH 92
#define PIPE '|'

#define ispipe(c) (c==PIPE)
#define isnull(c) (c==(char)NULL)
#define isquote(c) (c==DQUOTE)

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
    G_fatal_error ("G_site_new_struct: invalid # dims or fields\n");

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

int G_site_get ( FILE *fptr, Site *s)
/* Writes a site to file open on fptr. */
{
    return G__site_get (fptr, s, G_projection() );
}


int G__site_get ( FILE *ptr, Site *s, int fmt)
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
	s->dbl_att[d++] = strtod(buf, &p1);
	if (p1 == buf) {
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

int G_site_put ( FILE *fptr, Site *s)
/* Writes a site to file open on fptr. */
{
    return G__site_put (fptr, s, G_projection() );
}


int G__site_put ( FILE *fptr, Site *s, int fmt)
/* Writes a site to file open on fptr. */
{
  char ebuf[MAX_SITE_STRING], nbuf[MAX_SITE_STRING];
  char xbuf[MAX_SITE_STRING], buf[MAX_SITE_LEN];
  int i, j, k;


  G_format_northing (s->north, nbuf, fmt);
  G_format_easting (s->east, ebuf, fmt);
  sprintf (buf, "%s|%s|", ebuf, nbuf);
  for (i = 0; i < s->dim_alloc; ++i)
  {
    format_double (s->dim[i], nbuf);
    sprintf (xbuf, "%s|", nbuf);
    G_strcat (buf, xbuf);
  }
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


int G_site_describe ( FILE *ptr,
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
    fprintf (stderr, "\nPROGRAMMER ERROR: G_site_describe() must be called\n");
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
    buf[strlen (buf) - 1] = (char) NULL;

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
  while (G_index (buf, PIPE) != (char) NULL)
  {
    (*dims)++;
    while (!ispipe(*buf) && !isnull(*buf))
      buf++;
    if (isnull(*buf))
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

int G_site_put_head ( FILE *ptr, Site_head *head)
/*-
 * Writes site_head struct.
 */
{
  static char buf[128];

  if (head->name!=NULL)
    fprintf(ptr,"name|%s\n",head->name);
  if (head->desc!=NULL)
    fprintf(ptr,"desc|%s\n",head->desc);
  if (head->form!=NULL)
    fprintf(ptr,"form|%s\n",head->form);
  if (head->labels!=NULL)
    fprintf(ptr,"labels|%s\n",head->labels);
  /* time could be in (char *) stime, (struct TimeStamp *) time, 
     both, or neither */
  if (head->stime!=NULL || head->time !=NULL)
  {
    if (head->time != NULL)   /* TimeStamp struct has precendence */
    {
      G_format_timestamp (head->time, buf);
      fprintf(ptr,"time|%s\n",buf);
    }
    else if (head->stime != NULL)  /* next check string */
    {
      if (head->time==NULL) {
         if ((head->time=(struct TimeStamp *) 
                         G_malloc(sizeof(struct TimeStamp)))==NULL)
	     G_fatal_error("Memory error in writing timestamp");
	 else 
	 if (G_scan_timestamp (head->time, head->stime) < 0)
	 {         
	     G_warning("Illegal TimeStamp string");
	     return -1; /* added to prevent crash 5/2000 MN*/
	 }
      }

      G_format_timestamp (head->time, head->stime);
      fprintf(ptr,"time|%s\n",head->stime);
    }
  }
  return 0;
}

int G_site_get_head (FILE *ptr, Site_head *head)
/*-
 * Fills in site_head struct.
 */
{
  char sbuf[MAX_SITE_LEN], *buf;
  int len;

  if (ftell (ptr) != 0L)
  {
    fprintf (stderr, "\nPROGRAMMER ERROR: G_site_get_head() must be called\n");
    fprintf (stderr, "        immediately after G_fopen_sites_old()\n");
    return -2;
  }

  head->name=NULL;
  head->desc=NULL;
  head->form=NULL;
  head->labels=NULL;
  head->stime=NULL;
  head->time=NULL;

  buf = sbuf;

  if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
  {
    rewind (ptr);
    return EOF;
  }
  /* assume header lines are always first records. allow for comments */
  while ((*buf == '#' || !isdigit(*buf)) && *buf != '-' && *buf != '+')
  {
    len=strlen(buf);
    if (buf[len-1]=='\n')
         buf[len-1]='\0';
    if (!strncmp(buf,"name|",5))
    {
       buf+=5;
       head->name=G_strdup(buf);
    }
    else if (!strncmp(buf,"desc|",5))
    {
       buf+=5;
       head->desc=G_strdup(buf);
    }
    else if (!strncmp(buf,"form|",5))
    {
       buf+=5;
       head->form=G_strdup(buf);
    }
    else if (!strncmp(buf,"labels|",7))
    {
       buf+=7;
       head->labels=G_strdup(buf);
    }
    else if (!strncmp(buf,"time|",5))
    {
       buf+=5;
       head->stime=G_strdup(buf);
       if ((head->time=(struct TimeStamp *) 
                       G_malloc(sizeof(struct TimeStamp)))==NULL)
         G_fatal_error("Memory error in allocating timestamp");
       if (G_scan_timestamp (head->time , head->stime)<0)
       {
         G_warning(datetime_error_msg());
         head->time=NULL;
         head->stime=NULL;
       }
    }
    /* else is it a comment or some invalid header (not a decimal digit) */

    if ((buf = fgets (sbuf, 1024, ptr)) == (char *) NULL)
    {
      rewind (ptr);
      return EOF;
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
  char *stop, *p;

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
  while ((p = G_index (buf, BSLASH)) != (char *) NULL && p <= stop)
  {
    if (*(p + 1) != (char) NULL && *(p + 1) == DQUOTE)
    {
      while (*p != (char) NULL)
      {
	*p = *(p + 1);
	p++;
      }
      stop--;
    }
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

int G_site_c_cmp (void *a,void *b)
/* qsort() comparison function for sorting an array of
   site structures by category. */
{
  int result = 0;               /* integer to be returned */
  double diff;
 
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

int G_site_d_cmp (void *a,void *b)
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

int G_site_s_cmp (void *a,void *b)
/* qsort() comparison function for sorting an array of
   site structures by first decimal attribute. */
{
  return strcmp((*(char **)((*(Site **) a)->str_att)), 
                (*(char **)((*(Site **) b)->str_att)));
}

/*-************************************************************************
 *   char *
 *   G_ask_sites_new(prompt, name))
 *       asks user to input name of a new site list file
 *
 *   char *
 *   G_ask_sites_old(prompt, name)
 *       asks user to input name of an existing site list file
 *
 *   char *
 *   G_ask_sites_any(prompt, name)
 *       asks user to input any site list name
 *
 *   char *
 *   G_ask_sites_in_mapset(prompt, name)
 *       asks user to input name of an existing site list file in
 *       current mapset
 *
 *   parms:
 *      char *prompt    optional prompt for user
 *      char *name      buffer to hold name of map found
 *
 *   returns:
 *      char *pointer to a string with name of mapset
 *       where file was found, or NULL if not found
 *
 *   note:
 *      rejects all names that begin with .
 **********************************************************************
 *
 *  FILE *
 *  G_sites_open_old (name, mapset)
 *      opens the existing site list file 'name' in the 'mapset'
 *
 *  FILE *
 *  G_sites_open_new (name)
 *      opens a new site list file 'name' in the current mapset
 *
 *  parms
 *      char *name            map file name
 *      char *mapset          mapset containing map "name"
 *
 **********************************************************************/

char * G_find_sites (char *name,char *mapset)
{
  return G_find_file ("site_lists", name, mapset);
}

char * G_find_sites2 (char *name,char *mapset)
{
  return G_find_file2 ("site_lists", name, mapset);
}

char * G_ask_sites_new (char *prompt,char *name)
{
  return G_ask_new (prompt, name, "site_lists", "site list");
}

char * G_ask_sites_old (char *prompt,char *name)
{
  return G_ask_old (prompt, name, "site_lists", "site list");
}

char * G_ask_sites_any (char *prompt,char *name)
{
  return G_ask_any (prompt, name, "site_lists", "site list", 1);
}

char * G_ask_sites_in_mapset (char *prompt,char *name)
{
  return G_ask_in_mapset (prompt, name, "site_lists", "site list");
}

FILE * G_sites_open_old (char *name,char *mapset)
{
  return G_fopen_old ("site_lists", name, mapset);
}

FILE * G_sites_open_new (char *name)
{
  return G_fopen_new ("site_lists", name);
}

/*********************************************/
/* The following functions are obsolete.     */
/* They are retained here only for backwards */
/* compatability while porting applications  */
/*********************************************/
FILE *G_fopen_sites_old (char *name, char *mapset)
{
    return G_fopen_old ("site_lists", name, mapset);
}

FILE *G_fopen_sites_new (char *name)
{
    return G_fopen_new ("site_lists", name);
}

int G_get_site ( FILE *fd, double *east,double *north, char **desc)
{
    char buf[400];
    char temp[400];
    char ebuf[128], nbuf[128];
    static char *desc_ptr = NULL;

    G_warning("WARNING: %s needs modified for the new Sites API\n",
            G_program_name() );

    if (desc_ptr != NULL)
    {
	free (desc_ptr);
	desc_ptr = NULL;
    }
    *temp = 0;
    while (fgets (buf, sizeof buf, fd))
    {
	if (sscanf (buf, "point|%[^|]|%[^|]|%[^\n]", ebuf, nbuf, temp) >= 2
	||  sscanf (buf, "%[^|]|%[^|]|%[^\n]", ebuf, nbuf, temp) >= 2)
	{
	    if (G_scan_northing (nbuf, north, G_projection())
	    &&  G_scan_easting (ebuf, east, G_projection()))
	    {
		G_strip (temp);
		*desc = desc_ptr = G_store (temp);
		return 1;
	    }
	}
    }
    return -1;
}

int G_put_site ( FILE *fd, double east,double north, char *desc)
{
    char ebuf[128], nbuf[128];
    int fmt;
    G_warning("WARNING: %s needs modified for the new Sites API\n",
            G_program_name() );

/*  fmt = G_projection(); */ 
    fmt = -1;

    G_format_northing (north, nbuf, fmt);
    G_format_easting  (east,  ebuf, fmt);
    fprintf (fd, "%s|%s|%s\n", ebuf, nbuf, desc?desc:"");

    return 0;
}

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
	sprintf (xbuf, "%s%s\"%s\"", nfs, ((id==0) ? "" : "@"), s->str_att[i]);
      else
	sprintf (xbuf, "%s%s%s", nfs, ((id==0) ? "" : "@"), s->str_att[i]);
      G_strcat (buf, xbuf);
    }
  }
  return buf;
}



