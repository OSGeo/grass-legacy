/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       coorcnv library
 * AUTHOR(S):    Andreas Lange - andreas.lange@rhein-main.de
 * PURPOSE: 	 provide functions for reading datum parameters from the
 *               location database.     
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

#include <gis.h>
#include "CC.h"
#include <unistd.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "local_proto.h"

static int count = -1;


int 
CC_get_datum_by_name(const char *name)
{
  int i;

  (void) read_datum_table(0);
  for (i = 0; i < count; i++)
    {
      if (same(name, table[i].name))
	return i;
    }
  return -1;
}

char *
CC_get_datum_by_nbr(int n) 
{
  (void) read_datum_table(0);

  if (n < 0 || n >= count) 
    return NULL; 
  return table[n].name;
}

/* this sets the datum shift parameters for datum pointed to by name */
int 
CC_datum_shift (const char *name, double *dx, double *dy, double *dz)
{
  int i;
  
  (void) read_datum_table(0);
  for (i = 0; i < count; i++)
    {
      if (same(name, table[i].name))
	{
	  *dx = table[i].dx;
	  *dy = table[i].dy;
	  *dz = table[i].dz;
	  return 1;
	}
    }
  return 0;
}

/* set the ellipsoid name and parameters for datum */
int 
CC_get_datum_parameters (const char *name, char *ellps, double *dx, double *dy, double *dz)
{
  int i;
  
  (void) read_datum_table(0);

  for (i = 0; i < count; i++)
    {
      if (same(name, table[i].name))
	{
	  G_strcpy(ellps, table[i].ellps);
	  *dx = table[i].dx;
	  *dy = table[i].dy;
	  *dz = table[i].dz;
	  return 1;
	}
    }
  return 0;
}

char *
CC_datum_name (int n)
{

  (void) read_datum_table(0);

  if (n < 0 || n >= count) 
    return NULL; 
  return table[n].name;
}

char *
CC_datum_description (int n)
{ 

  (void) read_datum_table(0);
  
  if (n < 0 || n >= count)
    return NULL;
    /* was: return 0; */
  return table[n].descr;
}

char *
CC_datum_ellipsoid (int n)
{

  (void) read_datum_table(0);

  if (n < 0 || n >= count)
    return NULL;
  /* was: return 0; */
  return table[n].ellps;
}

static int 
read_datum_table(int fatal) 
{
  FILE *fd;
  char file[1024];
  char buf[1024];
  char name[100], ellps[100], descr[100], buf1[100], buf2[100], buf3[100];
  char badlines[256];
  int line;
  int err;

  if (count >= 0)
     return 1;
  count = 0;
  table = NULL;
  (void) datum_table_file(file);
  fd = fopen (file, "r");
  if (fd == NULL)
    {
      perror (file);
      sprintf(buf, "unable to open datum table file: %s", file);
      fatal ? G_fatal_error(buf) : G_warning(buf);
      return 0;
    }

  err = 0;
  *badlines = '\0';
  for (line = 1; G_getl(buf, (size_t)sizeof(buf), fd); line++)
    {
      G_strip(buf);
      if (*buf == '\0' || *buf == '#') {
	continue;
      }
      
      if (sscanf(buf, "%s \"%32[^\"]\" %s %s %s %s", name, descr, ellps, buf1, buf2, buf3) != 6) {
	  err++;
	  sprintf(buf, " %d", line);
	  if (*badlines)
	    G_strcat(badlines, ",");
	  G_strcat(badlines, buf);
	  continue;
      }

      table = (struct table *) G_realloc ((char *) table, ((count+1) * sizeof(*table)));
      if (table == NULL)
	{
	  sprintf(buf, "unable to reallocate memory");
	  fatal ? G_fatal_error(buf) : G_warning(buf);
	  return 0;
	}

      table[count].name  = G_store (name);
      table[count].descr = G_store (descr);
      table[count].ellps = G_store (ellps);
      
      if ( get_dx_dy_dz(buf1, buf2, buf3, &table[count].dx, &table[count].dy, &table[count].dz) == 1 ) {
	count++;
      } else {
	err++;
	sprintf(buf, " %d", line);
	if (*badlines) 
	  G_strcat(badlines, ",");
	G_strcat(badlines, buf);
	continue;
      }
    }
  if (!err)
    { 
      /* now overly correct typed */
      qsort((void *)table, (size_t)count, (size_t)sizeof(*table), (int (*)(const void*, const void*))(compare_table_names));
      return 1;
    }
  sprintf(buf, "Line%s%s of datum table file <%s> %s invalid",
	  err==1?"":"s", badlines, file, err==1?"is":"are");
  fatal ? G_fatal_error(buf) : G_warning (buf);
  return 0;
}

static int
get_dx_dy_dz (const char *s1, const char *s2, const char *s3, double *dx, double *dy, double *dz)
{
  if (sscanf(s1, "dx=%lf", dx) != 1) {
    return 0;
  }
  if (sscanf(s2, "dy=%lf", dy) != 1) {
    return 0;
  }
  if (sscanf(s3, "dz=%lf", dz) != 1) {
    return 0;
  }
  return 1;
}

static char *
datum_table_file(char *file)
{
  sprintf(file, "%s/etc/datum.table", G_gisbase());
  return file;
}

static int
compare_table_names(const struct table *a, const struct table *b)
{
  /* return strcmp(a->name,b->name); */
  return G_strcasecmp(a->name, b->name);
}

static int 
same(const char *a, const char *b)
{
  while (*a && *b)
    if (tolower(*a++) != tolower(*b++))
      return 0;
  return (*a == 0 && *b == 0);
}
