#include "gis.h"
#include <unistd.h>
#include <ctype.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>  /* for sqrt() */

static struct table
{
    char *name;
    char *descr;
    double a;
    double e2;
    double f;
} *table = NULL;

static int count = -1;

static int same (const char *, const char *);
/* static int get_a_e2 (char *, char *, double *,double *); */
static int get_a_e2_f (const char*, const char *, double *, double *, double*);
static char *ellipsoid_table_file(char *);
static int compare_table_names(const struct table *, const struct table *);
static int read_ellipsoid_table(int );

/*
 * This routine returns the ellipsoid parameters from the database.
 * If the PROJECTION_FILE exists in the PERMANENT mapset, read info from
 * that file, otherwise return WGS 84 values.
 *
 * Returns: 1 ok, 0 default values used.
 * Dies with diagnostic if there is an error
 */
int 
G_get_ellipsoid_parameters (double *a, double *e2)
{
    int in_stat;
    char err[1024], ipath[1024], *str, *str1;
    struct Key_Value *proj_keys;
    static char *PERMANENT = "PERMANENT";


    G__file_name (ipath, "", PROJECTION_FILE, PERMANENT);
    if (access(ipath,0) !=0) 
    {
	*a = 6378137.0 ;
	*e2 = .006694385 ;
	return 0;
    }
    proj_keys = G_read_key_value_file(ipath, &in_stat); 
    if (in_stat !=0)
    {
	sprintf (err, "Unable to open file %s in %s",PROJECTION_FILE,PERMANENT);
	G_fatal_error (err);
    }
    if ((str = G_find_key_value("ellps",proj_keys))!=NULL) {
      if (strncmp(str,"sphere",6)==0) { 
        str = G_find_key_value("a",proj_keys); 
        if (str!=NULL)  {
          if(sscanf(str,"%lf",a)!=1) {
	    sprintf (err, "invalid a: field %s in file %s in %s"
                              ,str,PROJECTION_FILE,PERMANENT);
	    G_fatal_error (err);
          }
        }
        else {
	  *a = 6370997.0 ;
        }
	*e2 = 0.0 ;
	return 0;
      }
      else {
        if (G_get_ellipsoid_by_name (str, a, e2)==0) {
	  sprintf (err, "invalid ellipsoid %s in file %s in %s"
                              ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
        else return 1;
      }
    }
    else {
      str = G_find_key_value("a",proj_keys); 
      str1 = G_find_key_value("es",proj_keys); 
      if ((str!=NULL) && (str1!=NULL)) {
        if(sscanf(str,"%lf",a)!=1) {
	  sprintf (err, "invalid a: field %s in file %s in %s"
                            ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
        if(sscanf(str1,"%lf",e2)!=1) {
	  sprintf (err, "invalid es: field %s in file %s in %s"
                            ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
        return 1;
      }
      else { 
        str = G_find_key_value("proj",proj_keys); 
        if ((str==NULL)||(strcmp(str,"ll")==0)) { 
  	  *a = 6378137.0 ;
	  *e2 = .006694385 ;
	  return 0;
        }
        else {
 	  sprintf (err, "No ellipsoid info given in file %s in %s",
                                        PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
      }
    }
    return 1;
    /* whats that? al 05/2000 */
    return 0;
}

/*
 * looks up ellipsoid in ellipsoid table and returns the
 * a, e2 parameters for the ellipsoid
 *
 * returns 1 if ok,
 *         0 if not found in table
 */

int 
G_get_ellipsoid_by_name (const char *name, double *a, double *e2)
{
    int i;

    (void) read_ellipsoid_table(0);

    for (i = 0; i < count; i++)
    {
	if (same(name, table[i].name))
	{
	    *a = table[i].a;
	    *e2 = table[i].e2;
	    return 1;
	}
    }
    return 0;
}

/*
 * returns name(s) of ellipsoid
 *
 * for (i = 0; name = G_ellipsoid_name(i); i++)
 *         ....
 */
char *
G_ellipsoid_name (int n)
{
    (void) read_ellipsoid_table(0);
    return n>=0 && n < count ? table[n].name : NULL;
}

/*
 * new 05/2000 by al: for datum shift the f parameter is needed too.
 * this all is not a clean design, but it keeps backward-
 * compatibility. 
 * looks up ellipsoid in ellipsoid table and returns the
 * a, e2 and f parameters for the ellipsoid
 * 
 * returns 1 if ok,
 *         0 if not found in table 
 */
int 
G_get_spheroid_by_name(const char *name, double *a, double *e2, double *f)
{
    int i;

    (void) read_ellipsoid_table(0);

    for (i = 0; i < count; i++)
    {
	if (same(name, table[i].name))
	{
	    *a = table[i].a;
	    *e2 = table[i].e2;
	    *f = table[i].f;
	    return 1;
	}
    }
    return 0;
}

char *
G_ellipsoid_description(int n)
{
    (void) read_ellipsoid_table(0);
    return n>=0 && n < count ? table[n].descr : NULL;
}

static int 
same (const char *a, const char *b)
{
    while (*a && *b)
	if (tolower(*a++) != tolower(*b++))
	    return 0;
    
    return (*a == 0 && *b == 0);
}

static int 
get_a_e2_f (const char *s1, const char *s2, double *a, double *e2, double *f)
{
    double b, recipf;

    if (sscanf (s1, "a=%lf", a) != 1)
	return 0;
    
    if (*a <= 0.0) 
        return 0;

    if (sscanf (s2, "e=%lf", e2) == 1) 
    {
        *f = (double)1.0 / - sqrt( ((double)1.0 - *e2) ) + (double)1.0;
        return (*e2 > 0.0);
    }

    if (sscanf (s2, "f=1/%lf", f) == 1)
    {
	if (*f <= 0.0) 
	    return 0;
	recipf = (double)1.0/(*f);
	*e2 = recipf + recipf - recipf * recipf;
	return (*e2 > 0.0);
    }

    if (sscanf (s2, "b=%lf", &b) == 1)
    {
	if (b <= 0.0) return 0;
	if (b == *a) {
	  *f = 0.0;
	  *e2 = 0.0;
	} else {
	  recipf = ((*a) - b) / (*a);
	  *f = (double)1.0 / recipf;
	  *e2 = recipf + recipf - recipf * recipf;
	}
	return (*e2 > 0.0);
    }
    return 0;
}

static char *
ellipsoid_table_file(char *file)
{
    sprintf (file, "%s/etc/ellipse.table", G_gisbase());
    return file;
}

static int 
compare_table_names(const struct table *a, const struct table *b)
{
  /* return strcmp(a->name,b->name); */
  return G_strcasecmp(a->name, b->name); 
}

static int 
read_ellipsoid_table(int fatal)
{
    FILE *fd;
    char file[1024];
    char buf[1024];
    char name[100], descr[100], buf1[100], buf2[100];
    char badlines[256];
    int line;
    int err;

    if (count >= 0)
	return 1;
    count = 0;
    table = NULL;

    (void) ellipsoid_table_file (file);
    fd = fopen (file, "r");
    if (fd == NULL)
    {
	perror (file);
	sprintf (buf, "unable to open ellipsoid table file: %s", file);
	fatal ? G_fatal_error(buf) : G_warning (buf);
	return 0;
    }

    err = 0;
    *badlines = 0;
    for (line = 1; G_getl (buf, sizeof buf, fd); line++)
    {
	G_strip (buf);
	if (*buf == 0 || *buf == '#')
	    continue;

	if (sscanf (buf, "%s  \"%32[^\"]\" %s %s", name, descr, buf1, buf2) != 4)
	{
	    err++;
	    sprintf (buf, " %d", line);
	    if (*badlines) 
	      G_strcat(badlines, ",");
	    G_strcat(badlines, buf);
	    continue;
	}

	table = (struct table *) G_realloc ((char *) table, (count+1) * sizeof(*table));
	table[count].name = G_store (name);
	table[count].descr = G_store (descr);

	if(get_a_e2_f (buf1, buf2, &table[count].a, &table[count].e2, &table[count].f)
	|| get_a_e2_f (buf2, buf1, &table[count].a, &table[count].e2, &table[count].f))
	    count++;
	else
	{
	    err++;
	    sprintf (buf, " %d", line);
	    if (*badlines) 
	      G_strcat (badlines, ",");
	    G_strcat (badlines, buf);
	    continue;
	}
    }
    if (!err)
    {
    	/* over correct typed version */
	qsort ((void *)table, (size_t)count, (size_t)sizeof(*table), (int (*)(const void*, const void *))(compare_table_names));
	return 1;
    }
    sprintf (buf, "Line%s%s of ellipsoid table file <%s> %s invalid",
	err==1?"":"s", badlines, file, err==1?"is":"are");
    fatal ? G_fatal_error(buf) : G_warning (buf);
    return 0;
}
