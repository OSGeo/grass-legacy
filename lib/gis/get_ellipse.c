#include <grass/gis.h>
#include <grass/glocale.h>
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

/* static int get_a_e2 (char *, char *, double *,double *); */
static int get_a_e2_f (const char*, const char *, double *, double *, double*);
void ellipsoid_table_file(char *);
static int compare_table_names(const void *, const void *);
static int read_ellipsoid_table(int );

/*
 * This routine returns the ellipsoid parameters from the database.
 * If the PROJECTION_FILE exists in the PERMANENT mapset, read info from
 * that file, otherwise return WGS 84 values.
 *
 * Returns: 1 ok, 0 default values used.
 * Dies with diagnostic if there is an error
 */

/*!
 * \brief get ellipsoid parameters
 *
 * This routine returns the semi-major axis <b>a</b> (in meters)
 * and the eccentricity squared <b>e2</b> for the ellipsoid associated with the
 * database. If there is no ellipsoid explicitly associated with the database, it
 * returns the values for the WGS 84 ellipsoid.
 *
 *  \param a
 *  \param e2
 *  \return int
 */

 
/*!
 * \brief get ellipsoid parameters
 *
 * This routine returns the semi-major axis <b>a</b> (in meters)
 * and the eccentricity squared <b>e2</b> for the ellipsoid associated with the
 * database. If there is no ellipsoid explicitly associated with the database, it
 * returns the values for the WGS 84 ellipsoid.
 *
 *  \param a
 *  \param e2
 *  \return int
 */

int 
G_get_ellipsoid_parameters (double *a, double *e2)
{
    int in_stat;
    char err[1024], ipath[1024], *str, *str1;
    struct Key_Value *proj_keys;
    static char *PERMANENT = "PERMANENT";

    proj_keys = NULL;

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
	sprintf (err, _("Unable to open file %s in %s"),PROJECTION_FILE,PERMANENT);
	G_fatal_error (err);
    }
    if ((str = G_find_key_value("ellps",proj_keys))!=NULL) {
      if (strncmp(str,"sphere",6)==0) { 
        str = G_find_key_value("a",proj_keys); 
        if (str!=NULL)  {
          if(sscanf(str,"%lf",a)!=1) {
	    sprintf (err, _("invalid a: field %s in file %s in %s")
                              ,str,PROJECTION_FILE,PERMANENT);
	    G_fatal_error (err);
          }
        }
        else {
	  *a = 6370997.0 ;
        }
	*e2 = 0.0 ;
	G_free_key_value(proj_keys);
	return 0;
      }
      else {
        if (G_get_ellipsoid_by_name (str, a, e2)==0) {
	  sprintf (err, _("invalid ellipsoid %s in file %s in %s")
                              ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
        else {
	    G_free_key_value(proj_keys);
	    return 1;
	}
      }
    }
    else {
      str = G_find_key_value("a",proj_keys); 
      str1 = G_find_key_value("es",proj_keys); 
      if ((str!=NULL) && (str1!=NULL)) {
        if(sscanf(str,"%lf",a)!=1) {
	  sprintf (err, _("invalid a: field %s in file %s in %s")
                            ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
        if(sscanf(str1,"%lf",e2)!=1) {
	  sprintf (err, _("invalid es: field %s in file %s in %s")
                            ,str,PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
	G_free_key_value(proj_keys);
        return 1;
      }
      else { 
        str = G_find_key_value("proj",proj_keys); 
        if ((str==NULL)||(strcmp(str,"ll")==0)) { 
  	  *a = 6378137.0 ;
	  *e2 = .006694385 ;
	  G_free_key_value(proj_keys);
	  return 0;
        }
        else {
 	  sprintf (err, _("No ellipsoid info given in file %s in %s"),
                                        PROJECTION_FILE,PERMANENT);
	  G_fatal_error (err);
        }
      }
    }

    G_free_key_value(proj_keys);

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


/*!
 * \brief get ellipsoid by name
 *
 * This routine returns the semi-major axis <b>a</b> (in
 * meters) and eccentricity squared <b>e2</b> for the named ellipsoid.  Returns
 * 1 if <b>name</b> is a known ellipsoid, 0 otherwise.
 *
 *  \param name
 *  \param a
 *  \param e2
 *  \return int
 */

 
/*!
 * \brief get ellipsoid parameters by name
 *
 * This routine returns the semi-major axis <b>a</b> (in
 * meters) and eccentricity squared <b>e2</b> for the named ellipsoid.  Returns
 * 1 if <b>name</b> is a known ellipsoid, 0 otherwise.
 *
 *  \param name
 *  \param a
 *  \param e2
 *  \return int
 */

int 
G_get_ellipsoid_by_name (const char *name, double *a, double *e2)
{
    int i;

    (void) read_ellipsoid_table(0);

    for (i = 0; i < count; i++)
    {
	if (G_strcasecmp(name, table[i].name) == 0)
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

/*!
 * \brief return ellopsoid name
 *
 * This routine
 * returns a pointer to a string containg the name for the <b>n</b><i>th</i>
 * ellipsoid in the GRASS ellipsoid table; NULL when <b>n</b> is too large. It
 * can be used as follows:
 \code
  int n ; 
  char *name ; 
  for ( n=0 ; name=G_ellipsoid_name(n) ; n++ ) 
     fprintf(stdout, "%s\n", name);
 \endcode
 *
 *  \param n
 *  \return char * 
 */

 
/*!
 * \brief return ellopsoid name
 *
 * This routine
 * returns a pointer to a string containg the name for the <b>n</b><i>th</i>
 * ellipsoid in the GRASS ellipsoid table; NULL when <b>n</b> is below zero or
 * too large. It can be used as follows:
 \code
  int n ; 
  char *name ; 
  for ( n=0 ; name=G_ellipsoid_name(n) ; n++ ) 
   fprintf(stdout, "%s\n", name);
 \endcode
 *
 *  \param n
 *  \return char * 
 */

 
/*!
 * \brief get ellipsoid name
 *
 * This function
 * returns a pointer to the short name for the <b>n</b><i>th</i> ellipsoid.
 * If <b>n</b> is less than 0 or greater than the number of known ellipsoids,
 * it returns a NULL pointer. 
 *
 *  \param n
 *  \return char * 
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

/*!
 * \brief get spheroid parameters by name
 *
 * This function returns the
 * semi-major axis <b>a</b> (in meters), the eccentricity squared <b>e2</b>
 * and the inverse flattening <b>f</b> for the named ellipsoid. Returns 1 if
 * <b>name</b> is a known ellipsoid, 0 otherwise. 
 *
 *  \param name
 *  \param a
 *  \param e2
 *  \param f
 *  \return int
 */

int 
G_get_spheroid_by_name(const char *name, double *a, double *e2, double *f)
{
    int i;

    (void) read_ellipsoid_table(0);

    for (i = 0; i < count; i++)
    {
	if (G_strcasecmp(name, table[i].name) == 0)
	{
	    *a = table[i].a;
	    *e2 = table[i].e2;
	    *f = table[i].f;
	    return 1;
	}
    }
    return 0;
}


/*!
 * \brief get description for <b>n</b><i>th</i> ellipsoid
 *
 * This function returns a pointer to the
 * description text for the <b>n</b><i>th</i> ellipsoid. If <b>n</b> is less
 * than 0 or greater than the number of known ellipsoids, it returns a NULL
 * pointer. 
 *
 *  \param n
 *  \return char * 
 */

char *
G_ellipsoid_description(int n)
{
    (void) read_ellipsoid_table(0);
    return n>=0 && n < count ? table[n].descr : NULL;
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
        return (*e2 >= 0.0);
    }

    if (sscanf (s2, "f=1/%lf", f) == 1)
    {
	if (*f <= 0.0) 
	    return 0;
	recipf = (double)1.0/(*f);
	*e2 = recipf + recipf - recipf * recipf;
	return (*e2 >= 0.0);
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
	return (*e2 >= 0.0);
    }
    return 0;
}

void ellipsoid_table_file(char *file)
{
   sprintf (file, "%s/etc/ellipse.table", G_gisbase());
   return;
}

static int 
compare_table_names(const void *pa, const void *pb)
{
    const struct table *a = pa, *b = pb;
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
	sprintf (buf, _("unable to open ellipsoid table file: %s"), file);
	fatal ? G_fatal_error(buf) : G_warning (buf);
	return 0;
    }

    err = 0;
    *badlines = 0;
    for (line = 1; G_getl2 (buf, sizeof buf, fd); line++)
    {
	G_strip (buf);
	if (*buf == 0 || *buf == '#')
	    continue;

	if (sscanf (buf, "%s  \"%99[^\"]\" %s %s", name, descr, buf1, buf2) != 4)
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
	qsort (table, count, sizeof(*table), compare_table_names);
	return 1;
    }
    
    (fatal ? G_fatal_error : G_warning)(
	(err > 1)
	    ? _("Lines%s of ellipsoid table file <%s> are invalid")
	    : _("Line%s of ellipsoid table file <%s> is invalid"),
	badlines, file);

    return 0;
}
