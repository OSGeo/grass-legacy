/* local definitions for coordinate conversion library */

static struct table
{
  char *name;   /* Name/acronym of map datum */
  char *descr;  /* description for map datum */
  char *ellps;  /* acronym for ellipsoid used with this datum */
  double dx;    /* delta x */
  double dy;    /* delta y */
  double dz;    /* delta z */
} *table = NULL;

/* datum.c */
static int same(const char *, const char *);
static char *datum_table_file(char *);
static int compare_table_names(const struct table *, const struct table *);
static int read_datum_table(int);
static int get_dx_dy_dz(const char *, const char *, const char *, double *, double *, double *);
