/*
 * $Id$
 */

#ifndef MAIN
# define MAIN extern
#endif

#include "gis.h"
#include "Vect.h"

typedef struct {
    int     cat;   /* category */
    int     i1;    /* value (count) */
    double  d1,d2; /* values (length or area or x,y) */
}VALUE;

MAIN VALUE   *Values;

#define OPTIONS struct _options_
MAIN OPTIONS
{
    char *name;
    char *mapset;
    int  field;    
    char *col1;
    char *col2;
    int  type;
    int  option;
    int  print;     /* print only */
    int  sql;       /* print only sql statements */
    int  units;
} options;

#define VSTAT struct _vstat_
MAIN VSTAT
{
    int  rcat;      /* number of categories read from map */
    int  select;    /* number of categories selected from DB */
    int  exist;     /* number of cats existing in selection from DB */
    int  notexist;  /* number of cats not existing in selection from DB */
    int  dupl;      /* number of cats with duplicate elements (currently O_COOR only) */
    int  update;    /* number of updated rows */
    int  error;     /* number of errors */
} vstat;

#define O_CAT		1
#define O_AREA		2
#define O_LENGTH	3
#define O_COUNT		4
#define O_COOR		5

#define U_ACRES		1
#define U_HECTARES	2
#define U_KILOMETERS	3
#define U_METERS	4
#define U_MILES		5
#define U_FEET		6

/* areas.c */
int read_areas(struct Map_info *);

/* calc.c */
double length(register int, register double *, register double *);

/* find.c */
int find_cat(int);

/* line.c */
int read_lines(struct Map_info *);

/* parse.c */
int parse_command_line(int, char *[]);

/* report.c */
int report(void);
int print_stat(void);

/* units.c */
int conv_units(void);

/* update.c */
int update (struct Map_info *);
