/*
 * $Id$
 */

#ifndef MAIN
# define MAIN extern
#endif

#include "gis.h"
#include "Vect.h"

MAIN struct Cell_head window ;

typedef struct
{
    int     cat;   /* category */
    int     i1;    /* value */
}CI;  

typedef struct
{
    int     cat;   /* category */
    double  d1;    /* value */
}CD;

typedef struct
{
    int     cat;   /* category */
    int     i1;    /* value    */
    double  d1,d2; /* value */
}CI2D;

typedef struct
{
    int     cat;   /* category */
    char    *c1;   /* value    */
}CC;

MAIN CI     *list_ci;
MAIN CD     *list_cd;
MAIN CI2D   *list_ci2d;
MAIN CC     *list_cc; 

#define OPTIONS struct _options_
MAIN OPTIONS
{
    char *name;
    char *mapset;
    char *table;    
    char *key;
    char *col1;
    char *col2;
    int  type;
    int  option;
    int  print;     /* print only */
    int  sql;       /* print only sql statements */
    int  units;
    int  list;      /* list type */
} options;

#define VSTAT struct _vstat_
MAIN VSTAT
{
    int  cat;       /* number of categories in map */
    int  alloc;     /* allocated in list */
    int  sort;      /* number of sorted cats in list */
    int  rcat;      /* number of categories read from map */
    int  select;    /* number of categories selected from DB */
    int  exist;     /* number of cats existing in selection from DB */
    int  notexist;  /* number of cats not existing in selection from DB */
    int  dupl;      /* number of cats with duplicate elements */
    int  update;    /* number of updated rows */
    int  error;     /* number of errors */
    int  maxerror;  /* max errors allowed */
} vstat;

/* exit codes */
#define OK    0
#define ERROR 1


#define LIST_CI 	1
#define LIST_CD 	2
#define LIST_CI2D 	3
#define LIST_CC 	4

#define O_CAT		1
#define O_LABEL		2
#define O_AREA		3
#define O_LENGTH	4
#define O_COUNT		5
#define O_COOR		6

#define U_ACRES		1
#define U_HECTARES	2
#define U_KILOMETERS	3
#define U_METERS	4
#define U_MILES		5
#define U_FEET		6

/* alloc.c */
int alloc_list(void);
int free_list(void);

/* areas.c */
int read_areas(struct Map_info *, struct Categories *);

/* calc.c */
double length(register int, register double *, register double *);

/* find.c */
int find_cat(int);

/* line.c */
int read_lines(struct Map_info *, struct Categories *);

/* parse.c */
int parse_command_line(int, char *[]);

/* points.c */
int read_points(struct Map_info *, struct Categories *);

/* report.c */
int report(void);
int print_stat(void);

/* units.c */
int conv_units(void);
