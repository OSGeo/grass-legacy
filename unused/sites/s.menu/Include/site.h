
#define SITE_LIST struct _sitelist
#define SITE struct _site

SITE
{
    double north;
    double east;
    char *desc;
    SITE *next;
};

SITE_LIST
{
    char name[100];
    char desc[100];
    SITE *cur;
    SITE *first;
    SITE *last;
};

/* copy_sites.c */
int copy_sites (SITE_LIST *,SITE_LIST *,int);
/* count_site.c */
int count_sites (SITE_LIST *,int);
/* eq_sites.c */
int equal_sites (SITE_LIST *,SITE_LIST *);
int cmp_sites (SITE_LIST *,SITE_LIST *);
/* parse.c */
int parse(char *, char *[], int, char *);
/* read_sites.c */
int read_site_list (SITE_LIST *, FILE *);
int get_site_list (SITE_LIST *, char *);
/* region.c */
char *format_res(double, char *, int);
char *format_east(double, char *, int);
char *format_north(double, char *, int);
int scan_north(char *, double *);
int scan_east(char *, double *);
int scan_res(char *, double *);
/* site.c */
int add_site (SITE_LIST *, double,double,char *);
int initialize_site_list (SITE_LIST *);
int rewind_site_list(SITE_LIST *);
int next_site (SITE_LIST *, double *, double *,char **);
int free_site_list(SITE_LIST *);

#ifndef GRASS_GIS_H
#include "gis.h"
#endif
/* ut_to_cell.c */
float northing_to_row(double, struct Cell_head *);
float easting_to_col(double, struct Cell_head *);
/* within_wnd.c */
int within_window(double, double, struct Cell_head *);
/* write_site.c */
int put_site_list (SITE_LIST *,char *,int,int);
int write_site_list (SITE_LIST *,FILE *,int,int);

/* announce.c */
int announce(char *);
/* center.c */
int center(char *, int);
/* chain.c */
int chain(char *);
/* copyfile.c */
int copyfile(char *, char *);
/* counter.c */
int counter_reset(char *, int);
int counter(int);
/* die.c */
int die(char *);
/* execute.c */
int execute(char *);
/* hitreturn.c */
int hitreturn(void);
/* maximum.c */
int maximum(register int *, int);
/* scan_int.c */
int scan_int(char *, int *);
/* trace.c */
int trace(int);
/* yes.c */
int yes(char *);
