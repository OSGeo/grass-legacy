#ifndef FILE
#include <stdio.h>
#endif

typedef struct 
{
    FILE *fd;
    /*
    char buf[1024];
    char *field[500];
    */
    char buf[10240];
    char *field[5000];
    int nfields;
    int nlayers;
    int npoints;
    struct
    {
	long cur;
	long next;
	long layers;
	long layers_next;
	long cats;
	long cats_next;
	long points;
	long points_next;
	long data;
	long data_next;
    } offset;
    struct
    {
	int size;
	int *down;
	int *right;
	int center;
	int n;
    } matrix;
    char *site_list_name;
    char *site_list_desc;
    char *location;
    char *fullname;
    char *mapset;
    double north;
    double south;
    double west;
    double east;
    double ns_res;
    double ew_res;

} REPORT ;

/* report.c */
REPORT *report_open (char *);
REPORT *report_open_ref (char *, REPORT *);
int report_close (REPORT *);
int report_read (REPORT *);
int report_record (REPORT *, char *);
int report_read_record ( REPORT *, char *);
int report_scan (REPORT *);
int report_matrix (REPORT *);
/* rprt_finds.c */
int report_find_layer (REPORT *,int);
int report_find_cat (REPORT *,int,int);
int report_find_point (REPORT *,int) ;
int report_find_data (REPORT *,int,int) ;
/* rprt_seeks.c */
int report_seek_layers ( REPORT *);
int report_seek_points ( REPORT *);
int report_seek_cats ( REPORT *);
int report_seek_data ( REPORT *);
/* meta_reprt.c */
int meta_report(char *, char *, char *, int, int);
/* parse.c */
int parse(char *, char *[], int, char *);
/* region.c */
char *format_res(double, char *, int);
char *format_east(double, char *, int);
char *format_north(double, char *, int);
int scan_north(char *, double *);
int scan_east(char *, double *);
int scan_res(char *, double *);
/* rpt_screen.c */
int new_report_screen(void);
#ifndef __linux__
/* memcopy.c */
int memcopy(char *, char *, int);
#endif

/* execute.c */
int execute(char *);
/* hitreturn.c */
int hitreturn(void);
/* scan_int.c */
int scan_int(char *, int *);
/* scn_double.c */
int scan_double(char *, double *);
/* scopy.c */
int scopy(char *, char *, int);
/* sort_int.c */
int sort_int(int [], int, int);
/* counter.c */
int counter_reset(char *, int);
int counter(int);
/* announce.c */
int announce(char *);
/* ask_quad.c */
int ask_quad(int *);
/* die.c */
int die(char *);
/* yes.c */
int yes(char *);
