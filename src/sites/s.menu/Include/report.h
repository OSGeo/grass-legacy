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

REPORT *report_open();
REPORT *report_open_ref();
