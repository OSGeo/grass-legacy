#include "gis.h"

struct units
{
    char *name;
    char *code;
    char marked[2];
};
#ifdef GLOBAL
struct units units[] = {
	{"meters","me"},
	{"kilometers","k"},
	{"feet","f"},
	{"miles","mi"},
	{"acres","a"},
	{"hectares","h"},
	{"counts","c"},
        {NULL,NULL}};
#else
extern struct units units[];
#endif

#ifndef GLOBAL
#define GLOBAL extern
#else
#define GLOBAL
#endif


GLOBAL char *stats_file;
GLOBAL char *report_file;
GLOBAL int nlayers;
GLOBAL struct layer { char *name; char *mapset; char *type;} *layer;

/* ask_layers.c */
int ask_layers(void);
/* ask_units.c */
int ask_units(void);
/* run_report.c */
int run_report(int);
