#include "gis.h"

#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif

#define LINE	0
#define AREA	1
#define DOT		2

#define FALSE	0
#define TRUE	1
#define BACKWARD	0
#define FORWARD	1

struct head
{
	char organization[30] ;
	char date[20] ;
	char your_name[20] ;
	char map_name[41] ;
	char source_date[11] ;
	int  orig_scale ;
	char line_3[73] ;
	int plani_zone ;
	double W, E, S, N ;
	double digit_thresh ;
	double map_thresh ;
} ;

struct  COOR
   {
	   struct COOR *bptr,*fptr;
	   int row,col,node;
   };

GLOBAL struct COOR *list;
GLOBAL struct COOR *coor;
GLOBAL struct  Cell_head	window;
GLOBAL int	row,col;
GLOBAL CELL	*cur_row,*next_row;
GLOBAL struct head head ;
GLOBAL char cellname[40], *cellmapset;
GLOBAL int row_buf_size, casenum, cell;
GLOBAL char *dlgname ;
GLOBAL char *digitname ;
GLOBAL char *digitname2 ;
GLOBAL FILE *digit ;
GLOBAL FILE *digit2 ;
FILE  *fopen();
CELL *nrow,*crow;

#define getpt ((struct COOR *)malloc(sizeof(struct COOR)))
#define NULPTR ( (struct COOR *) NULL )
