/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Omaha District under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

*******************************************************************************/

#define TRUE  1
#define FALSE 0

#define MAX_PVERTS 256

#define FNAMELEN   257
#define BUFFLEN   1024

/*-----------------------*/
/* structure definitions */
/*-----------------------*/
#define C_HEAD struct Cell_head
#define M_INFO struct Map_info
#define L_PNTS struct line_pnts
#define X_INFO struct xs_info
 
/*-----------------------*/
/* function declarations */
/*-----------------------*/
#include "raster.h"
#include "display.h"
#include "gis.h"
/* rsv.c */
int command_line(int, char **);
int get_input(void);


/* misc.c */
int init_graphics(C_HEAD *);
int cell_to_coord(C_HEAD *, int, int, double *, double *);
/* mkreach.c */
char *make_reach(M_INFO *);
int reach_input(M_INFO *, M_INFO *, FILE **);
int set_reach(M_INFO *, M_INFO *, FILE *);
int set_sides(double *, double *, double *, double *, L_PNTS *, L_PNTS *);
int reverse(L_PNTS *, L_PNTS *);
int append_points(L_PNTS *, L_PNTS *);
int get_interior_pt(L_PNTS *, int, double *, double *);
/* sort.c */
int user_sort(M_INFO *, char *, M_INFO *);
int vector_sort(M_INFO *, char *, M_INFO *);
int value_sort(M_INFO *, char *, M_INFO *);
int cline_sort(M_INFO *, L_PNTS *);
int secno_sort(M_INFO *);
int add_sort_entry(M_INFO *, int, L_PNTS *, int, double);
int write_sorted(M_INFO *, char *, M_INFO *);
int get_xsect_map(char *, char *, M_INFO *);
/* util.c */
int init_graphics(C_HEAD *);
/* vector.c */
int copy_vector(M_INFO *, M_INFO *, int *);
