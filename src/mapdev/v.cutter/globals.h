/**** globals.h ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/



#ifdef MAIN_T
#define GLOBAL_T
#else
#define GLOBAL_T extern
#endif


/* this structure is initialized at begining of program.
**  it is available for use by any subroutine which will not 
**  be calling any other subroutines which could potentially
**  need it also.
*/
GLOBAL_T struct line_pnts *TPoints;
GLOBAL_T int Debug_on;
GLOBAL_T int All;
GLOBAL_T int Quiet;
GLOBAL_T int Do_lines;
GLOBAL_T int Do_areas;
GLOBAL_T struct bbox Cutter_bbox;
GLOBAL_T struct poly_t *OPoly;
GLOBAL_T struct BM *intersect_bitmap;
