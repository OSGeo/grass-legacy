/* %W% %G% */
#include "gis.h"

#ifndef MAIN
#define EXTERN extern
#else
#define EXTERN
#endif

#define RECSIZE  1025   /* USGS record size + 1 */

EXTERN struct Cell_head cellhd;
EXTERN double  bas_elev, P_row, P_col;
EXTERN float x_res, y_res, z_res;
EXTERN char *inf, *of, *buffer, *buf_start, *buf_end;
EXTERN double north, south, east, west;
EXTERN char tapename[80], outname[80], command[300];
EXTERN int fd, profile_size, ignoreWin;
EXTERN int total_cols, ref_zone;
EXTERN FILE *tapefile;
EXTERN CELL *profile_buf;

#undef EXTERN
