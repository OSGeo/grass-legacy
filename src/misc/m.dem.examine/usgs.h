#include "gis.h"

#ifndef MAIN
#define RECORD_SIZE 1024
#define GLOBAL extern
#else
#define GLOBAL
#endif

GLOBAL struct  Cell_head	cellhd;
GLOBAL int		blocksize,rows,cols,elev;
GLOBAL int		DEM,pattern,ref_sys,ref_zone;
GLOBAL float   rel_e,rel_n,bas_elev,P_row,P_col;
GLOBAL int		sides,z_unit,xy_unit;
GLOBAL int		bas_e,bas_n,P_rows,P_cols;
GLOBAL float	max_elev,min_elev,x_res,y_res,z_res;
GLOBAL float	east[4],north[4];
GLOBAL float	file_north,file_south,file_east,file_west;
GLOBAL char	*inf,*of,*buffer,*buf_start,*buf_end;
GLOBAL double  col,row,n,s,e,w;
GLOBAL int     c,r,cur_row;
GLOBAL int     record_pos;
GLOBAL char    *tapename,name[51],command[300];
GLOBAL int     fd,filestat,tapefile;
GLOBAL int     count;
GLOBAL int     skip_row,skip_col,profile_buf_size;
GLOBAL CELL *profile_buf;
