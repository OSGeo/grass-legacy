#include "gis.h"

#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif

#define RECORD_SIZE 1024

GLOBAL struct  Cell_head        cellhd;
GLOBAL int      blocksize,no_files,col_start,rows,cols,elev;
GLOBAL int      DEM,pattern,ref_sys,ref_zone;
GLOBAL float    rel_e,rel_n;
GLOBAL double   bas_elev,P_row,P_col;
GLOBAL int      sides,z_unit,xy_unit;
GLOBAL int      bas_e,bas_n,P_rows,P_cols;
GLOBAL float    x_res,y_res,z_res;
GLOBAL double   max_elev,min_elev;
GLOBAL double   tpeast[4],tpnorth[4];
GLOBAL double   file_north,file_south,file_east,file_west;
GLOBAL char     *inf,*of,*buffer,*buf_start,*buf_end;
GLOBAL double  col,row,n,s,e,w;
GLOBAL int     c,r,cur_row;
GLOBAL char    *tapename,*outname,name[51],command[1024];
GLOBAL int     fd,filestat,tapefile;
GLOBAL int     skip_row,skip_col,profile_buf_size;
GLOBAL int     record_pos;
GLOBAL int     after_hdr;
GLOBAL int     count/* number of files*/ , C_record /* accuracy */;
GLOBAL CELL *profile_buf;
