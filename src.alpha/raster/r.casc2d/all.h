/*
 * Our "standard" includes
 */

#include <math.h>
#include <stdio.h>
#include <gis.h>
#include <malloc.h>
#include <string.h>

/*
 * Our "standard" defines
 */

#define sign(a) ((a)<0.0? -1.0:1.0)
#define abs(x)  ((x)<0.0? -(x):(x))
#define TRUE 1
#define FALSE 0

/*
 * "Global" shared variable declarations.
 */

/*!!!!!!!!!!!!!!!!!!!!!!!!!
@@
@@ GLOBAL Common Variables
@@
@@!!!!!!!!!!!!!!!!!!!!!!!*/

int   nrows,ncols;
int   GNUM,TNUM;
double w;

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
@@
@@ GLOBAL Channel Routing Variables
@@
@@!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

int   *con_vect, *con_link, *con_node;
int   nlinks,maxnodes;
int   NODES,LINKS,NUM;
float *qlat;

char  *channel_file,*chn_link_map,*chn_node_map;
int   chn_link_fd,chn_node_fd;
CELL  *chn_link_tmp,*chn_node_tmp;

/*!!!!!!!!!!!!!!!!!!!!!!!
@@
@@ GLOBAL Lake Variables
@@
@@!!!!!!!!!!!!!!!!!!!!!!*/

int   *lake_cat,*lake_cells;
double *lake_el, *qtolake;


/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  List of Functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

extern     void   OV_FLOW() ;
extern     void   OV_LAKE() ;
extern     void   INF_NODIST();
extern     void   INF_REDIST();
extern     void   RAIN_SQ_DIS();
extern     void   RAIN_THIESSEN();
extern     void   CH_FLOW();
extern     double   CH_DEPTH();
extern     void   WRITE_FILES();
extern     void   READ_GAGE_FILE();
extern     void   INTERCEPTION();
extern     void   CRASH();

extern     void   read_input();
extern     void   read_table();
extern     void   norm_calc();
extern     void   flow_route();
extern     void   section();
extern     void   coeff();
extern     void   weir_coeff();
extern     void   dsyot();
extern     void   usqot();
extern     void   spill();
extern     void   reservoir();
extern     void   dsqoy();
extern     void   ddsqoy();
