#define AS_CELLHD 1
#define AS_WINDOW 0
#define AS_DEF_WINDOW -1
/* modified 26nov to use word region instead of window
 * as far as the USER is concerned.
 */
/*
 **********************************************************************
 *
 *   G_edit_cellhd (cellhd, type)
 *      struct Cell_head *cellhd   (cellhd to be defined)
 *      int type 
 *        0 = region - user input resolutions
 *       -1 = default region - user input resolutions
 *        1 = cellhd - rows and cols must be set
 *
 *   Screen oriented user interactive session for modifying a cell header
 *   or region.
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library $(VASKLIB) and include $(CURSES) in
 *   the compile line
 *
 *   returns:
 *      -1 error of some sort, or user cancels the edit
 *       0 ok
 *
 **********************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "G3d.h"
#include "vask.h"
#include "local_proto.h"

static char *window_screen[] = {
"                              IDENTIFY REGION",
"",
"           ============================= DEFAULT REGION ========",
"           |          Default North:                           |",
"           |                                                   |",
"           |           =======  YOUR REGION  =======           |",
"           |           | NORTH EDGE:               |           |",
"           |           |                           |           |",
" Def. West |WEST EDGE  |                           |EAST EDGE  | Def. East",
"           |           |                           |           |",
"           |           | SOUTH EDGE:               |           |",
"           |           =============================           |",
"           |                                                   |",
"           |          Default South:                           |",
"           =====================================================",
" Def. Top  TOP EDGE                                 BOTTOM EDGE  Def. Bottom",
"",
"           PROJECTION:                                ZONE:",
"                   Default   GRID RESOLUTION   Region",
"                            --- East-West ---",
"                            -- North-South --",
"                            -- Top-Bottom ---",
NULL};

static int
hitreturn()
{
    char buf[100];

    fprintf(stderr, "hit RETURN -->");
    if (!G_gets(buf)) exit(0);
    G_strip (buf);
    if (strcmp (buf, "exit") == 0) exit(0);
    return 0;
}

/* the following is copied from wind_scan.c */
static int
scan_double (buf, value)
    char *buf;
    double *value;
{
    char junk[2];

/* use sscanf to convert buf to double
 * make sure value doesn't have other characters after it
 */
    *junk = 0;
    *value = 0.0;
    if(sscanf (buf, "%lf%1s", value, junk) == 1 && *junk == 0)
    {
       while(*buf) buf++;
       buf--;
       if(*buf>='A'&&*buf<='Z')
 	  return 0;
       if(*buf>='a'&&*buf<='z')
 	  return 0;
       return 1;
     }
     return 0;
}

int
edit_3dcellhd (cellhd)
    G3D_Region *cellhd ;
{
    char ll_north[20], ll_south[20], ll_east[20], ll_west[20];
    char ll_top[20], ll_bottom[20];
    char ll_nsres[20], ll_ewres[20], ll_tbres[20];
    char ll_def_north[20], ll_def_south[20], ll_def_east[20], ll_def_west[20];
    char ll_def_top[20], ll_def_bottom[20];
    char ll_def_nsres[20], ll_def_ewres[20], ll_def_tbres[20];
    char projection[80];
    char **screen;
    G3D_Region def_wind ;
    double north, south, east, west, top, bottom, nsres, ewres, tbres;
    char buf[64], buf2[30];
    short ok ;
    int line;
    char *prj, *G__projection_name();
    char *err, *G_adjust_Cell_head();
    char path[1000];

    G__file_name (path, "", G3D_DEFAULT_WINDOW_ELEMENT, G3D_PERMANENT_MAPSET);
    if (G3d_readWindow (&def_wind, path) == 0) return -1 ;
    if(cellhd->proj < 0) {
      cellhd->proj = def_wind.proj ;
      cellhd->zone = def_wind.zone ;
    } else 
      if(cellhd->zone < 0) cellhd->zone = def_wind.zone ;

    prj = G__projection_name (cellhd->proj);
    if (!prj) prj = "** unknown **";
    sprintf ( projection, "%d (%s)", cellhd->proj, prj) ;

    if (cellhd->west >= cellhd->east || cellhd->south >= cellhd->north) {
      cellhd->north = def_wind.north; 
      cellhd->south = def_wind.south;
      cellhd->west = def_wind.west; 
      cellhd->east = def_wind.east;
      cellhd->top = def_wind.top; 
      cellhd->bottom = def_wind.bottom;

      cellhd->ew_res = def_wind.ew_res;
      cellhd->ns_res = def_wind.ns_res;
      cellhd->tb_res = def_wind.tb_res;
      cellhd->rows = def_wind.rows;
      cellhd->cols = def_wind.cols;
      cellhd->depths = def_wind.depths;

      if (cellhd->proj != def_wind.proj) {
	fprintf(stderr, "%s projection %d differs from default projection %d\n",
		"region", cellhd->proj, def_wind.proj);
	if (!G_yes("do you want to make them match? ", 1)) return -1;
	cellhd->proj = def_wind.proj;
	cellhd->zone = def_wind.zone;
      }
      if (cellhd->zone != def_wind.zone) {
	fprintf(stderr, "%s zone %d differs from default zone %d\n",
		"region", cellhd->zone, def_wind.zone);
	    if (!G_yes("do you want to make them match? ", 1)) return -1;
	cellhd->zone = def_wind.zone;
      }
    }

    *ll_def_north = *ll_def_south = *ll_def_bottom = 0;
    *ll_def_east = *ll_def_west = *ll_def_top = 0;
    *ll_def_ewres = *ll_def_nsres = *ll_def_tbres = 0;
    G_format_northing (def_wind.north, ll_def_north, def_wind.proj);
    G_format_northing (def_wind.south, ll_def_south, def_wind.proj);
    G_format_easting  (def_wind.east,  ll_def_east,  def_wind.proj);
    G_format_easting  (def_wind.west,  ll_def_west, def_wind.proj);
    format_double  (def_wind.top,  ll_def_top);
    format_double  (def_wind.bottom,  ll_def_bottom);
    G_format_resolution (def_wind.ns_res, ll_def_nsres, def_wind.proj);
    G_format_resolution (def_wind.ew_res, ll_def_ewres, def_wind.proj);
    format_double (def_wind.tb_res, ll_def_tbres);

    *ll_north = *ll_south = *ll_east = *ll_west = *ll_top = *ll_bottom = 0;
    *ll_ewres = *ll_nsres = *ll_tbres = 0;
    G_format_northing (cellhd->north, ll_north, cellhd->proj);
    G_format_northing (cellhd->south, ll_south, cellhd->proj);
    G_format_easting  (cellhd->east, ll_east, cellhd->proj);
    G_format_easting  (cellhd->west, ll_west, cellhd->proj);
    format_double  (cellhd->top,  ll_top);
    format_double  (cellhd->bottom,  ll_bottom);
    G_format_resolution (cellhd->ew_res, ll_ewres, cellhd->proj);
    G_format_resolution (cellhd->ns_res, ll_nsres, cellhd->proj);
    format_double (cellhd->tb_res, ll_tbres);


while(1)
{
    ok = 1;

/* List window options on the screen for the user to answer */

    screen = window_screen;

    V_clear() ;
    line = 0;
    while (*screen) V_line (line++, *screen++);

/* V_ques ( variable, type, row, col, length) ; */
    V_ques (ll_north, 's',  6, 36, max(11,strlen(ll_north)));
    V_ques (ll_south, 's', 10, 36, max(11,strlen(ll_south))) ;
    V_ques (ll_west,  's',  9, 12, max(11,strlen(ll_west))) ;
    V_ques (ll_east,  's',  9, 52, max(11,strlen(ll_east))) ;
    V_ques (ll_top,  's',  16, 12, max(11,strlen(ll_top))) ;
    V_ques (ll_bottom,  's', 16, 52, max(11,strlen(ll_bottom))) ;

    V_ques (ll_ewres, 's', 19, 48, max(10,strlen(ll_ewres))) ;
    V_ques (ll_nsres, 's', 20, 48, max(10,strlen(ll_nsres))) ;
    V_ques (ll_tbres, 's', 21, 48, max(10,strlen(ll_tbres))) ;

    V_const (ll_def_north, 's',  3, 36, strlen(ll_def_north)) ;
    V_const (ll_def_south, 's', 13, 36, strlen(ll_def_north)) ;
    V_const (ll_def_west,  's',  9,  1, strlen(ll_def_west)) ;
    V_const (ll_def_east,  's',  9, 65, strlen(ll_def_east)) ;
    V_const (ll_def_top,  's',  16,  1, strlen(ll_def_top)) ;
    V_const (ll_def_bottom,  's',  16, 65, strlen(ll_def_bottom)) ;

    V_const (ll_def_ewres, 's', 19, 21, strlen(ll_def_ewres)) ;
    V_const (ll_def_nsres, 's', 20, 21, strlen(ll_def_nsres)) ;
    V_const (ll_def_tbres, 's', 21, 21, strlen(ll_def_tbres)) ;

    V_const (projection,   's', 17, 23, strlen(projection));
    V_const (&cellhd->zone,'i', 17,60,3);


    V_intrpt_ok();
    if(!V_call()) return -1;

    G_squeeze (ll_north);
    G_squeeze (ll_south);
    G_squeeze (ll_east);
    G_squeeze (ll_west);
    G_squeeze (ll_top);
    G_squeeze (ll_bottom);

    G_squeeze (ll_ewres);
    G_squeeze (ll_nsres);
    G_squeeze (ll_tbres);

    if (!G_scan_northing (ll_north, &cellhd->north, cellhd->proj)) {
      fprintf(stderr, "Illegal value for north: %s\n",ll_north);
      ok = 0;
    }
    if (!G_scan_northing (ll_south, &cellhd->south, cellhd->proj)) {
      fprintf(stderr, "Illegal value for south: %s\n", ll_south);
      ok = 0;
    }
    if (!G_scan_easting (ll_east, &cellhd->east, cellhd->proj)) {
      fprintf(stderr, "Illegal value for east: %s\n", ll_east);
      ok = 0;
    }
    if (!G_scan_easting (ll_west, &cellhd->west, cellhd->proj)) {
      fprintf(stderr, "Illegal value for west: %s\n", ll_west);
      ok = 0;
    }
    if (!scan_double (ll_top, &cellhd->top, cellhd->proj)) {
      fprintf(stderr, "Illegal value for top: %s\n", ll_west);
      ok = 0;
    }
    if (!scan_double (ll_bottom, &cellhd->bottom)) {
      fprintf(stderr, "Illegal value for bottom: %s\n", ll_east);
      ok = 0;
    }

    if (!G_scan_resolution (ll_ewres, &cellhd->ew_res, cellhd->proj)) {
      fprintf(stderr, "Illegal east-west resolution: %s\n", ll_ewres);
      ok = 0;
    }
    if (!G_scan_resolution (ll_nsres, &cellhd->ns_res, cellhd->proj)) {
      fprintf(stderr, "Illegal north-south resolution: %s\n", ll_nsres);
      ok = 0;
    }
    if (!scan_double (ll_tbres, &cellhd->tb_res)) {
      fprintf(stderr, "Illegal top-bottom resolution: %s\n", ll_nsres);
      ok = 0;
    }
    
    /* Do some checking */
    if (   cellhd->south  >= cellhd->north )
    {
      fprintf(stderr, "North must be greater than south!\n");
      ok = 0;
    }
    else if ( cellhd->west   >= cellhd->east )
    {
      fprintf(stderr, "East must be greater than west!\n");
      ok = 0;
    }
    else if (  cellhd->bottom >= cellhd->top )
    {
      fprintf(stderr, "Top must be greater than bottom!\n");
      ok = 0;
    }
    else if ( cellhd->ew_res <= 0.0 )
    {
      fprintf(stderr, "East-West resolution must be positive!\n");
      ok = 0;
    }
    else if ( cellhd->ns_res <= 0.0 )
    {
      fprintf(stderr, "North-South resolution must be positive!\n");
      ok = 0;
    }
    else if (  cellhd->tb_res <= 0.0 )
    {
       fprintf(stderr, "Top-Bottom resolution must be positive!\n");
       ok = 0;
    }
     
    if (!ok) {
      hitreturn();
      continue;
    }

/* Adjust and complete the cell header */

    north = cellhd->north;
    south = cellhd->south;
    east  = cellhd->east ;
    west  = cellhd->west ;
    top  = cellhd->top ;
    bottom  = cellhd->bottom ;

    nsres = cellhd->ns_res;
    ewres = cellhd->ew_res;
    tbres = cellhd->tb_res;

    G3d_adjustRegionRes (cellhd);

SHOW:
    fprintf(stderr, "\n\n") ;
    fprintf(stderr, "  projection:   %s\n", projection);
    fprintf(stderr, "  zone:         %d\n", cellhd->zone);

    G_format_northing (cellhd->north, buf,  cellhd->proj);
    G_format_northing (north,         buf2, cellhd->proj);
    fprintf(stderr, "  north:       %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    G_format_northing (cellhd->south, buf,  cellhd->proj);
    G_format_northing (south,         buf2, cellhd->proj);
    fprintf(stderr, "  south:       %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    G_format_easting (cellhd->east, buf,  cellhd->proj);
    G_format_easting (east,         buf2, cellhd->proj);
    fprintf(stderr, "  east:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    G_format_easting (cellhd->west, buf,  cellhd->proj);
    G_format_easting (west,         buf2, cellhd->proj);
    fprintf(stderr, "  west:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    format_double (cellhd->top, buf);
    format_double (top,         buf2);
    fprintf(stderr, "  top:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    format_double (cellhd->bottom, buf);
    format_double (bottom,         buf2);
    fprintf(stderr, "  bottom:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to match resolution)");
    }
    fprintf(stderr, "\n");

    fprintf(stderr, "\n");
    G_format_resolution (cellhd->ew_res, buf,  cellhd->proj);
    G_format_resolution (ewres,          buf2, cellhd->proj);
    fprintf(stderr, "  e-w res:     %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to conform to grid)");
    }
    fprintf(stderr, "\n");

    G_format_resolution (cellhd->ns_res, buf,  cellhd->proj);
    G_format_resolution (nsres,          buf2, cellhd->proj);
    fprintf(stderr, "  n-s res:     %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to conform to grid)");
    }
    fprintf(stderr, "\n");

    fprintf(stderr, "\n");
    format_double (cellhd->tb_res, buf);
    format_double (tbres,          buf2);
    fprintf(stderr, "  t-b res:     %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	fprintf(stderr, "  (Changed to conform to grid)");
    }
    fprintf(stderr, "\n");


    fprintf(stderr, "\n");
    fprintf(stderr, "  total   rows:  %15d\n",    cellhd->rows);
    fprintf(stderr, "  total   cols:  %15d\n",    cellhd->cols);
    fprintf(stderr, "  total depths:  %15d\n",    cellhd->depths);
    sprintf ( buf,"%ld",    (long) cellhd->rows * cellhd->cols * cellhd->depths);
    G_insert_commas(buf);
    fprintf(stderr, "  total cells:   %15s\n",    buf);
    fprintf(stderr, "\n");

    if (cellhd->north > def_wind.north) {
      fprintf(stderr, "warning - north falls outside the default region\n");
      ok = 0;
    }
    if (cellhd->south < def_wind.south) {
      fprintf(stderr, "warning - south falls outside the default region\n");
      ok = 0;
    }
    if (cellhd->proj != PROJECTION_LL) {
      if (cellhd->east > def_wind.east) {
	  fprintf(stderr, "warning - east falls outside the default region\n");
	  ok = 0;
	}
      if (cellhd->west < def_wind.west) {
	fprintf(stderr, "warning - west falls outside the default region\n");
	ok = 0;
      }
    }
    if (cellhd->top < def_wind.top) {
      fprintf(stderr, "warning - top falls outside the default region\n");
      ok = 0;
    }
    if (cellhd->bottom < def_wind.bottom) {
      fprintf(stderr, "warning - bottom falls outside the default region\n");
      ok = 0;
    }
    

ASK:
    fprintf(stderr, "\nDo you accept this %s? (y/n) [%s] > ",
	"region", ok?"y":"n") ;
    if(!G_gets(buf))
	goto SHOW;
    G_strip (buf);
    switch (*buf)
    {
    case 0:
	    break;
    case 'y':
    case 'Y':
	    ok = 1 ;
	    break;
    case 'n':
    case 'N':
	    ok = 0;
	    break;
    default:
	    goto ASK;
    }
    if (ok)
	return 0;
}
}


