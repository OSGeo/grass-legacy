/*
 **********************************************************************
 *
 *   G_edit_cellhd (cellhd, as_cellhd, with_default_wind)
 *      struct Cell_head *cellhd   (cellhd to be defined)
 *      int as_cellhd 
 *        0 = window - user input resolutions
 *        1 = cellhd - rows and cols must be set
 *
 *   Screen oriented user interactive session for modifying a cell header
 *   or window.
 *   Uses the visual_ask V_ask routines.  As such, programs including
 *   this must load the GRASS library $(VASK)
 *
 *   returns:
 *      -1 error of some sort, or user cancels the edit
 *       0 ok
 *
 **********************************************************************/
#include "gis.h"

static struct
{
    int type; /* 0 both, 1 window, 2 cellhd */
    char *text;
} screen[] = {
1,"                              IDENTIFY WINDOW",
2,"                           IDENTIFY CELL HEADER",
0,"",
0,"           ============================= DEFAULT WINDOW ========",
0,"           |          Default North:                           |",
0,"           |                                                   |",
1,"           |           =======  YOUR WINDOW  =======           |",
2,"           |           =======  CELL HEADER  =======           |",
0,"           |           | NORTH EDGE:               |           |",
0,"           |           |                           |           |",
0," Def. West |WEST EDGE  |                           |EAST EDGE  | Def. East",
0,"           |           |                           |           |",
0,"           |           | SOUTH EDGE:               |           |",
0,"           |           =============================           |",
0,"           |                                                   |",
0,"           |          Default South:                           |",
0,"           =====================================================",
0,"           PROJECTION:                                ZONE:",
0,"",
1,"                   Default   GRID RESOLUTION   Window",
1,"                            --- East-West ---",
1,"                            -- North-South --",
-1,NULL};

G_edit_cellhd (cellhd, as_cellhd)
    struct Cell_head *cellhd ;
{
    char ll_north[20];
    char ll_south[20];
    char ll_east[20];
    char ll_west[20];
    char ll_nsres[20];
    char ll_ewres[20];
    char ll_def_north[20];
    char ll_def_south[20];
    char ll_def_east[20];
    char ll_def_west[20];
    char ll_def_ewres[20];
    char ll_def_nsres[20];
    char projection[80];

    struct Cell_head def_wind ;
    double north, south, east, west ;
    double nsres, ewres;
    char buf[64], buf2[30];
    short ok ;
    int i,line;
    char *prj, *G_projection_name();
    char *err, *G_adjust_Cell_head();

    if (as_cellhd && (cellhd->rows <= 0 || cellhd->cols <= 0))
    {
	fprintf (stderr, "G_edit_cellhd() - programmer error\n");
	fprintf (stderr, "  ** rows and cols must be positive **\n");
	return -1;
    }
    if (G_get_default_window(&def_wind) != 1)
        return -1 ;

    prj = G_projection_name (def_wind.proj);
    if (!prj) prj = "** unknown **";
    sprintf (projection, "%d (%s)", def_wind.proj, prj) ;

    if(cellhd->proj < 0)
    {
	cellhd->proj        = def_wind.proj ;
	cellhd->zone        = def_wind.zone ;
    }
    else if(cellhd->zone < 0)
	cellhd->zone        = def_wind.zone ;

    if (cellhd->west >= cellhd->east || cellhd->south >= cellhd->north) 
    {
        cellhd->north       = def_wind.north     ; 
        cellhd->south       = def_wind.south     ;
        cellhd->west        = def_wind.west      ; 
        cellhd->east        = def_wind.east      ;
        cellhd->ew_res      = def_wind.ew_res    ;
        cellhd->ns_res      = def_wind.ns_res    ;
	cellhd->rows        = def_wind.rows      ;
	cellhd->cols        = def_wind.cols      ;
    }

    if (cellhd->proj != def_wind.proj)
    {
	printf ("%s projection %d differs from default window projection %d\n",
	    as_cellhd?"cell header":"window", cellhd->proj, def_wind.proj);
	if (!G_yes("do you want to make them match? ", 1))
	    return -1;
	cellhd->proj = def_wind.proj;
	cellhd->zone = def_wind.zone;
    }
    if (cellhd->zone != def_wind.zone)
    {
	printf ("%s zone %d differs from default window zone %d\n",
	    as_cellhd?"cell header":"window", cellhd->zone, def_wind.zone);
	if (!G_yes("do you want to make them match? ", 1))
		return -1;
	cellhd->zone = def_wind.zone;
    }

    *ll_def_north = 0;
    *ll_def_south = 0;
    *ll_def_east = 0;
    *ll_def_west = 0;
    *ll_def_ewres = 0;
    *ll_def_nsres = 0;
    G_format_northing (def_wind.north, ll_def_north, def_wind.proj);
    G_format_northing (def_wind.south, ll_def_south, def_wind.proj);
    G_format_easting  (def_wind.east,  ll_def_east,  def_wind.proj);
    G_format_easting  (def_wind.west,  ll_def_west, def_wind.proj);
    G_format_resolution (def_wind.ew_res, ll_def_ewres, def_wind.proj);
    G_format_resolution (def_wind.ns_res, ll_def_nsres, def_wind.proj);

    *ll_north = 0;
    *ll_south = 0;
    *ll_east = 0;
    *ll_west = 0;
    *ll_ewres = 0;
    *ll_nsres = 0;
    G_format_northing (cellhd->north, ll_north, cellhd->proj);
    G_format_northing (cellhd->south, ll_south, cellhd->proj);
    G_format_easting  (cellhd->east, ll_east, cellhd->proj);
    G_format_easting  (cellhd->west, ll_west, cellhd->proj);
    G_format_resolution (cellhd->ew_res, ll_ewres, cellhd->proj);
    G_format_resolution (cellhd->ns_res, ll_nsres, cellhd->proj);


while(1)
{
    ok = 1;

/* List window options on the screen for the user to answer */
    V_clear() ;
    line = 0;
    for (i = 0; screen[i].text; i++)
    {
	switch (screen[i].type)
	{
	case 0: V_line (line++, screen[i].text); break;
	case 1: if (!as_cellhd) V_line (line++, screen[i].text); break;
	case 2: if (as_cellhd) V_line (line++, screen[i].text); break;
	}
    }

/* V_ques ( variable, type, row, col, length) ; */
    V_ques (ll_north, 's',  6, 36, max(11,strlen(ll_north)));
    V_ques (ll_south, 's', 10, 36, max(11,strlen(ll_south))) ;
    V_ques (ll_west,  's',  9, 12, max(11,strlen(ll_west))) ;
    V_ques (ll_east,  's',  9, 52, max(11,strlen(ll_east))) ;
    if (!as_cellhd)
    {
	V_ques (ll_ewres, 's', 18, 48, max(10,strlen(ll_ewres))) ;
	V_ques (ll_nsres, 's', 19, 48, max(10,strlen(ll_nsres))) ;
    }

    V_const (ll_def_north, 's',  3, 36, strlen(ll_def_north)) ;
    V_const (ll_def_south, 's', 13, 36, strlen(ll_def_north)) ;
    V_const (ll_def_west,  's',  9,  1, strlen(ll_def_west)) ;
    V_const (ll_def_east,  's',  9, 65, strlen(ll_def_east)) ;
    if (!as_cellhd)
    {
	V_const (ll_def_ewres, 's', 18, 21, strlen(ll_def_ewres)) ;
	V_const (ll_def_nsres, 's', 19, 21, strlen(ll_def_nsres)) ;
    }

    V_const (projection,   's', 15, 23, strlen(projection));
    V_const (&def_wind.zone,'i', 15,60,3);


    V_intrpt_ok();
    if(!V_call())
	return -1;

    G_squeeze (ll_north);
    G_squeeze (ll_south);
    G_squeeze (ll_east);
    G_squeeze (ll_west);
    if (!as_cellhd)
    {
	G_squeeze (ll_ewres);
	G_squeeze (ll_nsres);
    }

    if (!G_scan_northing (ll_north, &cellhd->north, cellhd->proj))
    {
	printf("Illegal value for north: %s\n",ll_north);
	ok = 0;
    }
    if (!G_scan_northing (ll_south, &cellhd->south, cellhd->proj))
    {
	printf("Illegal value for south: %s\n", ll_south);
	ok = 0;
    }
    if (!G_scan_easting (ll_east, &cellhd->east, cellhd->proj))
    {
	printf("Illegal value for east: %s\n", ll_east);
	ok = 0;
    }
    if (!G_scan_easting (ll_west, &cellhd->west, cellhd->proj))
    {
	printf("Illegal value for west: %s\n", ll_west);
	ok = 0;
    }
    if (!as_cellhd)
    {
	if (!G_scan_resolution (ll_ewres, &cellhd->ew_res, cellhd->proj))
	{
	    printf("Illegal east-west resolution: %s\n", ll_ewres);
	    ok = 0;
	}
	if (!G_scan_resolution (ll_nsres, &cellhd->ns_res, cellhd->proj))
	{
	    printf("Illegal north-south resolution: %s\n", ll_nsres);
	    ok = 0;
	}
    }
    if (!ok)
    {
	hitreturn();
	continue;
    }

/* Adjust and complete the cell header */

    north = cellhd->north;
    south = cellhd->south;
    east  = cellhd->east ;
    west  = cellhd->west ;
    nsres = cellhd->ns_res;
    ewres = cellhd->ew_res;

    if (err = G_adjust_Cell_head(cellhd,as_cellhd,as_cellhd))
    {
	printf ("%s\n", err);
	hitreturn();
	continue;
    }
    if (as_cellhd)
    {
	nsres = cellhd->ns_res;
	ewres = cellhd->ew_res;
    }

SHOW:
    printf("\n\n") ;
    printf("  projection:   %s\n", projection);
    printf("  zone:         %d\n", cellhd->zone);

    G_format_northing (cellhd->north, buf,  cellhd->proj);
    G_format_northing (north,         buf2, cellhd->proj);
    printf("  north:       %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to match resolution)");
    }
    printf("\n");

    G_format_northing (cellhd->south, buf,  cellhd->proj);
    G_format_northing (south,         buf2, cellhd->proj);
    printf("  south:       %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to match resolution)");
    }
    printf("\n");

    G_format_easting (cellhd->east, buf,  cellhd->proj);
    G_format_easting (east,         buf2, cellhd->proj);
    printf("  east:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to match resolution)");
    }
    printf("\n");

    G_format_easting (cellhd->west, buf,  cellhd->proj);
    G_format_easting (west,         buf2, cellhd->proj);
    printf("  west:        %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to match resolution)");
    }
    printf("\n");

    printf("\n");
    G_format_resolution (cellhd->ew_res, buf,  cellhd->proj);
    G_format_resolution (ewres,          buf2, cellhd->proj);
    printf("  e-w res:     %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to conform to grid)");
    }
    printf("\n");

    G_format_resolution (cellhd->ns_res, buf,  cellhd->proj);
    G_format_resolution (nsres,          buf2, cellhd->proj);
    printf("  n-s res:     %s", buf);
    if (strcmp (buf, buf2) != 0)
    {
	ok = 0;
	printf("  (Changed to conform to grid)");
    }
    printf("\n");

    printf("\n");
    printf("  total rows:  %15d\n",    cellhd->rows);
    printf("  total cols:  %15d\n",    cellhd->cols);
    sprintf(buf,"%ld",    (long) cellhd->rows * cellhd->cols);
    G_insert_commas(buf);
    printf("  total cells: %15s\n",    buf);
    printf("\n");

    if (cellhd->north > def_wind.north)
    {
	printf ("warning - north falls outside the default window\n");
	ok = 0;
    }
    if (cellhd->south < def_wind.south)
    {
	printf ("warning - south falls outside the default window\n");
	ok = 0;
    }
    if (cellhd->proj != PROJECTION_LL)
    {
	if (cellhd->east > def_wind.east)
	{
	    printf ("warning - east falls outside the default window\n");
	    ok = 0;
	}
	if (cellhd->west < def_wind.west)
	{
	    printf ("warning - west falls outside the default window\n");
	    ok = 0;
	}
    }
ASK:
    printf("\nDo you accept this %s? (y/n) [%s] > ",
	as_cellhd?"cell header":"window", ok?"y":"n") ;
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

static
hitreturn()
{
    char buf[100];

    printf ("hit RETURN -->");
    if (!gets(buf)) exit(0);
    G_strip (buf);
    if (strcmp (buf, "exit") == 0) exit(0);
}

static
max(a,b)
{
    return a>b?a:b;
}
