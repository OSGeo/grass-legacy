/*
**  Written by Bill Brown, Winter 1991 - 1992 
**  US Army Construction Engineering Research Lab
*/

/*
** Copyright USA CERL 1992. All rights reserved.
*/

#include "externs.h"
/*
#include "gis.h"
*/


show_cat (width, mwidth, name, mapset, cat, label, terse, fs)
    char *name, *mapset, *label;
    int terse, cat, width, mwidth;
    char * fs;
{
    char *fname;
    if (terse)
    {
	fname = G_fully_qualified_name(name,mapset);
	fprintf (stderr, "%s%s%d%s%s\n", fname, fs, cat, fs, label);
    }
    else
	fprintf (stderr, "%*s in %-*s (%d)%s\n", width, name, mwidth, mapset, cat, label);
}

show_utm (north, east, window, terse, button, fs)
    double north, east;
    struct Cell_head *window;
    int terse, button;
    char * fs;
{
    char e[50], n[50];

    if (window->proj == PROJECTION_LL && !isatty (fileno (stdout)))
    {
	/* format in decimal rather than d.m.s */
	G_format_northing (north, n, -1);
	G_format_easting  (east,  e, -1);
    }
    else
    {
	G_format_northing (north, n, window->proj);
	G_format_easting  (east,  e, window->proj);
    }

    if (terse)
    {
	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "\n%s%s%s%s%d\n", e, fs, n, fs, button);
	fprintf (stderr, "\n%s%s%s%s%d\n", e, fs, n, fs, button);
    }
    else
    {
	if (window->proj != PROJECTION_LL)
	{
	    strcat (n, "(N)");
	    strcat (e, "(E)");
	}

	if (!isatty(fileno(stdout)))
	    fprintf (stdout, "\n%s %s\n", e, n);
	fprintf (stderr, "\n%s %s\n", e, n);
    }
}

show_what(east,north,elevation)
float east, north, elevation;
{
    int row, col, width, mwidth, map_num, i;
    CELL *buf;
    char *name_map[3];
    char buff[128];
    FILEDESC cellfile[3];
    struct Categories cats[3];

			
    cellfile[0] = cellfile[1] = cellfile[2] = NULL;    
    if (Three_map) map_num = 3;
    else map_num = 1;

    buf = G_allocate_cell_buf();
    
    show_utm (north, east, &wind, 0, 0, "");
    fprintf(stderr,"elevation: %f\n", elevation);

    {       /* elevation info */

    name_map[0] = G_find_file2 ("cell", Elevname, "");
    if ((cellfile[0] = G_open_cell_old(Elevname, name_map[0])) == -1) 
    {
	sprintf(buff,"Not able to open cellfile for [%s]", Elevname);
	G_warning(buff);
	return(-1);
    }
    width = strlen(Elevname);
    mwidth = strlen(name_map[0]);
    row = (wind.north - north) / wind.ns_res ;
    col = (east - wind.west) / wind.ew_res ;
    if (row < 0 || row >= wind.rows){
	G_close_cell(cellfile[0]);
    }
    else if (col < 0 || col >= wind.cols){
	G_close_cell(cellfile[0]);
    }
    else{
	if (G_get_map_row (cellfile[0], buf, row) < 0)
	    show_cat (width, mwidth, Elevname, name_map[0], (CELL) 0,
				"ERROR reading cell file", 0, "");
	else{
	    if (G_read_cats (Elevname, name_map[0], &cats[0]) >= 0)
				    /*cats[0].num = -1;*/
				    /*cats[0].count = -1;*/
	    show_cat (width, mwidth, Elevname, name_map[0], buf[col],
			    G_get_cat (buf[col], &cats[0]), 0, "");
	}
	G_close_cell(cellfile[0]);
    }

    }
	
    for(i = 0 ; i < map_num; ++i){    /* color info */

	name_map[i] = G_find_file2 ("cell", Cellname[i], "");
	if ((cellfile[i] = G_open_cell_old(Cellname[i], name_map[i])) == -1) 
	{
	    sprintf(buff,"Not able to open cellfile for [%s]", Cellname[i]);
	    G_warning(buff);
	    return(-1);
	}
	width = strlen(Cellname[i]);
	mwidth = strlen(name_map[i]);
	row = (wind.north - north) / wind.ns_res ;
	col = (east - wind.west) / wind.ew_res ;
	if (row < 0 || row >= wind.rows){
	    G_close_cell(cellfile[i]);
	    continue;
	}
	if (col < 0 || col >= wind.cols){
	    G_close_cell(cellfile[i]);
	    continue;
	}
	if (G_get_map_row (cellfile[i], buf, row) < 0)
	    show_cat (width, mwidth, Cellname[i], name_map[i], (CELL) 0,
				"ERROR reading cell file", 0, "");
	else{
	    if (G_read_cats (Cellname[i], name_map[i], &cats[i]) >= 0)
				    /* cats[i].num = -1; */
				    /*cats[i].count = -1;*/
	    show_cat (width, mwidth, Cellname[i], name_map[i], buf[col],
			    G_get_cat (buf[col], &cats[i]), 0, "");
	}
	G_close_cell(cellfile[i]);
    }

    free(buf);
}

void
show_where_viewpt()
{
double east, north, elev, height, p[3];
char ebuf[50], nbuf[50];

    
    east = FROM_TO[FROM][X]/XYscale + wind.west;
    north = FROM_TO[FROM][Y]/XYscale + wind.south;
    if(Z_exag)
	elev = FROM_TO[FROM][Z]/Z_exag + Z_Min_real;
    else
	elev = 0.0;

    G_format_northing (north, nbuf, wind.proj);
    G_format_easting  (east,  ebuf, wind.proj);
   
    if (wind.proj != PROJECTION_LL)
    {
	strcat (nbuf, "(N)");
	strcat (ebuf, "(E)");
    }

    fprintf(stderr,"\n%s  %s\nelevation: %.2lf\n", ebuf,nbuf,elev);

    p[X] = FROM_TO[FROM][X];
    p[Y] = FROM_TO[FROM][Y];
    if(viewcell_interp(p))
	fprintf(stderr,"height: %.2lf\n", elev - p[Z]);
}

