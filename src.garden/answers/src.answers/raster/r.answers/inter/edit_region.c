/*
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

     function: edit_region
     called by: step_1


     this was based on a quick hack of:
 *   G_edit_cur_window (window)
 *   Screen oriented user interactive session for modifying a cell header
 *   or region.
   
     called by step_1(). The purpose of this is to allow the user
     to directly edit the values for the region of study in their
     watershed project. The database default window is used as a 
     reference, and "window" is the either the current region setting
     or the cell head the user asked to use as a reference.  meanwhile, 
     the region information, once set and
     confirmed by the user, will be saved in the answers project
     directory in the user's mapset
     
     this function returns a 1 on error, 2 to indicate "start over"
     and 0 if all went as planned.
     
 */
#include "answers.h"

static char *window_screen[] = {
"",
"",
"              Edit or approve REGION for Watershed Project",
"",
"",
"           +-----------------------Default Region for Mapset --+",
"           |          Default North:                           |",
"           |                                                   |",
"           |           +------ PROJECT REGION -----+           |",
"           |           | NORTH EDGE:               |           |",
"           |           |                           |           |",
" Def. West |WEST EDGE  |                           |EAST EDGE  | Def. East",
"           |           |                           |           |",
"           |           | SOUTH EDGE:               |           |",
"           |           +---------------------------+           |",
"           |                                                   |",
"           |          Default South:                           |",
"           +---------------------------------------------------+",
"",
NULL};

edit_region ()
{

    char ll_north[20];
    char ll_south[20];
    char ll_east[20];
    char ll_west[20];
    char ll_def_north[20];
    char ll_def_south[20];
    char ll_def_east[20];
    char ll_def_west[20];
    char **screen;
    FILE *fd;

    struct Cell_head *tmp_window, def_wind;
    double north, south, east, west;
    char buf[64], buf2[30];
    short ok;
    int line;
    char *G_align_window();
    char *G__get_window();

    if (G_get_default_window(&def_wind) != 1)
    {
	fprintf(stderr, "\n\7WARNING: could not read default window");
        hit_return();
     	return (1);
    }
    if (window.west >= window.east || window.south >= window.north)
    {
	window.north = def_wind.north;
	window.south = def_wind.south;
	window.west = def_wind.west;
	window.east = def_wind.east;
	window.ew_res = def_wind.ew_res;
	window.ns_res = def_wind.ns_res;
	window.rows = def_wind.rows;
	window.cols = def_wind.cols;
    }


    *ll_def_north = 0;
    *ll_def_south = 0;
    *ll_def_east = 0;
    *ll_def_west = 0;
    G_format_northing(def_wind.north, ll_def_north, def_wind.proj);
    G_format_northing(def_wind.south, ll_def_south, def_wind.proj);
    G_format_easting(def_wind.east, ll_def_east, def_wind.proj);
    G_format_easting(def_wind.west, ll_def_west, def_wind.proj);

    *ll_north = 0;
    *ll_south = 0;
    *ll_east = 0;
    *ll_west = 0;

    sprintf(ll_north, "%.lf", window.north);
    sprintf(ll_south, "%.lf", window.south);
    sprintf(ll_east, "%.lf", window.east);
    sprintf(ll_west, "%.lf", window.west);

    ok = 1;
    while (1)
    {
	if (ok == 0)
	{
	    printf("\n\n");
	    if (G_yes("Would you like to start over?", 0))
                return(2);
	}
	ok = 1;

/* List window options on the screen for the user to answer */
	screen = window_screen;

	V_clear();
	line = 0;
	while (*screen)
	    V_line(line++, *screen++);

	V_ques(ll_north, 's', 9, 36, max(11, strlen(ll_north)));
	V_ques(ll_south, 's', 13, 36, max(11, strlen(ll_south)));
	V_ques(ll_west, 's', 12, 12, max(11, strlen(ll_west)));
	V_ques(ll_east, 's', 12, 52, max(11, strlen(ll_east)));

	V_const(ll_def_north, 's', 6, 36, strlen(ll_def_north));
	V_const(ll_def_south, 's', 16, 36, strlen(ll_def_north));
	V_const(ll_def_west, 's', 12, 1, strlen(ll_def_west));
	V_const(ll_def_east, 's', 12, 65, strlen(ll_def_east));

	V_intrpt_ok();
	if (!V_call())
	    return (1);

	G_squeeze(ll_north);
	G_squeeze(ll_south);
	G_squeeze(ll_east);
	G_squeeze(ll_west);

	if (!G_scan_northing(ll_north, &window.north, window.proj))
	{
	    printf("Illegal value for north: %s\n", ll_north);
	    ok = 0;
	}
	if (!G_scan_northing(ll_south, &window.south, window.proj))
	{
	    printf("Illegal value for south: %s\n", ll_south);
	    ok = 0;
	}
	if (!G_scan_easting(ll_east, &window.east, window.proj))
	{
	    printf("Illegal value for east: %s\n", ll_east);
	    ok = 0;
	}
	if (!G_scan_easting(ll_west, &window.west, window.proj))
	{
	    printf("Illegal value for west: %s\n", ll_west);
	    ok = 0;
	}
	if (!ok)
	{
	    hit_return();
	    continue;
	}

/* Adjust and complete the cell header */

	north = window.north;
	south = window.south;
	east = window.east;
	west = window.west;

	tmp_window = &window;

/* this allows us to insist that the resolution select by the user
   is used... thus they can set the project resolution */

	tmp_window->ew_res = proj_resolution;
	tmp_window->ns_res = proj_resolution;

/* if need be, window dimensions will be modified so that
   resolutions, UTM coords, and rows and cols work out evenly,
   without changing resolutions. thus, UTM coords may be rounded
   (larger) to make this so  */

	G_align_window(&window, tmp_window);

SHOW:
	printf("\n\n");
	printf("       Watershed Project Region Information\n\n");
	G_format_northing(window.north, buf, window.proj);
	G_format_northing(north, buf2, window.proj);
	printf("  north: %s", buf);
	if (strcmp(buf, buf2) != 0)
	{
	    ok = 0;
	    printf("  (Changed to match resolution)");
	}
	printf("\n");

	G_format_northing(window.south, buf, window.proj);
	G_format_northing(south, buf2, window.proj);
	printf("  south: %s", buf);
	if (strcmp(buf, buf2) != 0)
	{
	    ok = 0;
	    printf("  (Changed to match resolution)");
	}
	printf("\n");

	G_format_easting(window.east, buf, window.proj);
	G_format_easting(east, buf2, window.proj);
	printf("  east:  %s", buf);
	if (strcmp(buf, buf2) != 0)
	{
	    ok = 0;
	    printf("  (Changed to match resolution)");
	}
	printf("\n");

	G_format_easting(window.west, buf, window.proj);
	G_format_easting(west, buf2, window.proj);
	printf("  west:  %s", buf);
	if (strcmp(buf, buf2) != 0)
	{
	    ok = 0;
	    printf("  (Changed to match resolution)");
	}
	printf("\n");

	printf("  grid cell resolution: %6.f meters\n", proj_resolution);
	printf("  total rows in region: %6d\n", window.rows);
	printf("  total cols in region: %6d\n", window.cols);
	sprintf(buf, "%ld", (long) window.rows * window.cols);
	G_insert_commas(buf);
	printf("  total cells in region:%6s\n", buf);
	printf("  rows in watershed:    %6d\n", rows_in_wshd);
	printf("  cols in watershed:    %6d\n", cols_in_wshd);
/*
	printf("  cells in watershed:   %6d\n", cells_in_wshd);
	sprintf(buf, "%.1f", 
	   (cells_in_wshd * proj_resolution * proj_resolution / 10000));
	G_insert_commas(buf);
	printf("  area of watershed:  %s hectares", buf);
	sprintf(buf, "%.1f", 
	   (cells_in_wshd * proj_resolution * proj_resolution / 4046.856));
	G_insert_commas(buf);
	printf(" (%s acres)\n", buf);
*/
	printf("\n");


	if (window.north > def_wind.north)
	{
	    printf("warning - north falls outside the default region\n");
	    ok = 0;
	}
	if (window.south < def_wind.south)
	{
	    printf("warning - south falls outside the default region\n");
	    ok = 0;
	}
	if (window.proj != PROJECTION_LL)
	{
	    if (window.east > def_wind.east)
	    {
		printf("warning - east falls outside the default region\n");
		ok = 0;
	    }
	    if (window.west < def_wind.west)
	    {
		printf("warning - west falls outside the default region\n");
		ok = 0;
	    }
	}
ASK:
	printf("\nDo you accept this %s? (y/n) [%s] > ", "region", ok ? "y" : "n");
	if (!G_gets(buf))
	    goto SHOW;
	G_strip(buf);
	switch (*buf)
	{
	  case 0:
	    break;
	  case 'y':
	  case 'Y':
	    ok = 1;
	    break;
	  case 'n':
	  case 'N':
	    ok = 0;
	    break;
	  default:
	    goto ASK;
	}
	if (ok)
	{
	    G_clear_screen();
	    printf("\n\nSaving region and grid resolution for project\n\n");
	    fd = G_fopen_new(data_dir, "region");
	    if (!fd)
	        {
		fprintf(stderr, "\n\7WARNING: could not create <region> file in project database.\n");
		hit_return();
		return(1);
	        }
	    G__write_Cell_head(fd, &window, 0);
	    G_set_window(&window);
	    G_put_window(&window);
	    fclose(fd);
	    return (0);
	}
    }
}

static
max(a,b)
{
    return a>b?a:b;
}
 
