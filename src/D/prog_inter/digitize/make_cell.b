/*  %W%  %G%  */

#include "gis.h"
#include "graphics.h"
#include "circle.h"
#include <stdio.h>

static double D_north, D_west ;
static double U_to_D_xconv, U_to_D_yconv ;
static double U_west, U_south, U_north ;

make_cell()
{
	FILE *mapfile ;
	char *tempname ;
	char buffer[512] ;
	char newname[20] ;
	double first_ux, first_uy ;
	double D_get_d_north(), D_get_d_west() ;
	double D_get_u_to_d_xconv(), D_get_u_to_d_yconv() ;
	double D_get_u_west(), D_get_u_north(), D_get_u_south() ;
	double get_resolution() ;
	double hypot() ;
	double ux, uy ;
	double x, y ;
	int button ;
	int category ;
	int cur_screen_x, cur_screen_y ;
	int first_screen_x, first_screen_y ;
	int max_cat ;
	int not_ok ;
	int n_coors ;
	int screen_x, screen_y ;
	int i ;
	int type ;
	int radius ;
	int not_sure ;
	static int something_done ;
	struct table *table ;
	struct Cell_head universe ;

	screen_y  = get_map_top() ;
	screen_x = get_map_left() ;
	something_done = 0 ;
	max_cat = 0 ;

/* open new map file */
	tempname = G_tempfile() ;
	if ( (mapfile = fopen(tempname, "w")) == NULL)
	{
		G_clear_screen() ;
		fprintf(stderr, "Sorry, can't open temp file.\n") ;
		sleep(3) ;
		return(0) ;
	}

/* Get necessary conversion factors */
	D_north = D_get_d_north() ;
	D_west = D_get_d_west() ;
	U_to_D_xconv = D_get_u_to_d_xconv() ;
	U_to_D_yconv = D_get_u_to_d_yconv() ;
	U_west = D_get_u_west() ;
	U_north = D_get_u_north() ;
	U_south = D_get_u_south() ;

/* Get Universe window - currently poly to cell capabilities work only with 
   full universe windows.  In the future when this restriction is lifted, 
   the SIZE and BOUND values can key off a smaller area
 */
	G_get_set_window(&universe) ;

/* Print poly (grip) header information */
	print_header(mapfile, &universe) ;

	for(;;)
	{
		type = next_type() ;
		switch (type & 0177)
		{
		case 'E':
			fprintf(mapfile,"E\n") ;
			fclose(mapfile) ;
			return(0) ;

		case 'D':
			fprintf(mapfile,"E\n") ;
			fclose(mapfile) ;
			if (! something_done)
				return(0) ;
			break ;

		case 'A':
			something_done = 1 ;
			category = get_category() ;
			G_clear_screen() ;
			fprintf(stderr, "Mark first point\n") ;
			if (get_point(0, 0, &first_screen_x, &first_screen_y, 
				&first_ux, &first_uy) )
			{
				type = 'Z' ;
				break ;
			}
			fprintf(mapfile,"A %10.2f %10.2f %10d\n",
				first_uy, first_ux, category) ;
			break ;

		case 'L':
			something_done = 1 ;
			category = get_category() ;
			G_clear_screen() ;
			fprintf(stderr, "Mark first point\n") ;
			if (get_point(0, 0, &first_screen_x, &first_screen_y, 
				&first_ux, &first_uy) )
			{
				type = 'Z' ;
				break ;
			}
			fprintf(mapfile,"L %10.2f %10.2f %10d\n",
				first_uy, first_ux, category) ;
			break ;

		case 'C':
			something_done = 1 ;
			category = get_category() ;

			G_clear_screen() ;
			fprintf(stderr, "Mark circle center\n") ;
			if (get_point(0, 0, &first_screen_x, &first_screen_y, 
				&first_ux, &first_uy) )
			{
				type = 'Z' ;
				break ;
			}

			G_clear_screen() ;
			fprintf(stderr, "Mark a point on perimeter\n") ;
			screen_x = first_screen_x + 10 ;
			screen_y = first_screen_y + 10 ;
			if (get_point(first_screen_x, first_screen_y,
				&screen_x, &screen_y, &ux, &uy) )
			{
				type = 'Z' ;
				break ;
			}

		/* Get circle coordinates */
			radius = (int) (hypot( (double)(screen_x - first_screen_x),
				(double)(screen_y - first_screen_y)) + .5) ;
			Circledef(radius, &table, &n_coors) ;

		/* Plot and save coordinates */
			cur_screen_x = first_screen_x + table[0].col ;
			cur_screen_y = first_screen_y + table[0].row ;
			ux = ((double)cur_screen_x - D_west) / U_to_D_xconv + U_west ;
			uy = U_north - ((double)cur_screen_y - D_north)/ U_to_D_yconv ;
			fprintf(mapfile,"A %10.2f %10.2f %10d\n", uy, ux, category) ;
			for (i=1; i<n_coors; i++)
			{
				screen_x = first_screen_x + table[i].col ;
				screen_y = first_screen_y + table[i].row ;
				ux = ((double)screen_x - D_west) / U_to_D_xconv + U_west ;
				uy = U_north - ((double)screen_y - D_north)/ U_to_D_yconv ;
				fprintf(mapfile,"  %10.2f %10.2f\n", uy, ux) ;
				black_and_white_line(screen_x,screen_y,cur_screen_x,cur_screen_y) ;
				cur_screen_x = screen_x ;
				cur_screen_y = screen_y ;
			}
			free (table);
			break ;
		}

		if (type == 'D')
			break ;

		if (type == 'A' || type == 'L')
		{
			G_clear_screen() ;
			fprintf(stderr, "Buttons:\n") ;
			fprintf(stderr, "Left:   where am i\n") ;
			fprintf(stderr, "Middle: Mark next point\n") ;
			fprintf(stderr, "Right:  Mark LAST point\n") ;

			cur_screen_x = screen_x = first_screen_x ;
			cur_screen_y = screen_y = first_screen_y ;

			do
			{
				/*
				screen_x = cur_screen_x ;
				screen_y = cur_screen_y ;
				*/
				R_get_location_with_line(cur_screen_x, cur_screen_y, &screen_x, &screen_y, &button) ;
				ux = ((double)screen_x - D_west) / U_to_D_xconv + U_west ;
				uy = U_north - ((double)screen_y - D_north)/ U_to_D_yconv ;
				fprintf(stderr,"EAST: %10.2f\n", ux) ;
				fprintf(stderr,"NORTH: %10.2f\n", uy) ;
				if(button == 2)
				{
				/* printout next point */
					fprintf(mapfile,"  %10.2f %10.2f\n", uy, ux) ;
				/* Draw line on screen */
					black_and_white_line(screen_x,screen_y,cur_screen_x,cur_screen_y) ;
					cur_screen_x = screen_x ;
					cur_screen_y = screen_y ;
					R_move_abs(cur_screen_x, cur_screen_y) ;
				}
			} while (button != 3) ;
		}

		if (type == 'A') 
		{
		/* Complete area by drawing to first point */
			fprintf(mapfile,"  %10.2f %10.2f\n", first_uy, first_ux) ;
		/* Draw line on screen */
			black_and_white_line(first_screen_x,first_screen_y,
				cur_screen_x, cur_screen_y) ;
		}
		
		if (max_cat < category)
			max_cat = category ;
	}


/* Get name for new file */
	G_clear_screen() ;
	not_sure = 1 ;
	while (not_sure)
	{
		if (! G_ask_cell_new("", newname))
		{
			printf("\n Are you sure? (y/n) >") ;
			gets(buffer) ;
			if (buffer[0] == 'y')
				return(-1) ;
		}
		else
			not_sure = 0 ;
	}

	G_get_set_window(&universe) ;
	G_put_cellhd(newname, &universe) ;

/* Run the polytocell capabilities on this poly file */
	sprintf(buffer, "%s/etc/poly_to_bmif < %s | sort |  %s/etc/bmif_to_cell %s",
		G_gisbase(), tempname, G_gisbase(), newname ) ;

	fprintf(stderr, "Generating cell file\n") ;
	system(buffer) ;

	fprintf(stderr, "Generating support files\n") ;
	make_support(newname, G_mapset(), max_cat) ;

	fprintf(stderr, "Done\n") ;
	sleep(2) ;
}

static
print_header(mapfile, universe) 
	FILE *mapfile ;
	struct Cell_head *universe ;
{
	fprintf(mapfile,"TITLE:\n") ;
	fprintf(mapfile,"	User created mask\n") ;
	fprintf(mapfile,"ENDT\n") ;
	fprintf(mapfile,"SIZE      %10d %10d\n",
		universe->rows, universe->cols) ;
	fprintf(mapfile,"BOUND     %10.2f %10.2f %10.2f %10.2f\n",
		universe->ns_res, universe->ew_res,
		universe->south, universe->west) ;
	fprintf(mapfile,"VERTI\n") ;
}

static
next_type()
{
	int not_ok ;
	char buffer[64] ;

	/* Determine if the new line will be a line or an area boundary */
	for (;;)
	{
		G_clear_screen() ;
		fprintf(stderr, "Will the next points define\n") ;
		fprintf(stderr, "a Line,  Area, or Circle?\n") ;
		fprintf(stderr, "Enter 'Done' if done.\n") ;
		fprintf(stderr, "Enter 'Exit' to quit.\n") ;
		fprintf(stderr, " > ") ;
		gets(buffer) ;
		switch (buffer[0] & 0177)
		{
			case 'l': case 'L':
				return ('L') ;
				break ;
			case 'a': case 'A':
				return ('A') ;
				break ;
			case 'c': case 'C':
				return ('C') ;
				break ;
			case 'd': case 'D':
				return ('D') ;
			case 'e': case 'E':
				return ('E') ;
				break ;
			default:
				break ;
		}
	}
}

static
get_category()
{
	char buffer[64] ;
	int category ;

	for(;;)
	{
		G_clear_screen() ;
		fprintf(stderr, "Enter the category number\n") ;
		fprintf(stderr, " > ") ;
		gets(buffer) ;
		sscanf(buffer,"%d", &category) ;

		if (category >= 0 && category <= 240)
			return (category) ;
		
		fprintf(stderr, "Must be >= 0 and <= 240\n") ;
		sleep(2) ;
	}
}

static
get_point(csx, csy, screen_x, screen_y, ux, uy)
	int csx ;
	int csy ;
	int *screen_x ;
	int *screen_y ;
	double *ux ;
	double *uy ;
{
	char buffer[64] ;
	int button ;

	show_instructions_1() ;

	do
	{
		if (csx && csy)
			R_get_location_with_line(csx, csy, screen_x, screen_y, &button) ;
		else
			R_get_location_with_pointer(screen_x, screen_y, &button) ;
		
		if (button == 3)
			return(-1) ;

		*ux = ((double)*screen_x - D_west) / U_to_D_xconv + U_west ;
		*uy = U_north - ((double)*screen_y - D_north)/ U_to_D_yconv ;
		fprintf(stderr, "EAST:  %10.2f\n", *ux) ;
		fprintf(stderr, "NORTH: %10.2f\n", *uy) ;
	} while (button != 2) ;

	return(0) ;
}

static
make_support(name, mapset, num)
	char *name ;
	char *mapset ;
	int num ;
{
	struct Categories cats ;
	struct Colors colr ;
	struct Cell_head cellhd, wind ;
	struct History hist ;
	int i ;
	char buff[128] ;

/* Do categories */
	sprintf(buff,"Screen digit; %s; %s", G_whoami(), G_date()) ;
	G_init_cats (num, buff, &cats) ;

	for (i=0; i<=num; i++)
	{
		sprintf(buff, "Category %d", i) ;
		G_set_cat (i, buff, &cats) ;
	}

	G_write_cats(name, &cats) ;

/* Do cellhd.  It exists at this point, but has 0,0 as its SW corner
 * and has the cell resolution in both directions set to 1.0.
	G_get_set_window(&wind) ;
	G_get_cellhd(name, mapset, &cellhd) ;
	cellhd.ew_res = wind.ew_res ;
	cellhd.ns_res = wind.ns_res ;
	cellhd.south =  wind.south  ;
	cellhd.west  =  wind.west   ;
	cellhd.north =  cellhd.south + (double)cellhd.rows * cellhd.ns_res ;
	cellhd.east  =  cellhd.west  + (double)cellhd.cols * cellhd.ew_res ;
	G_put_cellhd(name, &cellhd) ;
 */

/* Do color */
	G_make_random_colors(&colr, 0, num) ;
	G_write_colors(name, mapset, &colr) ;

/* Do history */
	G_short_history(name, "cell", &hist);
	G_write_history (name, &hist) ;
}
