#include "gis.h"
#include "digit.h"

static int nlines = 50;

#define WDTH 2

what(once, Map, Cats)
struct Map_info *Map;
struct Categories *Cats;
{
	int lcat, acat ;
	int row, col;
	int nrows, ncols;
	struct Cell_head window;
	int screen_x, screen_y ;
	double east, north ;
	int button ;
	char east_buf[40], north_buf[40];
	double D_get_d_north(), D_get_d_south() ;
	double D_get_d_east(), D_get_d_west() ;
	double D_d_to_u_row(), D_d_to_u_col() ;
 	extern double G_area_of_polygon();
	double sq_meters;
	double x1, y1, x2, y2;

	P_LINE *Line ;
	P_AREA *Area ;
	plus_t line, area ;
	int i;
	struct line_pnts * Points;
   
	G_get_set_window (&window);
	G_begin_polygon_area_calculations();
	nrows = window.rows;
	ncols = window.cols;

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

	Points = Vect_new_line_struct();

	do
	{
		show_buttons (once);
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		if (button == 3) break;

		east  = D_d_to_u_col((double)screen_x) ;
		north = D_d_to_u_row((double)screen_y) ;

		row = (window.north - north) / window.ns_res ;
		col = (east - window.west) / window.ew_res ;
		if (row < 0 || row >= nrows) continue;
		if (col < 0 || col >= ncols) continue;

		/*  Nov 20, 1992   -dpg   the -1 allows ALL lines 
		**     including deleted lines  Also switch to point_by_line
		line = dig_point_to_line (Map, east, north, -1);
		*/

		x1 = D_d_to_u_col ((double)(screen_x-WDTH));
		y1 = D_d_to_u_row ((double)(screen_y-WDTH));
		x2 = D_d_to_u_col ((double)(screen_x+WDTH));
		y2 = D_d_to_u_row ((double)(screen_y+WDTH));

		line = dig_point_by_line (Map, x1, y1, x2, y2, LINE|AREA|DOT);

		area = dig_point_to_area (Map, east, north) ;

/*		printf("\nUTM  - %9.2f %10.2f\n", east, north);*/
                G_format_easting(east, east_buf, G_projection());
                G_format_northing(north, north_buf, G_projection());
		printf("\n       %s  %s\n", east_buf, north_buf);
		nlines++ ;

		if (line + area == 0)
		{
			printf("Nothing Found.\n") ;
			nlines++ ;
		}

		if (line == 0)
			/* printf ("Line not found.\n") */
			;
		else 
		{
			Line = &(Map->Line[line]);
			if (Line->att)
			{
				lcat = Map->Att[Line->att].cat ;
				if (Cats->num > 0)
					printf ("Line - Category %d %s\n", lcat,
					    G_get_cat(lcat, Cats));
				else
					printf ("Line - Category %d <not labeled>\n", lcat);
			}
			else
				printf ("Line - Category <not tagged>\n");
			nlines++ ;
		}

		if (area == 0)
			/* printf ("Area not found.\n")  */
			;
		else 
		{
			Area = &(Map->Area[area]);
			if (Area->att)
			{
				acat = Map->Att[Area->att].cat ;
				if (Cats->num > 0)
					printf ("Area - Category %d %s\n", acat,
					    G_get_cat(acat, Cats));
				else
					printf ("Area - Category %d <not labeled>\n", acat);
			}
			else
				printf ("Area - Category <not tagged>\n");

			/* Area stats - just for grins */
			/* dig_find_area2(Map, Area, &sq_meters); */
			Vect_get_area_points(Map, area, Points);
			sq_meters = 
			     G_area_of_polygon(Points->x, Points->y, Points->n_points);
                        /* substructing island areas */
                        for(i = 0;i<Area->n_isles;i++)
			{
			   Vect_get_isle_points(Map, Area->isles[i], Points);
			   sq_meters = sq_meters -
			      G_area_of_polygon(Points->x, Points->y, Points->n_points);
			}

			printf("Size - Sq Meters: %.3lf\t\tHectares: %.3lf\n",
			    sq_meters, (sq_meters/10000.) );

			printf("           Acres: %.3lf\t\tSq Miles: %.4lf\n",
			    ((sq_meters*10.763649)/43560.),((sq_meters*10.763649)/43560.)/640. );
			nlines += 3 ;

		}

	}while (!once);
	Vect_destroy_line_struct(Points);
}

/* TODO */
show_buttons (once)
{
	if (once)
	{
		fprintf (stderr, "\nClick mouse button on desired location\n\n");
		nlines = 3;
	}
	else if (nlines >= 18)      /* display prompt every screen full */
	{
		fprintf (stderr, "\n");
		fprintf (stderr, "Buttons\n");
		fprintf (stderr, " Left:  what's here\n");
		fprintf (stderr, " Right: quit\n");
		nlines = 4;
	}
}
