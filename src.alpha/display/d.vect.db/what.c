#include "gis.h"
#include "digit.h"

static int nlines = 50;

#define WDTH 2
#define LINE_TYPE 0
#define AREA_TYPE 1

int what(Map, Cats, color, hilite_color, type)
struct Map_info *Map;
struct Categories *Cats;
int color, hilite_color, type;
{
	int lcat, acat, chosen_cat;
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
   
	G_get_set_window (&window);
	nrows = window.rows;
	ncols = window.cols;

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;

        lcat = acat = chosen_cat = 0;
	do
	{
		show_buttons ();
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		if (button == 3) break;

                line = area = 0;
		lcat = acat = chosen_cat = 0;

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

                if(type==LINE_TYPE)
		line = dig_point_by_line (Map, x1, y1, x2, y2, LINE_TYPE|AREA_TYPE|DOT);

                if(type==AREA_TYPE)
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

		if(type==LINE_TYPE && line!=0) 
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
			chosen_cat = lcat;
			nlines++ ;
		}

                if(type==AREA_TYPE && area!=0)
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
			chosen_cat = acat;
			nlines++ ;

		}
                hilite(Map, chosen_cat, color, hilite_color);

	}while (1);
	/* LATER ON DECIDE WHICH to use lcat, acat or also let choose site */
	return chosen_cat;
}

/* TODO */
show_buttons ()
{
	if (nlines >= 18)      /* display prompt every screen full */
	{
		fprintf (stderr, "\n");
		fprintf (stderr, "Buttons\n");
		fprintf (stderr, " Left:  what's here\n");
		fprintf (stderr, " Right: select\n");
		nlines = 4;
	}
}
