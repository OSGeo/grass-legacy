#include "gis.h"
#include "raster.h"
#include "display.h"
#include "Vect.h"
#include "local_proto.h"

static int nlines = 50;
char *panel;

#define WDTH 2

int 
what (struct Map_info *Map, struct Categories *Cats)
{
	int lcat, acat ;
	int row, col;
	int nrows, ncols;
	int t,b,l,r;
	struct Cell_head window;
	int screen_x, screen_y ;
	double east, north ;
	int button ;
	double x1, y1, x2, y2;
	double bbn, bbw, bbs, bbe;
	char buffer[256];
	int color, backcolor;
	int first = 1;
	P_LINE *Line ;
	P_AREA *Area ;
	plus_t line, area ;

	G_get_set_window (&window);
	nrows = window.rows;
	ncols = window.cols;

	screen_x = ((int)D_get_d_west() + (int)D_get_d_east()) / 2 ;
	screen_y = ((int)D_get_d_north() + (int)D_get_d_south()) / 2 ;
	D_get_screen_window(&t, &b, &l, &r);
	panel = G_tempfile() ;
	color = D_translate_color("black");
	backcolor = D_translate_color("white");
	R_text_size(10, 10);
	bbn = 0.; bbw = 0.; bbs = 0.; bbe = 0.;
	fprintf (stderr, "\n");
	fprintf (stderr, "Mouse:\n");
	fprintf (stderr, " Left:  select area\n");
	fprintf (stderr, " Middle: cancel\n");
	fprintf (stderr, " Right: accept region\n");
	do
	{
		R_get_location_with_pointer(&screen_x, &screen_y, &button) ;
		if (! first) R_panel_restore(panel);
		if (button > 1) break;

		bbn = 0.; bbw = 0.; bbs = 0.; bbe = 0.;

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

		nlines++ ;

		if (line + area == 0)
		{
			sprintf(buffer,"Nothing found...") ;
			nlines++ ;
		}

		if (line == 0)
			/* fprintf (stdout,"Line not found.\n") */
			;
		else 
		{
			Line = &(Map->Line[line]);
			if (Line->att)
			{
				V2_get_line_bbox(Map, line, &bbn, &bbs,
							 &bbe, &bbw);
				lcat = Map->Att[Line->att].cat ;
				if (Cats->num > 0)
					sprintf (buffer,"Area: %d=%s", lcat,
					    G_get_cat(lcat, Cats));
				else
					sprintf (buffer,"Line: %d", lcat);
			}
			else
				sprintf (buffer, "Line: ??");
			nlines++ ;
		}

		if (area == 0)
			/* fprintf (stdout,"Area not found.\n")  */
			;
		else 
		{
			Area = &(Map->Area[area]);
			if (Area->att)
			{
				V2_get_area_bbox(Map, area, &bbn, 
							&bbs, &bbe, &bbw);
				acat = Map->Att[Area->att].cat ;
				if (Cats->num > 0)
					sprintf (buffer, "Area: %d=%s", acat,
					    G_get_cat(acat, Cats));
				else
					sprintf (buffer, "Area: %d", acat);
			}
			else
				sprintf (buffer, "Area: ??");

			nlines += 3 ;

		}
		if ( first) first = 0;
		label(t, b, l, r, backcolor, color, buffer, screen_x, screen_y);

	}while (1);
	if (button == 3 && bbn != 0. && bbw != 0. && bbs != 0. && bbe != 0.) 
	{
		window.east = bbe;
		window.north = bbn;
		window.south = bbs;
		window.west = bbw;
		G_put_window(&window);
	}

	return 0;
}

int label (
	int T, int B, int L, int R,
	int backcolor, int textcolor, char *answer,
	int SCREEN_X, int SCREEN_Y)
{
	char achar[2] ;
	int t, b, l, r, row ;

	achar[1] = 000 ;

	R_move_abs(SCREEN_X, SCREEN_Y) ;
	R_get_text_box(answer, &t, &b, &l, &r) ;
	SCREEN_X = SCREEN_X - (r - l)/2 ;
	SCREEN_Y = SCREEN_Y - (b - t)/2 ;
	R_move_abs(SCREEN_X, SCREEN_Y) ;
	R_get_text_box(answer, &t, &b, &l, &r) ;
	t = t - 2 ; if (t < T) t = T ;
	b = b + 2 ; if (b > B) b = B ;
	l = l - 2 ; if (l < L) l = L ;
	r = r + 2 ; if (r > R) r = R ;
		/* Save the panel */
	R_panel_save(panel, t, b, l, r) ;
	R_standard_color(backcolor) ;
	for(row=t; row<=b; row++)
	{
		R_move_abs(l, row) ;
		R_cont_abs(r, row) ;
	}

	/* Draw text */
	R_move_abs(SCREEN_X, SCREEN_Y) ;
	R_standard_color(textcolor) ;
	R_text(answer) ;
	R_flush() ;

	return 0;
}

