/* This version of interact.c was written by Paul Carlson specifically for 
 * the AT&T 3B2 and  PC6300.		8/88
 */

#include <math.h>
#include "graphics.h"
#include "gis.h"
#include "options.h"
#include "windows.h"
#include "variables.h"
#include <signal.h>
#define XSCALE	2.0
#define YSCALE	2.0

#define WRITE_STATUS \
	Write_status(cur_red, cur_grn, cur_blu, shift_incr, at_cat, hi_mode)

#define WRITE_CATS		Write_cats(categories, at_cat) 

static int lines;
static int cur_dot_row;
static int cur_dot_col;
static int dots_per_line;
static int dots_per_col;
static int white;
static int black;
static int t, b, l, r;

interact(categories,colors) 
struct Categories *categories;
struct Colors *colors;
{
	char buffer[128];
	char window_name[64];
	int at_cat;
	int hi_mode;
	int shift_incr;
	int cur_char;
	int cur_red;
	int cur_grn;
	int cur_blu;
	int colors_changed;
	int atcol;
	int atline;
	int cols;
	int cats;
	int tog;

	set_signals();
	hi_mode = 0;

	Dchoose(LEG.name);
	D_get_screen_window(&t, &b, &l, &r);
	white = D_translate_color("white");
	black = D_translate_color("black");
	cats = categories->num + 1;
	cols = 1 + sqrt((double)(cats) / ((double)(b-t) / (double)(r-l)));
	lines = 1 + cats / cols;
	dots_per_line = (b-t) / lines;
	dots_per_col  = (r-l) / cols;

new_screen:
	tog = 0;
	R_standard_color(black);
	R_box_abs(l, t, r, b);
	at_cat = 0;
	G_get_color(at_cat, &cur_red, &cur_grn, &cur_blu, colors);
	R_reset_color((unsigned char)cur_red, (unsigned char)cur_grn, 
			(unsigned char)cur_blu, at_cat);
	G_set_color(at_cat, cur_red, cur_grn, cur_blu, colors);
	for(atcol = 0; atcol < cols; atcol++)
	{	cur_dot_row = t;
		cur_dot_col = l + atcol * dots_per_col;
		for(atline = 0; atline < lines; atline++)
		{	cur_dot_row += dots_per_line;
/*----------- modified bu P.W.C.  6/7/89 ---------------------*/
			/*
			R_color(at_cat);
			*/
			if (at_cat > 0 && at_cat < colors->cmin) R_color(0);
			else R_color(at_cat);
/*------------------------------------------------------------*/
			R_box_abs(cur_dot_col+4, cur_dot_row-dots_per_line+3, 
				cur_dot_col+dots_per_col-2, cur_dot_row-3);
			if(++at_cat == cats) break;
		}
		if(at_cat == cats) break;
	}
	at_cat = 0;
	boxit(at_cat, white);

	colors_changed = 0;
	at_cat = 0;
	G_get_color(at_cat, &cur_red, &cur_grn, &cur_blu, colors);
	shift_incr = 63;
	Initialize_curses();
	WRITE_CATS;
	Write_menu();
	WRITE_STATUS;

	while(1) 
	{	if (tog) break;
		R_flush();
		cur_char = getchar() & 0177;
		sprintf(buffer,"  %c",cur_char);
		switch (cur_char)
		{	case '*':
				Replot_screen();
				break;

			case 'Q':
				if (!colors_changed 
				|| yes("Colors changed, NOT SAVED",
					"Quit (y/n) ?"))
				{	Close_curses();
					return(0);
				}
				break;

			case 'D': case 'U':
			case 'd': case 'u':
				switch(cur_char)
				{	case 'd':
						boxit(at_cat, black);
						at_cat++;
						if (at_cat == cats)
							at_cat = 0;
						boxit(at_cat, white);
						break;

					case 'u':
						boxit(at_cat, black);
						at_cat--;
						if (at_cat < 0)
							at_cat = cats - 1; 
						boxit(at_cat, white);
						break;

					case 'D':
						boxit(at_cat, black);
						at_cat += 10;
						if (at_cat >= cats)
							at_cat = 0;
						boxit(at_cat, white);
						break;

					case 'U':
						boxit(at_cat, black);
						at_cat -= 10;
						if (at_cat < 0)
							at_cat = cats - 1; 
						boxit(at_cat, white);
						break;
				}
				G_get_color(at_cat, &cur_red, &cur_grn, 
					&cur_blu, colors);
				WRITE_CATS;
				WRITE_STATUS;
				break;

			case 'r': case 'R': 
			case 'g': case 'G': 
			case 'b': case 'B':
			    if (at_cat == 0 || at_cat >= colors->cmin)
			    {
				G_get_color(at_cat, &cur_red, &cur_grn, 
					&cur_blu, colors);
				switch (cur_char)
				{	case 'r': cur_red = shift_color(cur_red,
						-shift_incr); 
						break;

					case 'R': cur_red = shift_color(cur_red,
						 shift_incr); 
						break;

					case 'g': cur_grn = shift_color(cur_grn,
						-shift_incr); 
						break;

					case 'G': cur_grn = shift_color(cur_grn,
						 shift_incr); 
						break;

					case 'b': cur_blu = shift_color(cur_blu,
						-shift_incr); 
						break;

					case 'B': cur_blu = shift_color(cur_blu,
						 shift_incr); 
						break;
				}
				R_reset_color((unsigned char)cur_red, 
					(unsigned char)cur_grn, 
					(unsigned char)cur_blu, at_cat);
				G_set_color(at_cat, cur_red, cur_grn, 
					cur_blu, colors);
				blockit(at_cat);
				colors_changed = 1;
				WRITE_STATUS;
			    }
			    break;

			case 'i':
				shift_incr = shift_color(shift_incr, -1);
				WRITE_STATUS;
				break;

			case 'I':
				shift_incr = shift_color(shift_incr, 1);
				WRITE_STATUS;
				break;

			case 'c':       
				colors_changed = 0;
				Clear_message();
				Write_message(2, "Writing color table      ");
				if (G_write_colors(map_name, mapset, colors) 
					== -1)
				{	sleep(1);
					Write_message(2, 
						"Can't write color table  ");
					sleep(2);
				}
				Clear_message();
				break;

			case 't':
				Clear_message();
				Write_message(2,"toggling new color table...");
				table_toggle(map_name, mapset, colors);
				Clear_message();
			 	colors_changed = 1;
				tog = 1;
				Close_curses();
				break;
			case 'z':
				Clear_message();
				Write_message(2, "   Replotting map...");
				map_replot(colors);
				Clear_message();
				break;
			default:
				sprintf(buffer,
					"  %c - Unknown Command",cur_char);
				Write_message(2, buffer);
				sleep(2);
				Clear_message();
				break;
		}
	}
	if (tog) goto new_screen;
}


static boxit(cat, colr)
int cat, colr;
{
	cur_dot_col = l + (cat / lines) * dots_per_col;
	cur_dot_row = t + ((cat % lines) + 1) * dots_per_line;

	R_standard_color(colr);

/*----------- modified bu P.W.C.  6/7/89 ---------------------*/
	/*
	R_move_abs(cur_dot_col+2, (cur_dot_row-1));
	R_cont_rel(0, (2-dots_per_line));
	R_cont_rel((dots_per_col-2), 0);
	R_cont_rel(0, (dots_per_line-2));
	R_cont_rel((2-dots_per_col), 0);
	*/
	R_move_abs(cur_dot_col+1, cur_dot_row);
	R_cont_rel(0, -dots_per_line);
	R_cont_rel(dots_per_col, 0);
	R_cont_rel(0, dots_per_line);
	R_cont_rel(-dots_per_col, 0);
/*------------------------------------------------------------*/
}


static blockit(cat)
int cat;
{
	cur_dot_col = l + (cat / lines) * dots_per_col;
	cur_dot_row = t + ((cat % lines) + 1) * dots_per_line;
	R_color(cat);
	R_box_abs(cur_dot_col+4, cur_dot_row-dots_per_line+3, 
		cur_dot_col+dots_per_col-2, cur_dot_row-3);
}


map_replot(colors)
struct Colors *colors;
{
	struct Cell_head wind;
	char window_name[64];
	char buff[128];
	int offset;
	int cellfile;
	CELL *xarray;
	int cur_A_row;
	int t, b, l, r;

	G_get_window(&wind);
	G_set_window(&wind);
	Dchoose(MAP.name);
	Derase(D_translate_color("black"));
	D_get_cur_wind(window_name);
	D_set_cur_wind(window_name);
	G_get_set_window(&wind); 
	D_offset_is(&offset);
	R_color_offset(offset);
/*	D_reset_colors (colors); NOT in 4.0 */
	D_set_colors (colors);
	G_free_colors (colors);
	D_get_screen_window(&t, &b, &l, &r);
	D_cell_draw_setup(t, b, l, r);
	cellfile = G_open_cell_old(map_name, mapset);
	xarray = G_allocate_cell_buf();
	for (cur_A_row = 0; cur_A_row != -1; )
	{	G_get_map_row(cellfile, xarray, cur_A_row); 
/*		cur_A_row = D_draw_cell_row(cur_A_row, xarray); NOT in 4.0 */
		cur_A_row = D_draw_cell(cur_A_row, xarray, colors);
	}
	R_flush();
	G_close_cell(cellfile);
	free (xarray);
}



shift_color(colr, shift)
{
	colr = colr + shift;
	if (colr <   0) colr =   0;
	if (colr > 255) colr = 255;
	return(colr);
}

static yes(msg1, msg2)
char *msg1, *msg2;
{
	int c;

	Clear_message();
	Write_message(2, msg1);
	Write_message(3, msg2);

	while (1)
	{	c = getchar() & 0177;
		switch (c)
		{
			case 'y':
			case 'Y':
				Clear_message();
				return 1;

			case 'n':
			case 'N':
				Clear_message();
				return 0;
		}
		putchar ('\7');
	}
}

