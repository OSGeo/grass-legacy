/*  %W%  %G%  */

#include "graphics.h"
#include "gis.h"
#include "options.h"
#include <signal.h>
#define XSCALE	2.0
#define YSCALE	2.0

#define WRITE_STATUS\
    Write_status(hi_mode?red_hi:cur_red,\
		 hi_mode?grn_hi:cur_grn,\
		 hi_mode?blu_hi:cur_blu,\
		 shift_incr, at_cat, hi_mode)

#define WRITE_CATS		Write_cats(categories, at_cat) 

interact(categories,colors) 
	struct Categories *categories ;
	struct Colors *colors ;
{
	char buffer[128] ;
	int at_cat ;
	int hi_mode ;
	int hi_save_mode ;
	int shift_incr ;
	int cur_char ;
	int red_hi ;
	int grn_hi ;
	int blu_hi ;
	int cur_red ;
	int cur_grn ;
	int cur_blu ;

	set_signals() ;
	red_hi = 0 ;
	grn_hi = 0 ;
	blu_hi = 0 ;
	hi_mode = 0 ;
	hi_save_mode = 0 ;

	at_cat = 0 ;
	G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
	shift_incr = 10 ;

	Initialize_curses() ;

	WRITE_CATS ;

	Write_menu() ;

	WRITE_STATUS ;

/*	mark_category(at_cat, 1) ;*/

	while(1) 
	{
		R_flush() ;
		cur_char = getchar() & 0177 ;
		sprintf(buffer,"  %c",cur_char) ;
		Write_message(2, buffer) ;
		switch(cur_char & 0177)
		{
			case '*':
				Replot_screen();
				break;
			case 'Q':
				Close_curses() ;
				return(0) ;
			case 'D': case 'U':
			case 'd': case 'u':
				if (hi_mode && ! hi_save_mode)
				{
					G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
					R_reset_color( (unsigned char)cur_red, (unsigned char)cur_grn, (unsigned char)cur_blu, at_cat) ;
				}
/*				mark_category(at_cat, 0) ;*/
				switch(cur_char & 0177)
				{
					case 'd':
						at_cat++ ;
						break ;
					case 'u':
						at_cat += categories->num ;
						break ;
					case 'D':
						at_cat += 10 ;
						break ;
					case 'U':
						at_cat += categories->num - 9;
						break ;
				}
				at_cat = at_cat % (categories->num + 1) ;

				if (hi_mode)
				{
					G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
					R_reset_color( (unsigned char)red_hi, (unsigned char)grn_hi, (unsigned char)blu_hi, at_cat) ;
					if (hi_save_mode)
						G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
				}
				else
				{
					G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
				}

				WRITE_CATS ;
				WRITE_STATUS ;
/*				mark_category(at_cat, 1) ;*/
				break ;
			case 'r': case 'R': case 'g': case 'G': case 'b': case 'B':
				if (hi_mode)
				{
					G_get_color (at_cat, &red_hi, &grn_hi, &blu_hi, colors) ;
					switch(cur_char & 0177)
					{
					case 'r': red_hi = shift_color(red_hi, -shift_incr) ; break ;
					case 'R': red_hi = shift_color(red_hi,  shift_incr) ; break ;
					case 'g': grn_hi = shift_color(grn_hi, -shift_incr) ; break ;
					case 'G': grn_hi = shift_color(grn_hi,  shift_incr) ; break ;
					case 'b': blu_hi = shift_color(blu_hi, -shift_incr) ; break ;
					case 'B': blu_hi = shift_color(blu_hi,  shift_incr) ; break ;
					}
					G_set_color (at_cat, red_hi, grn_hi, blu_hi, colors) ;
					R_reset_color((unsigned char)red_hi, (unsigned char)grn_hi, (unsigned char)blu_hi, at_cat) ;
				}
				else
				{
					G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
					switch(cur_char & 0177)
					{
					case 'r': cur_red = shift_color(cur_red, -shift_incr) ; break ;
					case 'R': cur_red = shift_color(cur_red,  shift_incr) ; break ;
					case 'g': cur_grn = shift_color(cur_grn, -shift_incr) ; break ;
					case 'G': cur_grn = shift_color(cur_grn,  shift_incr) ; break ;
					case 'b': cur_blu = shift_color(cur_blu, -shift_incr) ; break ;
					case 'B': cur_blu = shift_color(cur_blu,  shift_incr) ; break ;
					}
					G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
					R_reset_color((unsigned char)cur_red, (unsigned char)cur_grn, (unsigned char)cur_blu, at_cat) ;
				}
				WRITE_STATUS ;
				break ;
			case 'i':
				shift_incr = shift_color(shift_incr, -1) ;
				WRITE_STATUS ;
				break ;
			case 'I':
				shift_incr = shift_color(shift_incr, 1) ;
				WRITE_STATUS ;
				break ;
			case '+':
			{
				int red_temp, grn_temp, blu_temp ;
				int incr ;
				G_get_color (colors->min, &red_temp, &grn_temp, &blu_temp, colors) ;
				for (incr=colors->min;  incr<colors->max; incr++)
				{
					G_get_color (incr+1, &cur_red, &cur_grn, &cur_blu, colors) ;
					G_set_color (incr,    cur_red,  cur_grn,  cur_blu, colors) ;
				}
				G_set_color (colors->max, red_temp, grn_temp, blu_temp, colors) ;
				R_reset_colors(colors->min, colors->max,
					colors->red, colors->grn, colors->blu) ;
				WRITE_STATUS ;
			}
				break ;
			case '-':
			{
				int red_temp, grn_temp, blu_temp ;
				int incr ;
				G_get_color (colors->max, &red_temp, &grn_temp, &blu_temp, colors) ;
				for (incr=colors->max;  incr>colors->min; incr--)
				{
					G_get_color (incr-1, &cur_red, &cur_grn, &cur_blu, colors) ;
					G_set_color (incr,    cur_red,  cur_grn,  cur_blu, colors) ;
				}
				G_set_color (colors->min, red_temp, grn_temp, blu_temp, colors) ;
				R_reset_colors(colors->min, colors->max,
					colors->red, colors->grn, colors->blu) ;
				WRITE_STATUS ;
			}
				break ;
			case 'c':        /* Writeout color lookup table */
			{
				Clear_message() ;
				Write_message(2, "Writing color table      ") ;

				if (G_write_colors(map_name, mapset, colors) == -1)
				{
					Write_message(2, "Can't write color table  ") ;
					sleep(2) ;
				}
				Clear_message() ;
			}
				break ;
			case 't':
				Clear_message() ;
				Write_message(1, "toggling new color table...") ;
				table_toggle(map_name, mapset, colors) ;
				Clear_message() ;
				break ;
			case 'h': case 'H':
				if (hi_mode)
				{
					if (hi_save_mode)
						G_set_color (at_cat, cur_red, cur_grn, cur_blu, colors) ;
					G_get_color (at_cat, &cur_red, &cur_grn, &cur_blu, colors) ;
					hi_mode = 0 ;
					hi_save_mode = 0 ;
				}
				else
				{
					cur_red = red_hi ;
					cur_grn = grn_hi ;
					cur_blu = blu_hi ;
					hi_mode = 1 ;
					if (cur_char == 'H')
						hi_save_mode = 1 ;
				}
				R_reset_color((unsigned char)cur_red, (unsigned char)cur_grn, (unsigned char)cur_blu, at_cat) ;
				WRITE_STATUS ;
					
				break ;
			default:

				sprintf(buffer,"  %c - Unknown Command",cur_char) ;
				Write_message(2, buffer) ;
				break ;
		}
	}
}

shift_color(colr, shift)
{
	colr = colr + shift ;
	if (colr <   0) colr =   0 ;
	if (colr > 255) colr = 255 ;
	return(colr) ;
}
