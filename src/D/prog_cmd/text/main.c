/* @(#)main.c	2.1   6/26/87 */

/*
 *   Dtext
 *
 *   Usage:  Dtext [color] [size] [beg_line]
 *           Dtext [color=name] [size=num] [line=num]
 *
 *   Draw text in a text window.   Text lines come from stdin.
 *   R_text control:
 *      .C color_name        change color
 *      .S size              change text size
 *      .B 0                 bold (double print) off
 *      .B 1                 bold (double print) on
 *      .F name              change font to name
 */

#include <stdio.h>
#define MAIN
#include "options.h"
#define USAGE	"[color=name] [size=num] [line=num]"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char *gets() ;
	char window_name[64] ;
	char *cmd_ptr ;
	int i ;
	int tsize ;
	int t, b, l, r ;
	int cur_dot_row ;
	int dots_per_line ;
	int bold ;
	extern int stash_away() ;

/* Initialize the GIS calls */
	G_gisinit("Dtext") ;

/* Check command line */
	set_default_options() ;

	if (D_parse_command(argc, argv, variables, n_variables, stash_away))
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		exit(-1) ;
	}

/* */
	R_open_driver();

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

	dots_per_line = (int)(size/100.0 * (float)(b - t)) ;
	tsize = (int)(.8 * (float)dots_per_line) ;
	cur_dot_row = t + (start_line - 1) * dots_per_line;
	R_text_size(tsize, tsize) ;
	R_standard_color(color) ;
	bold = 0 ;

/* Do the plotting */
	while (gets(buff))
	{
		if (*buff == '.')
		{
			for(cmd_ptr=buff+2; *cmd_ptr==' '; cmd_ptr++) ;
			switch (buff[1] & 0x7F)
			{
			case 'F':   /* font */
				R_font(cmd_ptr) ;
				break ;
			case 'C':   /* color */
				if (color = D_translate_color(cmd_ptr))
					R_standard_color(color) ;
				break ;
			case 'S':   /* size */
				if (sscanf(cmd_ptr, "%f", &size))
				{
					dots_per_line = (int)(size/100.0 * (float)(b - t)) ;
					tsize = (int)(.8 * (float)dots_per_line) ;
					R_text_size(tsize, tsize) ;
				}
				break ;
			case 'B':   /* bold */
				if (! sscanf(cmd_ptr, "%d", &bold))
					bold = 0 ;
				break ;
			default:
				break ;
			}
		}
		else
		{
			cur_dot_row += dots_per_line ;
			R_move_abs(l+5, cur_dot_row) ;
			R_text(buff) ;
			if (bold)
			{
			    R_move_abs(5 + l, 1 + cur_dot_row) ;
			    R_text(buff) ;
			    R_move_abs(6 + l, cur_dot_row) ;
			    R_text(buff) ;
			}
		}
	}

	R_text_size(5, 5) ;

/* Add this command to list */
	strcpy(buff, argv[0]) ;
	for(i=1; i<argc; i++)
	{
		strcat(buff, " ") ;
		strcat(buff, argv[i]) ;
	}
	D_add_to_list(buff) ;

    R_close_driver();
}
