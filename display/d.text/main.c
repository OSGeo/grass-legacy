/*
* $Id$
*
****************************************************************************
*
* MODULE:       d.text
*
* AUTHOR(S):    James Westervelt, US Army CERL
*
* PURPOSE:      display text in active frame
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*
 *   d.text
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
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

int 
main (int argc, char **argv)
{
        char *cmd_ptr, *max_buff ;
        char buff[512] ;
        char window_name[64] ;
        float size ;
        int bold ;
        int color, R, G, B ;
        int cur_dot_row, cur_dot_column ;
        int dots_per_line ;
        int start_line ;
        int t, b, l, r ;
        int tsize ;
	double x,y ;

	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4;
	struct Flag *flag_b;
	char *wind_file_name;
	FILE *wind_file;

        /* Initialize the GIS calls */
        G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
	   "Draws text in the active display frame on the graphics monitor using the current font.";

        opt1 = G_define_option() ;
        opt1->key        = "size" ;
        opt1->type       = TYPE_DOUBLE ;
        opt1->required   = NO ;
        opt1->answer      = "5" ;
        opt1->options    = "0-100";
        opt1->description="Height of letters in percent of available frame height";

        opt2 = G_define_option() ;
        opt2->key        = "color" ;
        opt2->type       = TYPE_STRING ;
        opt2->answer     = "gray" ;
        opt2->required   = NO ;
        opt2->description=
	   "Text color, either a standard GRASS color or R:G:B triplet (separated by colons)" ;

        opt3 = G_define_option() ;
        opt3->key        = "line" ;
        opt3->required   = NO ;
        opt3->type       = TYPE_INTEGER ;
        opt3->options    = "1-1000" ;
        opt3->description= "The screen line number on which text will begin to be drawn";

	opt4 = G_define_option() ;
	opt4->key	= "at" ;
	opt4->key_desc  = "x,y";
	opt4->type	= TYPE_DOUBLE ;
	opt4->options	= "0-100";
	opt4->required	= NO ;
	opt4->description=
	   "Screen postion at which text will begin to be drawn (percentage, [0,0] is lower left)";

	flag_b = G_define_flag();
	flag_b->key         = 'b';
	flag_b->description = "Use bold text";


        /* Check command line */
        if (G_parser(argc, argv))
                exit(1);

	if(opt3->answer && opt4->answer)
		G_fatal_error("Please choose only one placement method");


	if (isatty(0))
		fprintf (stdout,"\nPlease enter text instructions.  Enter EOF (ctrl-d) on last line to quit\n") ;

        sscanf(opt1->answer,"%f",&size);

	/* Parse and select text color */
	if(sscanf(opt2->answer, "%d:%d:%d", &R, &G, &B) == 3) {
		if (R>=0 && R<256 && G>=0 && G<256 && B>=0 && B<256) {
			color = 1;
			R_reset_color(R, G, B, color);
			R_color(color);
		}
	}
	else {
		color = D_translate_color(opt2->answer);
		R_standard_color(color);
	}

	if(!color)
		G_fatal_error("[%s]: No such color", opt2->answer);


	/* Open monitor window */
        if (R_open_driver() != 0)
		G_fatal_error ("No graphics device selected");

        if (D_get_cur_wind(window_name))
                G_fatal_error("No current window") ;

        if (D_set_cur_wind(window_name))
                G_fatal_error("Current window not available") ;

        /* Figure out where to put text */
        D_get_screen_window(&t, &b, &l, &r) ;
        R_set_window(t, b, l, r) ;

        dots_per_line = (int)(size/100.0 * (float)(b - t)) ;
        tsize = (int)(.8 * (float)dots_per_line) ;

	if(!opt4->answer) {
		if(opt3->answer)
			sscanf(opt3->answer,"%d",&start_line);
		else start_line = 1;

        	cur_dot_row = t + (start_line - 1) * dots_per_line;
		cur_dot_column = l+5;
	}
	else {
		x = atof(opt4->answers[0]);
		y = atof(opt4->answers[1]);
		if(x<0 || x>100 || y<0 || y>100)
			G_fatal_error("value [%.0f,%.0f] out of range [0-100]", x, y);

		cur_dot_row = t+(int)((b-t)*(100.-y)/100.);
		cur_dot_column = l+(int)((r-l)*x/100.);
	}
	
        R_text_size(tsize, tsize) ;

	if(flag_b->answer)
		bold = 1;
	else
		bold = 0 ;

        wind_file_name = G_tempfile();
        if ((wind_file=fopen(wind_file_name,"w")) == NULL)
          G_fatal_error("Unable to open the temporary file.");

        /* Do the plotting */
        while (fgets(buff,512,stdin))
        {
                fprintf(wind_file, "%s", buff);
                if (*buff == '.')
                {
                        for(cmd_ptr=buff+2; *cmd_ptr==' '; cmd_ptr++) ;
                        G_squeeze(cmd_ptr); /* added 6/91 DBS @ CWU */
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
                        if(!opt4->answer)
				cur_dot_row += dots_per_line ;
                        R_move_abs(cur_dot_column, cur_dot_row) ;
                        R_text(buff) ;
                        if (bold)
                        {
                                R_move_abs(cur_dot_column, 1 + cur_dot_row) ;
                                R_text(buff) ;
                                R_move_abs(cur_dot_column + 1, cur_dot_row) ;
                                R_text(buff) ;
                        }
                }
        }

        R_text_size(5, 5) ;

	fclose(wind_file);
	
	max_buff = G_malloc(strlen(wind_file_name)+strlen(G_recreate_command())+4);
	sprintf(max_buff, "%s < %s", G_recreate_command(), wind_file_name);
	D_add_to_list(max_buff);
	G_free(max_buff);

        R_close_driver();

	exit(0);
}
