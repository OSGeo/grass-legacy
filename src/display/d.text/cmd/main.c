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
#include <unistd.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

int 
main (int argc, char **argv)
{
        char *cmd_ptr ;
        char buff[512] ;
        char window_name[64] ;
        float size ;
        int bold ;
        int color ;
        int cur_dot_row ;
        int dots_per_line ;
        int start_line ;
        int t, b, l, r ;
        int tsize ;
		struct GModule *module;
        struct Option *opt1, *opt2, *opt3;
        char *wind_file_name;
        FILE *wind_file;

		module = G_define_module();
		module->description =
			"Draws text in the active display frame on the graphics monitor.";

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
        opt2->options    = D_color_list();
        opt2->description= "Color desired for drawing text" ;

        opt3 = G_define_option() ;
        opt3->key        = "line" ;
        opt3->required   = NO ;
        opt3->type       = TYPE_INTEGER ;
        opt3->answer     = "1" ;
        opt3->options    = "1-1000" ;
        opt3->description= "The screen line number on which text will begin to be drawn ";


        /* Initialize the GIS calls */
        G_gisinit(argv[0]) ;

        /* Check command line */
        if (G_parser(argc, argv))
                exit(-1);

		if (isatty(0))
			fprintf (stdout,"\nPlease enter text instructions.  Enter EOF on last line to quit\n") ;

        sscanf(opt1->answer,"%f",&size);

        color = D_translate_color(opt2->answer) ;
        if (color == 0)
        {
                fprintf (stdout,"Don't know the color %s\n", opt2->answer) ;
                exit(-1);
        }

        sscanf(opt3->answer,"%d",&start_line);


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

        wind_file_name = G_tempfile();
        if ((wind_file=fopen(wind_file_name,"w")) == NULL)
          G_fatal_error("Unable to open the temporary file.");

        /* Do the plotting */
        while (fgets(buff,512,stdin))
        {
                fprintf(wind_file, "%s\n", buff);
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

        fclose(wind_file);

		D_add_to_list(G_recreate_command()) ;

        R_close_driver();

	exit(0);
}
