/*
 *   d.label
 *
 *   Draw interactive labels in a text window.
 */

#include <stdio.h>
#include "gis.h"
#include "display.h"
#include "raster.h"

int label(int, int, int, int, int, int, int);

int 
main (int argc, char **argv)
{
	char window_name[64] ;
	float size ;
	int backcolor ;
	int dots_per_line ;
	int t, b, l, r ;
	int textcolor ;
	int tsize ;
	struct GModule *module;
	struct Option *opt1, *opt2, *opt3, *opt4 ;

	module = G_define_module();
	module->description =
		"Creates and displays text labels "
		"in the active display frame on the graphics monitor.";

	opt1 = G_define_option() ;
	opt1->key        = "size" ;
	opt1->type       = TYPE_DOUBLE ;
	opt1->required   = NO ;
	opt1->options    = "0-100";
	opt1->answer     = "10" ;
	opt1->description= "Sets the label text size to the specified number" ;

	opt2 = G_define_option() ;
	opt2->key        = "backcolor" ;
	opt2->type       = TYPE_STRING ;
	opt2->required   = NO ;
	opt2->answer     = "black" ;
	opt2->options    = D_color_list();
	opt2->description= "Sets the color of the label background" ;

	opt3 = G_define_option() ;
	opt3->key        = "textcolor" ;
	opt3->type       = TYPE_STRING ;
	opt3->required   = NO ;
	opt3->answer     = "white" ;
	opt3->options    = D_color_list();
	opt3->description= "Sets the color of the label text" ;

	opt4 = G_define_option() ;
	opt4->key        = "font" ;
	opt4->type       = TYPE_STRING ;
	opt4->required   = NO ;
	opt4->options="romand,romanp,romant,romans,scriptc,scripts,romancs,italicc,italiccs,gothitt,gothgrt,gothgbt" ;
	opt4->description= "Sets the font" ;

	/* Initialize the GIS calls */
	G_gisinit(argv[0]) ;

	/* Check command line */

	if (G_parser(argc, argv))
		exit(-1);

	if (opt1->answer != NULL)
		sscanf(opt1->answer,"%f",&size);

	backcolor = D_translate_color(opt2->answer) ;
	if (backcolor == 0)
	{
		fprintf (stdout,"Don't know the color %s\n",opt2->answer);
		exit(-1) ;
	}

	textcolor = D_translate_color(opt3->answer);
	if (textcolor == 0)
	{
		fprintf (stdout,"Don't know the color %s\n",opt3->answer);
		exit(-1);
	}


	/* */
	R_open_driver();

	if (opt4->answer != NULL)
		R_font(opt4->answer) ;

	if (D_get_cur_wind(window_name))
		G_fatal_error("No current window") ;

	if (D_set_cur_wind(window_name))
		G_fatal_error("Current window not available") ;

	/* Figure out where to put text */
	D_get_screen_window(&t, &b, &l, &r) ;
	R_set_window(t, b, l, r) ;

	dots_per_line = (int)(size/100.0 * (float)(b - t)) ;
	tsize = (int)(.8 * (float)dots_per_line) ;
	R_text_size(tsize, tsize) ;

	label(t, b, l, r, backcolor, textcolor, dots_per_line) ;

	R_close_driver();

	exit(0);
}
