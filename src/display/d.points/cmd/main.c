#define GLOBAL
#include "globals.h"
#include "raster.h"
#include "display.h"
#include "local_proto.h"

int 
main (int argc, char **argv)
{
    char *D_color_list();
	struct GModule *module;
    struct Option *opt1, *opt2, *opt3, *opt4;

	module = G_define_module();
	module->description =
		"Displays point graphics in the "
		"active frame on the graphics display monitor.";

    opt1 = G_define_option() ;
    opt1->key        = "color" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = NO ;
    opt1->answer     = "gray" ;
    opt1->options    = D_color_list();
    opt1->description= "Sets the current color to that stated" ;

    opt2 = G_define_option() ;
    opt2->key        = "size" ;
    opt2->type       = TYPE_INTEGER ;
    opt2->required   = NO ;
    opt2->answer     = "5" ;
    opt2->options    = "0-1000" ;
    opt2->description= "Size, in pixels, in which the icon is to be drawn" ;

    opt3 = G_define_option() ;
    opt3->key        = "type" ;
    opt3->type       = TYPE_STRING ;
    opt3->required   = NO ;
    opt3->answer     = "+" ;
    opt3->options    = "x,diamond,box,+" ;
    opt3->description= "Specify the type of the icon" ;

    opt4 = G_define_option() ;
    opt4->key        = "file" ;
    opt4->type       = TYPE_STRING ;
    opt4->required   = NO ;
    opt4->answer     = NULL ;
    opt4->description= "Input is a UNIX file name" ;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Check command line */

    if (G_parser(argc, argv))
	    exit(-1);

    color = D_translate_color(opt1->answer) ;
    if (color == 0)
    {
	fprintf (stdout,"Don't know the color %s\n", opt1->answer);
	G_usage() ;
	exit(-1);
    }

    sscanf(opt2->answer,"%d",&size);

    type = TYPE_PLUS;
    if (! strcmp(opt3->answer, "x"))
	type = TYPE_X ;
    else if (! strcmp(opt3->answer, "+"))
	type = TYPE_PLUS ;
    else if (! strcmp(opt3->answer, "box"))
	type = TYPE_BOX ;
    else if (! strcmp(opt3->answer, "diamond"))
	type = TYPE_DIAMOND ;

    infile = stdin ;
    if (opt4->answer != NULL)
    {
	infile = fopen(opt4->answer, "r") ;
	if (infile == NULL)
	{
	    fprintf(stderr, "File %s not available\n", opt4->answer) ;
	    G_usage() ;
	    exit(-1);
	}
    }


    /* Setup driver and check important information */
    R_open_driver();
    setup();

    /* Do the plotting */
    R_standard_color (color) ;
    R_text_size (size, size);
    switch(type)
    {
    case TYPE_X:
	    draw_points_x() ;
	    break ;
    case TYPE_PLUS:
	    draw_points_plus() ;
	    break ;
    case TYPE_BOX:
	    draw_points_box() ;
	    break ;
    case TYPE_DIAMOND:
	    draw_points_diamond() ;
	    break ;
    }

    R_close_driver();
    exit(0);
}
