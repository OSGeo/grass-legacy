/*
 *   Draws icons at points listed in stdin.   
 */

#include "gis.h"
#include "display.h"
#include "raster.h"
#include "icon.h"

/* draw_icon.c */
int draw_icon(ICON *, register int, register int);
/* plot_points.c */
int plot_points(ICON *, int);
int input(char *, int);
/* setup.c */
int setup_plot(void);

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct
    {
	struct Option *color, *icon, *size, *points;
    } parm;
    struct
    {
	struct Flag *reversed;
    } flag;
    float size;
    int color;
    FILE *infile;
    char *name, *mapset;
    char *D_color_list();
    ICON icon1, icon2;

	module = G_define_module();
	module->description =
		"Displays points, as icons, at user-defined locations "
		"in the active display frame on the graphics monitor.";

    /* Define the different options */

    parm.icon = G_define_option() ;
    parm.icon->key        = "icon";
    parm.icon->type       = TYPE_STRING;
    parm.icon->required   = YES;
    parm.icon->gisprompt  = "old,icons,icon" ;
    parm.icon->description = "icon file name";

    parm.color = G_define_option() ;
    parm.color->key        = "color";
    parm.color->type       = TYPE_STRING;
    parm.color->required   = NO;
    parm.color->answer     = "white";
    parm.color->options    = D_color_list();
    parm.color->description= "set the color" ;

    parm.size = G_define_option() ;
    parm.size->key        = "size";
    parm.size->type       = TYPE_DOUBLE;
    parm.size->required   = NO;
    parm.size->answer     = "1";
    parm.size->options    = "1-1000";
    parm.size->description= "icon scaling factor";

    parm.points = G_define_option() ;
    parm.points->key        = "points";
    parm.points->type       = TYPE_STRING;
    parm.points->required   = NO;
    parm.points->description= "Unix file containing point coordinates";

    flag.reversed = G_define_flag();
    flag.reversed->key = 'r';
    flag.reversed->description = "Input coordinates reversed (north east)";

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    /* Check command line */

    if (G_parser(argc, argv) < 0)
        exit(-1);

    size = 1.0;
    sscanf(parm.size->answer,"%f",&size);
    if (size <= 0.0)
	size = 1.0;

    color = D_translate_color(parm.color->answer) ;

    name = parm.icon->answer;
    mapset = G_find_file2("icons", name, "");
    if (mapset == NULL)
    {
	fprintf (stderr, "icon <%s> not found\n", name);
	exit(1);
    }
    if (get_icon(name, mapset, &icon1) < 0)
    {
	fprintf (stderr, "ERROR reading icon <%s>\n", name);
	exit(1);
    }
    scale_icon (&icon1, &icon2, size);


    if (parm.points->answer != NULL)
    {
        if(freopen(parm.points->answer, "r", stdin) == NULL)
        {
	    perror (parm.points->answer);
            exit(-1);
        }
    }


    /* Setup driver and check important information */
    if (R_open_driver() != 0)
	    G_fatal_error ("No graphics device selected");
    R_standard_color (color) ;
    setup_plot();

    plot_points (&icon2, flag.reversed->answer);

	D_add_to_list(G_recreate_command()) ;
    R_close_driver();
    exit(0);
}
