/*
 *   d.vect (was: d.vect.cats)
 *
 *   - merged d.vect code into 1/2002 Markus Neteler
 *   - updated for GRASS 5 9/99 Bill Hughes
 *
 *   Draw the category in the binary vector (dig) file that
 *   the user wants displayed on top of the current image.
 *   jaf 12/1/91
 *
 */
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "display.h"
#include "raster.h"
#include "colors.h"
#define MAIN
#include "local_proto.h"
int quiet = 1;

static int check_catlist (char *answer)
{
    int status = 0;
    char *cptr;
    
    if (answer) {
        cptr = answer;
        do {
            if (!atoi (cptr))
                status = 1;
        } while ((cptr = strchr (cptr, ',')) && *(++cptr));
    }
    return status;
}

int main( int argc , char **argv )
{
    char *mapset ;
    char buf[1024] ;
    int stat=0;
    int color,fill;
    char map_name[128] ;
    struct GModule *module;
    struct Option *opt1, *opt2, *opt3;
    struct Flag *flag1, *flag2;
    struct line_pnts *Points;

    /* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description =
        "displays vector maps, optionally only selected categories in current monitor window.";

    opt1 = G_define_option() ;
    opt1->key        = "map" ;
    opt1->type       = TYPE_STRING ;
    opt1->required   = YES ;
    opt1->multiple   = NO ;
    opt1->gisprompt  = "old,dig,vector" ;
    opt1->description= "Name of existing vector map to be displayed" ;

    opt2 = G_define_option() ;
    opt2->key        = "color" ;
    opt2->type       = TYPE_STRING ;
    opt2->answer     = "white" ;
    opt2->options    = D_COLOR_LIST ;
    opt2->description= "Color desired for drawing map" ;

    opt3 = G_define_option();
    opt3->key	= "catnum" ;
    opt3->type	= TYPE_INTEGER ;
    opt3->required	= NO ;
    opt3->multiple	= YES ;
    opt3->checker       = check_catlist;
    opt3->description= "Vector category number(s) to be displayed" ;

    flag1 = G_define_flag();
    flag1->key 	= 'f';
    flag1->description= "Fill areas of selected polygons";

    flag2 = G_define_flag ();
    flag2->key             = 'v';
    flag2->description     = "Run verbosely";

    /* Check command line */
    if (G_parser(argc, argv))
        exit(-1);

    fill=0;
    fill=flag1->answer;
    quiet = !flag2->answer;

    strcpy(map_name, opt1->answer);

    color = D_translate_color(opt2->answer);

    /* Make sure map is available */
    mapset = G_find_file2 ("dig", map_name, "") ;
    if (mapset == NULL)
    {
        sprintf(buf,"Vector file [%s] not available", map_name);
        G_fatal_error(buf) ;
        exit(-1);
    }

    if (R_open_driver() != 0)
        G_fatal_error ("No graphics device selected");

    D_setup(0);
    R_standard_color(color) ;

    /********** First try level 2 vector read ***********/
    Points = Vect_new_line_struct ();

    if(opt3->answers)   /* Use opt#->answers for multiple */
    {
        stat = plotCat (map_name, mapset, Points, opt3->answers, fill);
        if (stat == -2)
            G_fatal_error ("bad category list");
    }
    else /* we want to see all vectors, no opt3 */
    {
        /* Vlevel 2 plotting, area filling not supported (yet?) */
        if (fill)
            fprintf(stderr, "-f ignored as only supported for vector selections with 'catnum' parameter\n");
        stat = plot2 (map_name, mapset, Points);
    }

    if (stat < 0 ) /* no topology found, try again */
    {
        /* Vlevel 1 plotting, area filling not supported (yet?) */
        if (fill)
            fprintf(stderr, "-f ignored as only supported for vector selections with 'catnum' parameter\n");
        if (opt3->answer)
            fprintf(stderr, "WARNING: Cannot select vectors as topology not present. Run v.support first\n");
        if (use_plot1(map_name, mapset))
            stat = plot1 (map_name, mapset, Points);
    }

    if (stat == 0) {
        D_add_to_list(G_recreate_command());
        D_set_dig_name(G_fully_qualified_name(
                    map_name, mapset));
        D_add_to_dig_list(G_fully_qualified_name(
                    map_name, mapset));
    }

    Vect_destroy_line_struct (Points);
    R_close_driver();
    exit(stat);
}
