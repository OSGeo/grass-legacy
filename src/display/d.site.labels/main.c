/* Hacked for new sites API by Eric G. Miller <egm2@jps.net> 2000-10-01 */
/* 
 * $Id$ */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "raster.h"
#include "display.h"
#include "colors.h"
#include "local_proto.h"

#define DEFAULT_ATTRIBUTE "string"
#define DEFAULT_INDEX "1"

int main (int argc, char **argv)
{
    struct Cell_head window ;
    char window_name[64] ;
    char *site_name ;
    char *mapset, position[MAX_SITE_STRING] ;
    FILE *infile ;
    char buff[128] ;
    int t, b, l, r ;
    int i, column, index ;
    struct GModule *module;
    struct Option *site_opt, *xref_opt, *yref_opt, *color_opt, *size_opt, *backgr_opt;
    struct Option *border_opt, *font_opt, *col_opt, *index_opt ;
    struct Flag *mouse;
	

/* Initialize the GIS calls */
    G_gisinit(argv[0]) ;

    module = G_define_module();
    module->description =
        "Interactively label sites using a string or double "
        "attribute value or the category value on the current "
        "graphics monitor.";

    mouse = G_define_flag() ;
    mouse->key = 'm';
    mouse->description =  "Use mouse to interactively place scale" ;

    site_opt = G_define_option() ;
    site_opt->key        = "file" ;
    site_opt->type       = TYPE_STRING ;
    site_opt->required   = YES ;
    site_opt->gisprompt  = "old,site_lists,sites" ;
    site_opt->description= "Name of sites file" ;

    col_opt = G_define_option();
    col_opt->key     = "attr";
    col_opt->type    = TYPE_STRING;
    col_opt->required = NO;
    col_opt->description = "Type of attribute to use for labels";
    col_opt->options = "string,cat,double,coords,dim";
    col_opt->answer  = DEFAULT_ATTRIBUTE;

    index_opt = G_define_option();
    index_opt->key = "index";
    index_opt->type = TYPE_STRING;
    index_opt->required = NO;
    index_opt->description = "Index of attribute. Ignored when attr=cat or attr=coords.";
    index_opt->answer = DEFAULT_INDEX;
    
    xref_opt = G_define_option() ;
    xref_opt->key        = "xref" ;
    xref_opt->type       = TYPE_STRING ;
    xref_opt->required   = NO ;
    xref_opt->options    = "left,center,right";
    xref_opt->answer     = "center";
    xref_opt->description = "Relative horizontal position 'left|center|right'" ;

    yref_opt = G_define_option();
    yref_opt->key = "yref";
    yref_opt->type = TYPE_STRING;
    yref_opt->required = NO;
    yref_opt->options = "top,center,bottom";
    yref_opt->answer  = "bottom";
    yref_opt->description = "Relative vertical position 'top|center|bottom'" ;

    size_opt = G_define_option() ;
    size_opt->key        = "size" ;
    size_opt->type       = TYPE_DOUBLE ;
    size_opt->required   = NO ;
    size_opt->answer     = "10";
    size_opt->description= "Size of text (pixels)" ;

    color_opt = G_define_option() ;
    color_opt->key        = "color" ;
    color_opt->type       = TYPE_STRING ;
    color_opt->required   = NO ;
    color_opt->options    = D_COLOR_LIST;
    color_opt->answer     = "white";
    color_opt->description= "Text color" ;

    backgr_opt = G_define_option() ;
    backgr_opt->key        = "backgr" ;
    backgr_opt->type       = TYPE_STRING ;
    backgr_opt->required   = NO ;
    backgr_opt->answer     = "none" ;
    backgr_opt->options    = "none," D_COLOR_LIST;
    backgr_opt->description= "Background color" ;

    border_opt = G_define_option() ;
    border_opt->key        = "border" ;
    border_opt->type       = TYPE_STRING ;
    border_opt->required   = NO ;
    border_opt->answer     = "none" ;
    border_opt->options    = "none," D_COLOR_LIST;
    border_opt->description= "Border color" ;

    font_opt = G_define_option() ;
    font_opt->key        = "font" ;
    font_opt->type       = TYPE_STRING ;
    font_opt->required   = NO ;
    font_opt->options    = "cyrilc,gothgbt,gothgrt,gothitt,greekc,greekcs,greekp,"
    	"greeks,italicc,italiccs,italict,romanc,romancs,romand,"
	"romans,romant,scriptc,scripts";
    font_opt->answer     = "romans" ;
    font_opt->description= "Fontname" ;

    if (G_parser(argc, argv))
        exit(-1);

/* Check command line */

/* Save map name */
    site_name = site_opt->answer ;

/* Make sure map is available */
    mapset = G_find_sites (site_name, "") ;
    if (mapset == NULL)
    {
        sprintf(buff,"Sites file [%s] not available", site_name);
        G_fatal_error(buff) ;
    }

/* Open map is available */
    infile = G_fopen_sites_old (site_name, mapset) ;
    if (infile == NULL)
    {
        sprintf(buff,"Cant open sitesfile [%s]", site_name);
        G_fatal_error(buff) ;
    }

/* Index column and number */
    if(strcmp(col_opt->answer, "string") == 0) {
        column = SITE_ATTR_STR;
    }
    else if (strcmp(col_opt->answer, "cat") == 0) {
        column = SITE_ATTR_CAT;
    }
    else if (strcmp(col_opt->answer, "double") == 0) {
        column = SITE_ATTR_DBL;
    }
    else if (strcmp(col_opt->answer, "coords") == 0) {
        column = SITE_ATTR_COORD;
    }
    else if (strcmp(col_opt->answer, "dim") == 0) {
        column = SITE_ATTR_DIM;
    }
    else {
        G_fatal_error("Unknown attribute type!\n");
    }

    index = atoi(index_opt->answer) - 1;
    if (index < 0) {
        G_fatal_error("Index must be a positive number greater than zero!\n");
    }

/* Default positioning hack (should fix do_labels) */
    sprintf(position, "%s %s", xref_opt->answer, yref_opt->answer);

    if (R_open_driver() != 0)
        G_fatal_error ("No graphics device selected");

    if (D_get_cur_wind(window_name))
        G_fatal_error("No current window") ;

    if (D_set_cur_wind(window_name))
        G_fatal_error("Current window not available") ;

/* Read in the map window associated with window */
    G_get_window (&window);
    if (D_check_map_window(&window))
        G_fatal_error("Setting map window") ;

    if (G_set_window(&window) == -1) 
        G_fatal_error("Current window not settable") ;

/* Determine conversion factors */
    t = b = l = r = 0;
    if (D_get_screen_window(&t, &b, &l, &r))
        G_fatal_error("Getting screen window") ;
    if (D_do_conversions(&window, t, b, l, r))
        G_fatal_error("Error in calculating conversions") ;

	
/* Go draw the cell file */
    do_labels(infile,window, position, color_opt->answer, size_opt->answer, 
            backgr_opt->answer, border_opt->answer, 
            font_opt->answer, column, index, mouse->answer);

    D_add_to_list(G_recreate_command()) ;

    fclose(infile) ;

    R_close_driver();

    return 0;
}
/* vim: softtabstop=4 shiftwidth=4 expandtab */
