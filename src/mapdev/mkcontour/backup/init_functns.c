#include "contour.h"

cntr_init_functns(dfp,mapset,cellfile,contour_interval,window)
    FILE	*dfp; /*pointer to dig file */
    char	*mapset, *cellfile;
    int		contour_interval;
    struct Cell_head	*window; /* info for active window */
{

    struct head	 	contour_head; /* hold info for dig_binary_header file */
    char		*date;
    char		*name;


    
    
    /* read in info from current cell using active window values ******/
    G_get_set_window(window);

    
    /* fill in the elements of contour_head */

    /* organization name default is used */
    strcpy(contour_head.organization,"US Army Const. Eng. Rsch. Lab");

    /* calculate today's date */
    if ((date = G_date()) == NULL)
	date = "";
    strcpy(contour_head.date,date);

    /* print out the user's name */
    if ((name = G_whoami()) == NULL)
	name = "";
    strcpy(contour_head.your_name,name);

    /* combine information for map_name */
    sprintf(contour_head.map_name,"%s in %s",cellfile,mapset);

    /* ???? */
    strcpy(contour_head.source_date,"");

    /* create "other info" line in dig header*/
    sprintf(contour_head.line_3,"%s with contour interval of %d",
            cellfile,contour_interval);

    contour_head.orig_scale = 24000;
    contour_head.plani_zone = 0 ; /* ???? */
    contour_head.W = window->west;
    contour_head.N = window->north;
    contour_head.E = window->east;
    contour_head.S = window->south;
    contour_head.digit_thresh = 24000; /* ???? */
    contour_head.map_thresh = 0; /* ???? */

    /* write the appropriate header information to binary dig vector file */

    dig_write_head_binary(dfp,&contour_head);

}
