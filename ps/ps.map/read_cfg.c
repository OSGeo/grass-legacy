/* Function: read_cfg
** added #include <stdlib.h> 7/98 Richard Nairn
#include <unistd.h>
**
** This function reads the configuration file to get the printer info.
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdlib.h>
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <math.h>
#include "gis.h"
#include "ps_info.h"
#include "paper.h"

#define FIELD(x) strcmp(x,field)==0

extern int verbose;
extern int rotate_plot;

/* Set page to one of predefined papers */
int
set_paper ( char *pname ) 
{
    int i;
 
    G_debug ( 3, "set_paper(): pname = %s", pname);

    /* Set default (a4) */
    PS.level = 2;
    PS.page_width = 8.27;
    PS.page_height = 11.69;
    PS.left_marg = 0.5;
    PS.right_marg = 0.5;
    PS.top_marg = 1.0;
    PS.bot_marg = 1.0;
    PS.res = 75;
    
    i = 0;
    while ( papers[i].name != NULL ) {
	if ( G_strcasecmp(papers[i].name, pname) == 0 ) {
	    PS.page_width = papers[i].page_width;
	    PS.page_height = papers[i].page_height;
	    PS.left_marg = papers[i].left_marg;
	    PS.right_marg = papers[i].right_marg;
	    PS.top_marg = papers[i].top_marg;
	    PS.bot_marg = papers[i].bot_marg;
	    PS.res = 75;
            G_debug ( 4, "  paper w = %f h = %f", PS.page_width, PS.page_height);
	    return 0;
	}
	i++;
    }
    G_warning ("Paper '%s' not found, using defaults", pname ); 

    return -1;
}

/* Reset map size and position */
void
reset_map_location ( void )
{
    double w, h;
    /* First reset origin if necessary */
    if ( PS.map_y_loc < PS.top_marg )  PS.map_y_loc = PS.top_marg;
    if ( PS.map_x_orig < PS.left_marg) PS.map_x_orig = PS.left_marg;
    
    PS.map_y_orig = PS.page_height - PS.map_y_loc;
   
    w = PS.page_width - PS.map_x_orig - PS.right_marg;
    h = PS.page_height - PS.map_y_loc - PS.bot_marg;
    
    if ( PS.map_width <= 0 || PS.map_width > w ) /* not specified or greater than available space */ 
        PS.map_width = w;
     
    if ( PS.map_height <= 0 || PS.map_height > h ) 
	PS.map_height = h;

    PS.min_y = 72.0 * PS.map_y_orig;
    
    G_debug ( 3, "map: w = %f h = %f", PS.map_width, PS.map_height);
}

