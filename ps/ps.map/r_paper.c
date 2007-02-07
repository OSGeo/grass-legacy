/* Function: infofile
**
** Author: Paul W. Carlson	April 1992
*/

#include <stdlib.h>
#include <string.h>
#include "map_info.h"
#include "ps_info.h"
#include "local_proto.h"

#define KEY(x) (strcmp(key,x)==0)

extern int rotate_plot;

static char *help[] =
{
    "width      #",
    "height     #",
    "left       #",
    "right      #",
    "top        #",
    "bottom     #",
    ""
};

int 
read_paper (void)
{	
    char buf[1024];
    char *key, *data;
    double w, h, t, l, b, r;

    w = PS.page_width;
    h = PS.page_height;
    l = PS.left_marg;
    r = PS.right_marg;
    t = PS.top_marg;
    b = PS.bot_marg;

    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

        if (KEY("width")) {
	    w = atof (data);
	    if ( w <= 0 ) {
		error(key, data, "illegal paper width request");
	    }
	    continue;
	}
        if (KEY("height")) {
	    h = atof (data);
	    if ( h <= 0 ) {
		error(key, data, "illegal paper height request");
	    }
	    continue;
	}
        if (KEY("left")) {
	    l = atof (data);
	    if ( l < 0 ) {
		error(key, data, "illegal paper left margin request");
	    }
	    continue;
	}
        if (KEY("right")) {
	    r = atof (data);
	    if ( r < 0 ) {
		error(key, data, "illegal paper right margin request");
	    }
	    continue;
	}
        if (KEY("top")) {
	    t = atof (data);
	    if ( t < 0 ) {
		error(key, data, "illegal paper top margin request");
	    }
	    continue;
	}
        if (KEY("bottom")) {
	    b = atof (data);
	    if ( b < 0 ) {
		error(key, data, "illegal paper bottom margin request");
	    }
	    continue;
	}

	error(key, data, "illegal paper sub-request");
    }

    PS.page_width = (rotate_plot) ? h : w;
    PS.page_height = (rotate_plot) ? w : h;
    PS.left_marg = (rotate_plot) ? t : l;
    PS.right_marg = (rotate_plot) ? b : r;
    PS.top_marg = (rotate_plot) ? l : t;
    PS.bot_marg = (rotate_plot) ? r : b;

    return 0;
}

