#include "string.h"
#include "gis.h"

#define NUM_COLORS      15 

static struct {
    char *name;
    int r, g, b;
} _colors[NUM_COLORS] =
{
    {"white",   255, 255, 255},
    {"black",     0,   0,   0},
    {"red",     255,   0,   0},
    {"green",     0, 255,   0},
    {"blue",      0,   0, 255},
    {"yellow",  255, 255,   0},
    {"magenta", 255,   0, 255},
    {"cyan",      0, 255, 255},
    {"aqua",    100, 127, 255},
    {"grey",    127, 127, 127},
    {"gray",    127, 127, 127},
    {"orange",  255, 127,   0},
    {"brown",   180,  75,  25},
    {"violet",  255,   0, 255},
    {"indigo",    0, 127, 255}
};

/* 
*  Parses color string and sets red,green,blue
* 
*  Returns: 1 - OK
*           2 - NONE 
*           0 - Error 
* 
*/
int G_str_to_color (char *str, int *red, int *green, int *blue)
{
    int i, ret;
    char buf[100], temp[10]; 

    strcpy (buf, str );
    G_chop (buf);
    
    G_debug (3, "G_str_to_color(): str = '%s'", buf );

    if ( G_strcasecmp ( buf, "NONE" ) == 0 ) return 2;
    
    ret = sscanf (buf, "%d%[,:; ]%d%[,:; ]%d", red, temp, green, temp, blue);
   
    if ( ret == 5 ) { 
	if ( *red < 0 || *red > 255 || *green < 0 || *green > 255 ||
	     *blue < 0 || *blue > 255 ) 
	{ 
	    return 0; 
	}
        return 1;
    } else {
	for (i = 0; i < NUM_COLORS; i++) {
	    if ( G_strcasecmp(buf, _colors[i].name) == 0) {
		*red   = _colors[i].r;
		*green = _colors[i].g;
		*blue  = _colors[i].b;
                return 1;
	    }
        }
        return 0;
    }
	
    return 0;
}

