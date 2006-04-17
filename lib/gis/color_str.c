#include "string.h"
#include <grass/gis.h>
#include <grass/colors.h>

#define NUM_COLORS      0

/* These are color names that work where R:G:B does, but are not
   preallocated colors on devices */
/* Currently there are none */
static struct {
    char *name;
    int r, g, b;
} _colors[NUM_COLORS] =
{
/*    {"purple",  128,   0, 255}   Example of what a color could be */
};

/* 
*  Parses color string and sets red,green,blue
* 
*  Returns: 1 - OK
*           2 - NONE 
*           0 - Error 
* 
*/
int G_str_to_color (const char *str, int *red, int *green, int *blue)
{
    int i, ret, n;
    char buf[100], temp[10]; 

    G_strcpy (buf, str );
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
	/* Look for this color in the standard (preallocated) colors */
	for (i = 0; i < MAX_COLOR_NAMES; i++) {
	    if ( G_strcasecmp(buf, standard_color_names[i].name) == 0) {
		n = standard_color_names[i].number;
		*red   = (int) standard_colors_rgb[i].r;
		*green = (int) standard_colors_rgb[i].g;
		*blue  = (int) standard_colors_rgb[i].b;
                return 1;
	    }
        }

	/* Compare to local color table */
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

