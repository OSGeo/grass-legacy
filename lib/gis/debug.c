#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>
#include "gis.h"

/*
 *
 * G_debug (int level, char *msg)
 * 
 * Print debugging message if environment variable GRASS_DEBUG_LEVEL
 * is set to level equal or greater  
 *
 * Levels: (recommended levels) 
 * 1 - message is printed once or few times per module
 * 3 - each row (raster) or line (vector)
 * 5 - each cell (raster) or point (vector) 
*/
static grass_debug_level = -1;

int G_debug (int level, char *msg,...)
{
#ifdef GDEBUG
    char buffer[1000], *lstr;
    va_list ap;
   
    if (grass_debug_level < 0) {
        lstr =  getenv("GRASS_DEBUG_LEVEL");

        if ( lstr != NULL )
            grass_debug_level = atoi ( lstr );
        else
            grass_debug_level = 0;
    }
	
    if ( grass_debug_level >= level ) {
        va_start(ap,msg);
        vsprintf(buffer,msg,ap);
        va_end(ap);
        fprintf (stderr, "D%d/%d: %s\n", level, grass_debug_level, buffer);
    }
#endif

    return 0;
}

