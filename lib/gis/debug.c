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

#define BUFFER_SIZE 2000

static grass_debug_level = -1;

int G_debug (int level, char *msg,...)
{
#ifdef GDEBUG
    char    buffer[BUFFER_SIZE + 1], *lstr, *filen;
    va_list ap;
    FILE    *fd;
   
    if (grass_debug_level < 0) {
        lstr =  getenv("GRASS_DEBUG_LEVEL");

        if ( lstr != NULL )
            grass_debug_level = atoi ( lstr );
        else
            grass_debug_level = 0;
    }
	
    if ( grass_debug_level >= level ) {
        va_start(ap,msg);
        vsnprintf(buffer, BUFFER_SIZE, msg,ap);
        va_end(ap);

	filen =  getenv("GRASS_DEBUG_FILE"); 
        if ( filen != NULL ) {
	    fd = fopen (filen,"a");
            if ( !fd ) {
		G_warning ( "Cannot open debug file '%s'", filen);
		return 0;
	    }
            fprintf (fd, "D%d/%d: %s\n", level, grass_debug_level, buffer);
	    fclose ( fd );
	} else {
            fprintf (stderr, "D%d/%d: %s\n", level, grass_debug_level, buffer);
	}
    }
#endif

    return 1;
}

