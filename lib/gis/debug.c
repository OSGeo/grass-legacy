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

static int grass_debug_level = -1;

int G_debug (int level, char *msg,...)
{
#ifdef GDEBUG
    int     n, size = 100;
    char    *buffer, *lstr, *filen;
    va_list ap;
    FILE    *fd;
   
    if (grass_debug_level < 0) {
        lstr = G__getenv( "DEBUG" );

        if ( lstr != NULL )
            grass_debug_level = atoi ( lstr );
        else
            grass_debug_level = 0;
    }
	
    if ( grass_debug_level >= level ) {
	buffer = (char *) G_malloc (size);

	while (1) {
	    /* Try to print in the allocated space. */
	    va_start(ap, msg);
	    n = vsnprintf (buffer, size, msg, ap);
	    va_end(ap);

	    /* If that worked, continue. */
            if (n > -1 && n < size) break;

	    /* Else try again with more space. */
	    size *= 2;  /* twice the old size */
	    buffer = (char *) G_realloc (buffer, size);
	}

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

