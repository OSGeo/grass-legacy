#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

/******************************************************************/
/*                                                                */
/* getproj - import proj component of e00 - M. Wurtz (11/1999)    */
/*                                                                */
/******************************************************************/

extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fdlog;     	/* log file descriptor */

extern long read_e00_line( char *);

void getproj( void)
{
    char line[84];

    do {
	read_e00_line( line);
	if (debug > 3 && *line != '~')
	    fprintf( fdlog, "%s\n", line);
	if (!strncmp( line, "Units", 5))
	    sscanf( line+6, "%lf", &scale);
	if (scale == 0.0)		/* no number ? */
	    scale = 1.0;
    } while (strncmp( line, "EOP", 3));
    scale = 1.0 / scale;
    if (debug > 2)
	fprintf( fdlog, "Scale used = %f\n", scale);
}

