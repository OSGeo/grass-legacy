#include <stdio.h>
#include <unistd.h>
#include <string.h>
#include <math.h>

/******************************************************************/
/*                                                                */
/* getproj - import proj component of e00 - M. Wurtz (1998-10-10) */
/*                                                                */
/******************************************************************/

extern int debug;               /* debug level (verbosity) */
extern double scale;            /* scale of coordinates (Cf PRJ) */
extern FILE *fde00, *fdlog;     /* input and log file descriptors */

void getproj()
{
    char line[1024];

    do {
	fgets( line, 1024, fde00);
	if (debug > 3 && *line != '~')
	    fprintf( fdlog, line);
	if (!strncmp( line, "Units", 5))
	    sscanf( line+6, "%lf", &scale);
	if (scale == 0.0)		/* no number ? */
	    scale = 1.0;
    } while (strncmp( line, "EOP", 3));
    scale = 1.0 / scale;
    if (debug > 2)
	fprintf( fdlog, "Scale used = %lf\n", scale);
}

