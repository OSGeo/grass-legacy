/* Function: do_psfiles
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_info.h"

extern int verbose;

do_psfiles()
{
    int i;
    char buf[256];
    FILE *fp;

    for (i = 0; i < PS.num_psfiles; i++)
    {
	if ((fp = fopen(PS.psfiles[i], "r")) == NULL) continue;
	if (verbose > 1)
	{
            printf("PS-PAINT: reading PostScript include file <%s> ...",
	    	PS.psfiles[i]);
            fflush(stdout);
	}
	fprintf(PS.fp, "\n");
        while (fgets(buf, 256, fp) != NULL) fprintf(PS.fp, "%s", buf);
	fprintf(PS.fp, "\n");
	fclose(fp);
	if (verbose > 1)
	{
            printf("\n");
            fflush(stdout);
	}
    }
}
