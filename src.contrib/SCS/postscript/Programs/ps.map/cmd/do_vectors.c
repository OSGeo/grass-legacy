/* Function: do_vectors
**
** Author: Paul W. Carlson	March 1992
*/

#include "ps_info.h"
#include "vector.h"
#include "Vect.h"

do_vectors(after_masking)
int after_masking;
{
    int n, z, lz, dig;
    struct Map_info Map;
    char dashes[100], buf[20], *ptr;
    extern int verbose;

    n = vector.count;
    while (n-- > 0)
    {
	if ( after_masking &&  vector.masked[n]) continue;
	if (!after_masking && !vector.masked[n]) continue;
	if (verbose > 1)
	{
	    printf("PS-PAINT: reading vector file <%s in %s> ...",
		vector.name[n], vector.mapset[n]);
	    fflush(stdout);
	}

	Vect_set_open_level(1);
	if (0 >= Vect_open_old(&Map, vector.name[n], vector.mapset[n]))
	{
	    char name[100];

	    sprintf(name, "%s in %s",vector.name[n], vector.mapset[n]);
	    error("vector file", name, "can't open");
	    continue;
	}

	fprintf(PS.fp, "[] 0 setdash\n");
	if (vector.hwidth[n])
	{
	    set_rgb_color(vector.hcolor[n]);
	    fprintf(PS.fp, "%d W\n", vector.width[n] + 2 * vector.hwidth[n]);
	    PS_vector_plot(&Map);
	    Vect_rewind(&Map); 
	}

	fprintf(PS.fp, "%d W\n", vector.width[n]);
	set_rgb_color(vector.colors[n][0]);
	dashes[0] = '[';
	dashes[1] = 0;
	lz = 0;
	if (vector.linestyle[n] != NULL)
	{
	    G_strip(vector.linestyle[n]);
 	    ptr = vector.linestyle[n];
	    while (*ptr && (*ptr < '1' || *ptr > '9'))
	    {
		lz++;
		ptr++;
	    }
	    if (lz) 
	    {
		sprintf(buf, "%d ", lz);
		strcat(dashes, buf);
	    }
	    while (*ptr)
	    {
		dig = 0;
		while (*ptr >= '1' && *ptr <= '9')
		{
		    dig++;
		    ptr++;
		}
		if (dig) 
		{
		    sprintf(buf, "%d ", dig);
		    strcat(dashes, buf);
		}
		z = 0;
	        while (*ptr && (*ptr < '1' || *ptr > '9'))
	        {
		    z++;
		    ptr++;
	        }
	        if (z) 
		{
		    sprintf(buf, "%d ", z);
		    strcat(dashes, buf);
		}
	    }
	}
	sprintf(buf, "] %d", lz);
	strcat(dashes, buf);
	fprintf(PS.fp, "%s setdash\n", dashes);
	vector.setdash[n] = G_store(dashes);
	PS_vector_plot(&Map);
	Vect_close(&Map);

	fprintf(PS.fp, "[] 0 setdash\n");
	if (verbose > 1) printf("\n");
    }
}
