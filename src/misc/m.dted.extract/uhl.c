#include <stdlib.h>
#include "dma.h"

int uhl (int first)
{
/* degrees, minutes, seconds, hemisphere,  number of lines/points */

    int d,m,s,res;
    char h;
    int n;

/* longitude in 10ths of arc seconds (negate eastern hemisphere) */
    if(sscanf(tapebuf,"%*4c%3d%2d%2d%1c",&d,&m,&s,&h) != 4)
	return 0;
    if (h != 'E' && h != 'W')
	return 0;
    if(sscanf(tapebuf,"%*47c%4d",&n) != 1)
	return 0;
    if(sscanf(tapebuf,"%*20c%4d",&res) != 1)
	return 0;
    
    if (!first && (res != lonres))
	return -1;

    lonres = res ;

    twest = (3600 * d + 60 * m + s) * 10;
    if (h == 'E')
	twest = -twest;
    teast = twest - (n-1) * lonres;

/* latitude in 10ths of arc seconds (negate southern hemisphere) */
    if(sscanf(tapebuf,"%*12c%3d%2d%2d%1c",&d,&m,&s,&h) != 4)
	return 0;
    if (h != 'N' && h != 'S')
	return 0;
    if(sscanf(tapebuf,"%*51c%4d",&n) != 1)
	return 0;
    if(sscanf(tapebuf,"%*24c%4d",&res) != 1)
	return 0;
    if (!first && (res != latres))
	return -1;

    latres = res ;

    tsouth = (3600 * d + 60 * m + s) * 10;
    if (h == 'S')
	tsouth = -tsouth;
    tnorth = tsouth + (n-1) * latres;


/* reallocate the tapebuf. data is 2 bytes per value */
    n = 8 + n * 2 ;
    if (tapebuflen < n)
    {
	tapebuf = realloc (tapebuf, tapebuflen = n);
	if (tapebuf == NULL)
	{
	    error ("Not Enough Memory",0);
	    exit(1);
	}
    }
    return 1;
}
