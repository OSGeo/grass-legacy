#include <stdio.h>
#define MAXGREY 230
static int nlevels=256 ;
static int ncolors=256;
static float reds[MAXGREY];
static float grns[MAXGREY];
static float blus[MAXGREY];
static int LAST=0;

Pset_color_levels(n)
{
/*    nlevels = n;
    ncolors = n*n*n;
	*/
}

Pcolorlevels (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = nlevels;
    *grn = nlevels;
    *blu = nlevels;
}

Pcolormultipliers (red, grn, blu)
    int *red, *grn, *blu;
{
    *red = 1;
    *grn = 1;
    *blu = 1;
}

Pcolornum (red, green, blue)
    float red, green, blue ;
{
    int i;
	if (LAST != 0)
	for (i=0;i<=LAST;i++){
		if (i > 256) {
			fprintf(stderr,"Warning: color table too big\n");
			sleep(5);
			exit();
			}
		if (red == reds[i] && green == grns[i] && blue == blus[i])
			return(i);
		}
	reds[LAST] = red; grns[LAST] = green; blus[LAST] = blue;
	LAST++;
	return (LAST-1);
}

Pcolorvalue (n, red, grn, blu)
	int n;
    float *red, *grn, *blu;
{
 if (n <= LAST)
	{
    	*red = reds[n] ;
    	*grn = grns[n] ;
    	*blu = blus[n] ;
	}
}

Pncolors()
{
    return ncolors;
}
