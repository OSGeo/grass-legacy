#include <stdio.h>
#include "vizual.h"

/* going to see if the header information from the grid3 file is the same
as that in the dspf file which it should be.  If not there is no sense
to continue. If they are the same, then since we can just use one of them
we must add the grid3 *fp  to the informatin in Headfax
*/

diff_header (G3header, Headfax)
    file_info *G3header;
    file_info *Headfax;
{
	if (G3header->xdim != Headfax->xdim)
	{
		fprintf(stderr,"ERROR:xdim is %d in G3header, and %d in Headfax\n",G3header->xdim,Headfax->xdim);
/*		return(-1);*/
	}

	if (G3header->ydim != Headfax->ydim)
	{
		fprintf(stderr,"ERROR:ydim is %d in G3header, and %d in Headfax\n",G3header->ydim,Headfax->ydim);
	/*	return(-1);*/
	}

	if (G3header->zdim != Headfax->zdim)
	{
		fprintf(stderr,"ERROR:zdim is %d in G3header, and %d in Headfax\n",G3header->zdim,Headfax->zdim);
/*		return(-1);*/
	}


	if (G3header->type != Headfax->type)
	{
		fprintf(stderr,"ERROR:type is %d in G3header, and %d in Headfax\n",G3header->type,Headfax->type);
/*		return(-1);*/
	}

	if (G3header->min != Headfax->min)
	{
		fprintf(stderr,"ERROR:min is %f in G3header, and %f in Headfax\n",G3header->min,Headfax->min);
/*		return(-1);*/
	}

	if (G3header->max != Headfax->max)
	{
		fprintf(stderr,"ERROR:max is %f in G3header, and %f in Headfax\n",G3header->max,Headfax->max);
/*		return(-1);*/
	}
	/*everything matches*/
	Headfax->datainfp = G3header->datainfp;
	return(1);
}
