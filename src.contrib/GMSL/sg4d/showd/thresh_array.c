#include "Viz.h"
/*
**  return an array of struct cmndln_info of resulting
**   thresholds based on  in_out flag
*/

/*
#define DEBUG1
*/
#define DEBUG2

build_thresh_arrays (D_spec, headp)
    struct dspec *D_spec;
    file_info *headp;
{
    double min_thresh, max_thresh;
    int a,b,i;

    min_thresh = headp->linefax.tvalue[D_spec->low]; 
    max_thresh = headp->linefax.tvalue[D_spec->hi]; 

    /* initializations */
    D_spec->threshes[0].nthres = 0;
    D_spec->threshes[1].nthres = 0;	/* for INSIDE CASE */
    
    if (D_spec->in_out == INSIDE)
    {
	b = 0;
	for (a = 0; a < headp->linefax.nthres; a++)
	{
	    if(min_thresh <= headp->linefax.tvalue[a] &&
	       max_thresh >= headp->linefax.tvalue[a])
	    {
		  D_spec->threshes[0].tvalue[b++] = headp->linefax.tvalue[a];
		  D_spec->threshes[0].nthres++;
	    }
	}
	
#ifdef DEBUG1
	printf("nthres = %d\n",D_spec->threshes[0].nthres);
	for (a=0; a<D_spec->threshes[0].nthres; a++) 
		printf("tvalue = %f\n",D_spec->threshes[0].tvalue[a]);
#endif
    }
    else /* OUTSIDE */
    {
	  for (i = 0; i < 2; i++)
	  {
	    b=0;
	    for (a = 0; a < headp->linefax.nthres; a++)
	    {
		if (!i)
		    if(min_thresh >= headp->linefax.tvalue[a]) 
		    {
		      D_spec->threshes[i].tvalue[b++] = headp->linefax.tvalue[a];
		      D_spec->threshes[i].nthres++;
		    }
		    else
		    if(max_thresh <= headp->linefax.tvalue[a]) 
		    {
		      D_spec->threshes[i].tvalue[b++] = headp->linefax.tvalue[a];
		      D_spec->threshes[i].nthres++;
		    }
	    }
	}
#ifdef DEBUG2
	printf("OUTSIDE nthres = %d\n",D_spec->threshes[1].nthres);
	for (a=0; a<D_spec->threshes[1].nthres; a++) 
		printf("tvalue = %f\n",D_spec->threshes[1].tvalue[a]);
#endif
    }
}
