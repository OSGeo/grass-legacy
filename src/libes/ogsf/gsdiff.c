/*  gsdiff.c
    Bill Brown, USACERL  
    November 1994
routines to set up automatic on-the-fly recalculation
of surface elevations, doing a "scaled difference" using another
surface for reference

Note that we're using a true difference here, between data set values,
no translations, etc.

TODO: generalize this concept to allow transform functions which are
dependent on surfaces that are dependent on other surfaces, etc., as long
as the dependency doesn't loop back.

*/
	
#include "gstypes.h"
#include "gsget.h"
#include <stdio.h>

static geosurf *Refsurf = NULL;
static typbuff *Refbuff = NULL;
static float Refscale = 1.0;


/***********************************************************************/
gsdiff_set_SDscale(scale)
float scale;
{

    Refscale = scale;
}

/***********************************************************************/
float
gsdiff_get_SDscale()
{
    return(Refscale);
}

/***********************************************************************/
gsdiff_set_SDref(gsref)
geosurf *gsref;
{

	Refsurf = gsref;
	Refbuff = gs_get_att_typbuff(gsref, ATT_TOPO, 0);

}

/***********************************************************************/
geosurf *
gsdiff_get_SDref()
{
    if(Refsurf && Refbuff)
	return(Refsurf);
    return(NULL);

}

/***********************************************************************/
float
gsdiff_do_SD(val, offset)
float val;
int offset;
{
float ref;

    if(Refbuff){
	GET_MAPATT(Refbuff, offset, ref);
	return(ref + (val - ref)*Refscale);
    }
    return(val);


}

/***********************************************************************/

/***********************************************************************/

/***********************************************************************/

