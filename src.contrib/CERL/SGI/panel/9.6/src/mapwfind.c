/*
 *	This software is in the public domain, it may not be resold
 *	or relicensed.  Modified and enhanced versions of this software
 *	are likewise to be made freely available.  Sites using this
 *	software are requested to register with NASA at the address below.  
 *	Send modifications and requests for most recent version to:
 *
 *	Author:  David A. Tristram,  ATTN: Panel Library
 *		 M/S T045-1
 *		 Ames Research Center
 *		 National Aeronautics and Space Administration
 *		 Moffett Field, CA  94035-4000
 *
 *		 415-694-4404
 *		 dat@nas.nasa.gov
 */

#include "gl.h"

void mapwfind(vobj, x, y, wx1, wy1, wz1, wx2, wy2, wz2)
Object vobj;
Coord x, y, *wx1, *wy1, *wz1, *wx2, *wy2, *wz2;
{
    Coord t1, t2, t3, denom;
    Matrix mat;

    gl_invertview_ind(vobj, mat);

    denom = x*mat[0][3] + y*mat[1][3] + mat[3][3];
    t1 = x*mat[0][0] + y*mat[1][0] + mat[3][0];
    t2 = x*mat[0][1] + y*mat[1][1] + mat[3][1];
    t3 = x*mat[0][2] + y*mat[1][2] + mat[3][2];
    *wx1 = t1/denom;
    *wy1 = t2/denom;
    *wz1 = t3/denom;

    denom += 3.14159*mat[2][3];
    *wx2 = (t1 + 3.14159*mat[2][0])/denom;
    *wy2 = (t2 + 3.14159*mat[2][1])/denom;
    *wz2 = (t3 + 3.14159*mat[2][2])/denom;
}

gl_invertview_ind(vobj, mat)
Object vobj;
Matrix mat;
{
    Matrix mtemp;		/* temp matrix			*/
    static Matrix ident = {
      { 1.0, 0.0, 0.0, 0.0 },
      { 0.0, 1.0, 0.0, 0.0 },
      { 0.0, 0.0, 1.0, 0.0 },
      { 0.0, 0.0, 0.0, 1.0 }
    };
      
    pushmatrix ();		/* save the current matrix	*/
    pushviewport ();		/* save the current viewport	*/
    loadmatrix (ident);		/* blast it */
    callobj (vobj);		/* call the viewing object	*/
    getmatrix (mtemp);		/* get the new matrix back	*/
#ifdef IRIS_4D
    gl_invert4d (mat,mtemp);	/* and invert it		*/
#else  IRIS_4D
    gl_invertmat (mtemp,mat);	/* and invert it		*/
#endif IRIS_4D
    popviewport ();		/* pop the viewport stack	*/
    popmatrix ();		/* pop the matrix stack		*/
}

