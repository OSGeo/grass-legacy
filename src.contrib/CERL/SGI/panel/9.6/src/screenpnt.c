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
#include <gl.h>

Boolean
screenpnt(wx, wy, wz, sx, sy)	/* returns false if pnt is clipped */
Coord wx, wy, wz;
Screencoord *sx, *sy;
{
short buf[3];

    feedback(buf, 3);
    pnt(wx, wy, wz);
    passthrough(0x99);
    endfeedback(buf);
    if (buf[0]==0x8) { /* passthrough command */
	*sx= *sy=0;
	return FALSE;
    } else {
        *sx=buf[1];
	*sy=buf[2];
	return TRUE;
    }
}

