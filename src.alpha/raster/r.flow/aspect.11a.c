/*
** Written by Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
   US Army Construction Engineering Research Lab, University of Illinois 
** Copyright  Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
*/

/*
The flowtracing program, both binary and source is copyrighted, but available 
without fee for education, research and non-commercial purposes. Users may 
distribute the binary and source code to third parties provided that the
copyright notice and this statement appears on all copies and that no
charge is made for such copies.  Any entity wishing to integrate all or
part of the source code into a product for  commercial use or resale,
should contact authors of the software, U.S.Army CERL and University
of Illinois.

THE SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. THE
U.S.Army CERL, UNIVERSITY OF ILLINOIS OR AUTHORS SHALL NOT BE LIABLE FOR 
ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.

By copying this program, you, the user, agree to abide by the copyright
conditions and understandings with respect to any software which is marked
with a copyright notice.
*/

#include <math.h>
#include "gis.h"
#include "r.flow.11a.h"

CELL
aspect_fly(n, c, s, d)
    CELL *n, *c, *s;
    double d;
{
    double xslope 	= ((n[-1] + c[-1] + c[-1] + s[-1]) -
			   (n[1] + c[1] + c[1] + s[1])) / (8 * d);
    double yslope	= ((s[-1] + s[0] + s[0] + s[1]) -
			   (n[-1] + n[0] + n[0] + n[1])) / (8 * region.ns_res);
    double asp;

    if (!yslope)
	if (!xslope)
	    asp = UNDEF;
	else if (xslope > 0)
	    asp = parm.up ? 270. : 90.;
	else
	    asp = parm.up ? 90. : 270.;
    else
	if ((asp = atan2(xslope, yslope) / DEG2RAD) < 0.)
	    asp += 360.;

    return ROUND(asp);
}

	    
    
