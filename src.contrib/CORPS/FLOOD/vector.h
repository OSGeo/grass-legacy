/* vector.h */

typedef struct pointstruct2D {
    float px;
    float py;
    } POINT2D;

typedef struct rpointstruct2D {
    float px;
    float py;
    float pw;
    } RPOINT2D;

typedef struct pointstruct {
    float px;
    float py;
    float pz;
    } POINT;

typedef struct rpointstruct {
    float px;
    float py;
    float pz;
    float pw;
    } RPOINT;

typedef struct vecstruct {
    float dx;
    float dy;
    float dz;
    } VECTOR;

/* 1/27/89 -- added DOUBLE structs seth */

typedef struct dpointstruct2D {
    double px;
    double py;
    } DPOINT2D;

typedef struct rdpointstruct2D {
    double px;
    double py;
    double pw;
    } DRPOINT2D;

typedef struct dpointstruct {
    double px;
    double py;
    double pz;
    } DPOINT;

typedef struct drpointstruct {
    double px;
    double py;
    double pz;
    double pw;
    } DRPOINT;

typedef struct dvecstruct {
    double dx;
    double dy;
    double dz;
    } DVECTOR;

float
mag	(/*a*/),		    /*	||a||	*/
recip_mag (/*a*/),		    /*	1.0/||a||*/
dot	(/*a, b*/);		    /*  a . b	*/

VECTOR
*vlerp	(/*p, a, b, t*/),	    /*  v = ta + (a-t)b */
*norm	(/*a*/),		    /*  a /= ||a||  */
*assign	(/*a, b*/),		    /*	a = b	    */
*vscale	(/*a, t*/),		    /*	a *= t	    */
*diff	(/*a, b, c*/),		    /*	a = b - c   */
*cross	(/*a, b, c*/),		    /*	a = b x c   */
*lcross	(/*a, b, c*/);		    /*	a = -b x c  */

POINT
*plerp	(/*p, a, b, t*/),	    /* p = ta + (a-t)b */
*midpoint (/*a, b, c*/),	    /*	a = (b+c)/2 */
*pminusv (/*a, p, v*/),		    /*	a = p - v   */
*pplusv  (/*a, p, v*/),		    /*	a = p + v   */
*pminustv (/*a, p, t, v*/),	    /*	a = p - tv  */
*pplustv (/*a, p, t, v*/);	    /*	a = p + tv  */
