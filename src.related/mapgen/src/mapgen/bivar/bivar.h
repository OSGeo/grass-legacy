/* @(#)bivar.h	AMG v.3.2 */
/* definitions for bivariate routines */

struct bfacts {
	double *work;		/* general work array			*/
	double *p;		/* equation work			*/
	double *xyzw;		/* data variables			*/
	int wghts;		/* weight (if != 3)			*/
	int (*np)();		/* bivariate poly generator		*/
	int deg;		/* univariate degree			*/
	double *Tx, *Ty;	/* address of (deg+1)*sizeof(double)	*/
} bfacts;

struct SCALE {
	double scale, off;
	double min, max;
};
