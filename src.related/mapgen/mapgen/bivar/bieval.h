/* @(#)bieval.h	AMG v.3.2 */
/* data structures for bivariate polynomial evaluation procedures. */

struct _bieval{
	int	type;
	int	(*poly)();	/* polynomial evaluation routine*/
	double	x_scale, x_off, x_min, x_max;
	double	y_scale, y_off, y_min, y_max;
	int	x_deg, y_deg;
	int	maxC;
	double	*Cxy;		/* size maxC words		*/
	double	*Tx, *Ty;	/* size x_deg +1 words		*/
};

# define BICOEF struct _bieval
