			/*
			This function conducts interpolation and extrapolation
			given n+1 element's arrays 
			xa[1...n] and ya[1...n], 
			and given a value x, this routine returns a value y, 
			and an error estimate dy. the function is a polynomial
			*/

#include <math.h>
#include "gis.h"
#include "globals.h"

void 
polint (double xa[], double ya[], int n, double x, double *y, double *dy)
{
	int i,m,ns=1;
	double den,dif,dift,ho,hp,w;
	double *c,*d;
/*
for (i=1; i<=n; i++)
fprintf (stdout,"in polint, xa[%d]=%lf, ya=%lf\n",i,xa[i],ya[i]);
*/

	dif=fabs(x-xa[1]);
/*
fprintf (stdout,"in polint, before for, dif=%lf\n", dif);
*/

	c=(double *)G_calloc(n,sizeof(double));
	d=(double *)G_calloc(n,sizeof(double));
	for (i=1;i<=n;i++){	
		if((dift=fabs(x-xa[i])) < dif){
			ns=i;
			dif=dift;
/*
fprintf (stdout,"in polint, after for, dif=%lf\n", dif);
*/
		}
		c[i]=ya[i];
		d[i]=ya[i];
	}
	*y=ya[ns--];
/*
fprintf (stdout,"in polint, first, y=%lf\n", *y);
*/

	for (m=1;m<n;m++){
		for (i=1;i<=n-m;i++) {
			ho=xa[i]-x;
			hp=xa[i+m]-x;
			w=c[i+1]-d[i];
			if((den=ho-hp) == 0.0) 
{
/*
fprintf (stdout,"in polint, m=%d, i=%d, ho=%lf, hp=%lf, w=%lf, den=%lf\n", 
m,i,ho,hp,w,den);
*/
			G_fatal_error("error in routine POLINT");
}
			den=w/den;
			d[i]=hp*den;
			c[i]=ho*den;
		}
/*
		*y += (*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
fprintf (stdout,"in polint, second, i=%d, m=%d, y=%lf\n", i, m, *y);
*/
	}
	G_free(d);
	G_free(c);
}
