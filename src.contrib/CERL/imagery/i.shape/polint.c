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

void polint(xa,ya,n,x,y,dy)
double xa[],ya[],x,*y,*dy;
int n;
{
	int i,m,ns=1;
	double den,dif,dift,ho,hp,w;
	double *c,*d;
/*
for (i=1; i<=n; i++)
printf("in polint, xa[%d]=%lf, ya=%lf\n",i,xa[i],ya[i]);
*/

	dif=fabs(x-xa[1]);
/*
printf("in polint, before for, dif=%lf\n", dif);
*/

	c=(double *)calloc(n,sizeof(double));
	d=(double *)calloc(n,sizeof(double));
	for (i=1;i<=n;i++){	
		if((dift=fabs(x-xa[i])) < dif){
			ns=i;
			dif=dift;
/*
printf("in polint, after for, dif=%lf\n", dif);
*/
		}
		c[i]=ya[i];
		d[i]=ya[i];
	}
	*y=ya[ns--];
/*
printf("in polint, first, y=%lf\n", *y);
*/

	for (m=1;m<n;m++){
		for (i=1;i<=n-m;i++) {
			ho=xa[i]-x;
			hp=xa[i+m]-x;
			w=c[i+1]-d[i];
			if((den=ho-hp) == 0.0) 
{
/*
printf("in polint, m=%d, i=%d, ho=%lf, hp=%lf, w=%lf, den=%lf\n", 
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
printf("in polint, second, i=%d, m=%d, y=%lf\n", i, m, *y);
*/
	}
	free(d);
	free(c);
}
