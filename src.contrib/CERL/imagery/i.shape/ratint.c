#include <math.h>
#include "gis.h"

				/*
				Given arrays xa[1,...n] and ya[1,...n], and 
				given a value x, this routine returns a value 
				of y and an accuracy estimate dy.  The value 
				returned is that of the diagonal rational 
				function, evaluated at x, which passes through 
				the n points (xai, yai), i = 1...n.
				*/

#define TINY 1.0e-25
#define FREERETURN {G_free(d);G_free(c);return;}

void 
ratint (double xa[], double ya[], int n, double x, double *y, double *dy)
{
	int m,i,ns=1;
	double w,t,hh,h,dd,*c,*d;
/*
for (i=1;i<=n;i++)
fprintf (stdout,"in ratint, xa[%d]=%lf, ya=%lf\n",i,xa[i],ya[i]);
fprintf (stdout,"x=%lf\n",x);
*/

	c = (double *)G_calloc(n,sizeof(double));
	d = (double *)G_calloc(n,sizeof(double));
	hh=fabs(x-xa[1]);
	for (i=1;i<=n;i++)
		{
		h=fabs(x-xa[i]);
		if(h == 0.0) {
			*y=ya[i];
			*dy=0.0;
			FREERETURN
		} else if (h<hh) {
			ns=i;
			hh=h;
		}
		c[i]=ya[i];
		d[i]=ya[i]+TINY;
	}
/*
for(i=1;i<=n;i++)
{
fprintf (stdout,"c[%d]=%lf, d[%d]=%lf\n",i,c[i],i,d[i]);
}
fprintf (stdout,"ns=%d\n",ns);
*/
	*y=ya[ns--];
/*
fprintf (stdout,"y=%lf,ns=%d\n",*y,ns);
*/
	for (m=1;m<n;m++) {
		for (i=1;i<=n-m;i++) {
			w=c[i+1]-d[i];
			h=xa[i+m]-x;
			t=(xa[i]-x)*d[i]/h;
			dd=t-c[i+1];
			if (dd == 0.0)
			{
			dd=TINY;
/*
			fprintf (stdout,"error in ratint but ignored\n");
*/
			}
			dd=w/dd;
			d[i]=c[i+1]*dd;
			c[i]=t*dd;
/*
fprintf (stdout,"in for, c[%d]=%lf, d[%d]=%lf\n",i,c[i],i,d[i]);
*/
		}
/*
fprintf (stdout,"ns=%d\n",ns);
*/
		*y +=(*dy=(2*ns < (n-m) ? c[ns+1] : d[ns--]));
/*
fprintf (stdout,"n-m=%d, 2ns=%d, ns=%d\n",n-m, 2*ns, ns);
fprintf (stdout,"c[%d]=%lf, d[%d]=%lf\n",ns+1,c[ns+1],ns,d[ns]);
fprintf (stdout,"dy=%lf\n",*dy);
*/
	}
/*
fprintf (stdout,"in ratint, y=%lf\n", *y);
*/
	FREERETURN
}
