#include "gis.h"
#include "globals.h"
#include <math.h>

#define SWAP(a,b) tempr=(a);(a)=(b);(b)=tempr

void fft_curve (double data[], int nn, int isign)
{
	int n,mmax,m,j,istep,i;
	double wtemp,wr,wpr,wpi,wi,theta;
	float tempr,tempi;
/*
fprintf (stdout,"in fft_curve, nn=%d\n", nn);
for (i=0; i<nn; i++)
fprintf (stdout,"in fft_curve, dalpha[%d]=%lf\n", i, data[i]);
*/
	n=nn << 1;
	j=1;
	for(i=1;i<n;i+=2){
		if (j > i) {
			SWAP(data[j],data[i]);
			SWAP(data[j+1],data[i+1]);
		}
		m=n >>1;
		while (m >= 2 && j > m) {
			j -= m;
			m >>= i;
		}
		j += m;
	}
	mmax=2;
	while (n > mmax) {
		istep = 2*mmax;
		theta=6.28318530717959/(isign*mmax);
		wtemp=sin(0.5*theta);
		wpr=-2.0*wtemp*wtemp;
		wpi=sin(theta);
		wr=1.0;
		wi=0.0;
		for (m=1; m<mmax; m+=2) {
			for(i=m;i<=n;i+=istep) {
				j=i+mmax;
				tempr=wr*data[j]-wi*data[j+1];
				tempi=wr*data[j+1]+wi*data[j];
				data[j]=data[i]-tempr;
				data[j+1]=data[i+1]-tempi;
				data[i] += tempr;
				data[i+1] += tempi;
			}
			wr=(wtemp=wr)*wpr-wi*wpi+wr;
			wi=wi*wpr+wtemp*wpi+wi;
		}
		mmax=istep;
	}
}
