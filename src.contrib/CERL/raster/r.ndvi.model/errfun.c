#include <stdio.h>
#include <math.h>

double
err2(y1,y2,n)
    double *y1,*y2;
{
    double sum;
    double diff;
    int i;

    sum = 0.0;
    for (i=0; i<n;i++)
    {
	diff = y1[i]-y2[i];
	sum += diff*diff;
    }
    return sum;
}

double
rms(y1,y2,ndata)
    double *y1,*y2;
{
    return sqrt(err2(y1,y2,ndata))/ndata;
}
void
function (x, nx, value, na)
    double *x, *value;
    int nx,na;
{
    int i;
    for (i = 0; i < nx; i++)
{	value[i] = x[i];
}
    value[na-1] = 1.0;
}

show_parms(fdoutp, a,na)
    FILE *fdoutp;
    double *a;
{
    int i;
    for (i = 0; i < na; i++)
	{
	fprintf (stdout," a[%d]=%g", i,a[i]);
	fprintf (fdoutp, "%g\n",a[i]);
	}
    fprintf (stdout,"\n");
    fprintf (fdoutp, "a[0]=%g, a[1]=%g\n", a[0], a[1]);
}
yes(prompt)
    char *prompt;
{
    char ans[256];
    while (1)
    {
	fprintf (stdout,"%s (y/n) ", prompt);
	if (!fgets(ans,80,stdin)) exit(0);
	if (*ans == 'n' || *ans == 'N') return 0;
	if (*ans == 'y' || *ans == 'Y') return 1;
    }
}
