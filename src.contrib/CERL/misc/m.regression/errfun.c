#include <stdio.h>
#include <math.h>

double err2 (double *y1, double *y2, int n)
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

double rms (double *y1, double *y2, int ndata)
{
    return sqrt(err2(y1,y2,ndata))/ndata;
}

void function (double *x, int nx, double *value, int na)
{
    int i;
    for (i = 0; i < nx; i++)
{	value[i] = x[i];
}
    value[na-1] = 1.0;
}

int show_parms (FILE *fdoutp, double *a, int na)
{
    int i;
    for (i = 0; i < na; i++)
	{
	fprintf (stdout," a[%d]=%g", i,a[i]);
	fprintf (fdoutp, " a[%d]=%g", i,a[i]);
	}
    fprintf (stdout,"\n");
    fprintf (fdoutp, "\n");

    return 0;
}

int yes (char *prompt)
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
