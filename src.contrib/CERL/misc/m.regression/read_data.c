#include <stdio.h>
int 
read_data (FILE *fd, FILE *fdoutp, double **x, int *nx, double **y, int *ndata)
{
    double *vector();
    double *xx, *yy;
    double dummy;
    char *b;
    char buf[1024];
    int i,j;

/* figure out how many variables on the first line */
    if(NULL == fgets(b = buf, sizeof buf, fd))
	*buf = 0;

    *nx = 0;
    while(1)
    {
	while (*b == ' ' || *b == '\t' || *b == '\n')
		b++;
	if (*b == 0) break;

	(*nx)++;
	while (*b && *b != ' ' && *b != '\t' && *b != '\n')
		b++;
    }
    if (*nx == 1)
    {
	fprintf (stderr, "%s - no variables\n", fd);
	exit(1);
    }
    if (*nx == 0)
    {
	fprintf (stderr, "%s - no data\n", fd);
	exit(1);
    }
    fprintf (stdout,"%d variables\n", *nx - 1);
    fprintf (fdoutp, "%d variables\n", *nx - 1);

/* read the file once to findout how may data values */
    fseek (fd, 0L, 0);
    *ndata = 0;
    while (fscanf(fd, "%lf", &dummy) == 1)
	(*ndata)++;
    if ((*ndata) % (*nx))
    {
	fprintf (stderr, "%s - missing or extra data\n", fd);
	exit(1);
    }
    *ndata /= *nx;
    fprintf (stdout,"%d observations\n", *ndata);
    fprintf (fdoutp, "%d observations\n", *ndata);
    (*nx)--;
    *x = xx = vector((*nx)*(*ndata));
    *y = yy = vector (*ndata);
    fseek (fd, 0L, 0);
    for (i = 0; i < *ndata; i++)
    {
	for (j = 0; j < *nx; j++)
	    fscanf (fd, "%lf", xx++);
	fscanf (fd, "%lf", yy++);
    }
    fclose (fd);
}

int 
nonlinear_data (double *x1, int *nx1, double *y, double *epsilon, int *ndata, double **x2, int *nx2)
{
	int k,m,i;
	double *vector();

	*nx2 = (*nx1)*2;
	*x2 = vector (*nx2 * (*ndata));
	for (k=0; k< (*ndata); k++)
	   for (m=0; m< (*nx1); m++)
	       {
		*((*x2)+k*(*nx2)+m) = *(x1+k*(*nx1)+m);
	        *((*x2)+k*(*nx2)+(*nx1)+m)=*(x1+k*(*nx1)+m) *
	(*(epsilon+k) - *(y+k));
	       }
}

