#include <stdio.h>
read_data(fd,fdoutp,x,nx,y,ndata)
    FILE *fd, *fdoutp;
    double **x, **y;
    int *nx, *ndata;
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
    printf ("%d variables\n", *nx - 1);
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
    printf ("%d observations\n", *ndata);
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

nonlinear_data(x1,nx1,y,epsilon,ndata,x2,nx2)
	double *x1, **x2, *y, *epsilon;
	int *nx1, *nx2, *ndata;
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

