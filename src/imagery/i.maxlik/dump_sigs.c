#include "imagery.h"

main(argc,argv) char *argv[];
{
    FILE *fd;
    struct Signature S;
    int nfiles;


    if (argc != 3)
	usage(argv[0]);
    if (sscanf (argv[2], "%d", &nfiles) != 1)
	usage(argv[0]);
    if (nfiles <= 0)
	usage(argv[0]);
    I_init_signatures (&S, nfiles);
    fd = fopen (argv[1], "r");
    if (fd == NULL)
    {
	printf ("can't open signature file\n");
	exit(0);
    }
    if (I_read_signatures (fd, &S) < 0)
    {
	printf ("error reading signature file\n");
	exit(0);
    }
    fclose (fd);
    printf ("%d signatures\n", S.nsigs);
    test_signatures(&S);
}
test_signatures(S)
    struct Signature *S;
{
    int c;	/* class */
    int stat;
    int *ik, *jk;
    double det;
    struct One_Sig *s;
    char *malloc();

    ik = (int *) malloc (S->nbands * sizeof(int));
    jk = (int *) malloc (S->nbands * sizeof(int));

/* invert each signature */
    for (c = 0; c < S->nsigs; c++)
    {
	printf ("\n###### signature %d #########\n",c+1);
	s = &S->sig[c];
	if (*s->desc)
	    printf ("%s\n", s->desc);
	printf ("%d points\n", s->npoints);
	if (s->have_color)
	    printf ("color %f %f %f\n", s->r, s->g, s->b);
	stat = invert (s = &S->sig[c], S->nbands, ik, jk, &det);
	if(stat != 1)
	    printf ("** not valid **\n");
    }

    free (ik);
    free (jk);

}

static
invert (s, nbands, ik, jk, det)
    struct One_Sig *s;
    int *ik, *jk;
    double *det;
{
    int i,j,k;
    double max;
    double dx,dy,v;

/* copy lower half to upper half */
    for (k=0; k < nbands; k++)
	for (j=0; j < k; j++)
	    s->var[j][k] = s->var[k][j];


print_sig(s,nbands);


/* invert */
    *det = 1.0;
    for (k = 0; k < nbands; k++)
    {							/* 30 */
	max = 0.0;
	for (i = k; i < nbands; i++)
	{						/* 330 */
	    for (j = k; j < nbands; j++)
	    {						/* 330 */
		if((dx = v = s->var[i][j]) < 0)
		    dx = -dx;
		if ((dy = max) < 0)
		    dy = -dy;
		if (dy <= dx)
		{
		    ik[k] = i;
		    jk[k] = j;
		    max = v;
		}
	    }
/*330*/	}
	if (max == 0.0)
	{	
	    printf ("** ill conditioned matrix **\n");
	    return -1;	/* ill conditioned matrix */
	}

	if (ik[k] != k)
	{					/* 351 */
	    int kk;
	    kk = ik[k];
	    for (j = 0; j < nbands; j++)
	    {					/* 350 */
		v = s->var[k][j];
		s->var[k][j] = s->var[kk][j];
		s->var[kk][j] = -v;
/*350*/	    }
/*351*/	}
	if (jk[k] != k)
	{					/* 361 */
	    int jj;
	    jj = jk[k];
	    for (i = 0; i < nbands; i++)
	    {					/* 360 */
		v = s->var[i][k];
		s->var[i][k] = s->var[i][jj];
		s->var[i][jj] = -v;
/*360*/	    }
/*361*/	}
	for (j = 0; j < nbands; j++)
	    if (j != k)
		s->var[j][k] /= -max;
	for (j = 0; j < nbands; j++)
	    if (j != k)
	    {
		v = s->var[j][k];
		for (i = 0; i < nbands; i++)
		    if (i != k)
			s->var[j][i] += v * s->var[k][i];
	    }
	for (j = 0; j < nbands; j++)
	    if (j != k)
		s->var[k][j] /= max;

	*det *= max;
	s->var[k][k] = 1.0/max;
/*30*/
    }

printf ("inverted matrix\n\n");
print_sig(s,nbands);

    printf ("det = %lf", *det);
    if (*det <= 0.0)
    {
	printf (" ** determinant is negative **\n");
	return 0;	/* bad category! */
    }
    printf ("\n");

/* restore ordering of matrix */

    for (k = nbands-1; k>= 0; k--)
    {						/* 530 */
	j = ik[k];
	if (j > k)
	    for (i=0; i < nbands; i++)
	    {					/* 510 */
		v=s->var[i][k];
		s->var[i][k] = -(s->var[i][j]);
		s->var[i][j] = v;
/*510*/	    }
	i = jk[k];
	if (i > k)
	    for (j=0; j < nbands; j++)
	    {					/* 520 */
		v=s->var[k][j];
		s->var[k][j] = -(s->var[i][j]);
		s->var[i][j] = v;
/*520*/	    }
/*530*/
    }

/*
print_sig(s,nbands);
*/
    return 1;
}
print_sig(s,nbands)
    struct One_Sig *s;
{
    int j,k;

    for (k=0; k < nbands; k++)
    {
	for (j=0; j < nbands; j++)
	    printf ("%lf ",s->var[k][j]);
	printf ("\n");
    }
    printf ("\n");
}
usage(me) char *me;
{
    fprintf (stderr, "usage: %s sigfile nbands\n",me);
    exit(0);
}
