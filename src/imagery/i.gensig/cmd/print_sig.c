#include "imagery.h"

print_one_sig (S, k)
	struct Signature *S;
{
	struct One_Sig *s;
	int i,n;
	FILE *fd;
	 
	fd = stdout;

	s = &S->sig[k];

	fprintf (fd, "#%s\n", s->desc);
	fprintf (fd, "%d\n", s->npoints);
	for (i=0; i < S->nbands; i++)
	    fprintf(fd, "%lf ", s->mean[i]);
	fprintf (fd, "\n");
	for (i=0; i < S->nbands; i++)
	{
	    for (n=0; n < S->nbands; n++)
		fprintf (fd, "%lf ", s->var[i][n]);
	    fprintf (fd, "\n");
	}
	if (s->have_color)
	    fprintf (fd, "%f %f %f\n", s->r, s->g, s->b);
}
