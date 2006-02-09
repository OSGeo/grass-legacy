#include <grass/imagery.h>
#include <grass/glocale.h>
#include "signature.h"


int 
print_one_sig (struct Signature *S, int k)
{
	struct One_Sig *s;
	int i,n;

	s = &S->sig[k];

	G_message(_("#%s"), s->desc);
	G_message("%d", s->npoints);

	for (i=0; i < S->nbands; i++)
	    G_message("%lf\n", s->mean[i]);

	for (i=0; i < S->nbands; i++)
	{
	    for (n=0; n < S->nbands; n++)
		G_message("%lf\n", s->var[i][n]);
	}

	if (s->have_color)
	    G_message("%f %f %f", s->r, s->g, s->b);
}
