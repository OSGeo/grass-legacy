#include "imagery.h"
check_signatures(S)
    struct Signature *S;
{
    int i,j;
    struct Signature temp;
    double *lambda;


    lambda = (double *) G_calloc (S->nbands, sizeof(double));
    I_init_signatures (&temp, S->nbands);
    I_new_signature (&temp);
    for (i = 0; i < S->nsigs; i++)
    {
	copy_covariances (temp.sig[0].var, S->sig[i].var, S->nbands);
	if (!can_invert(temp.sig[0].var, S->nbands))
	{
	    S->sig[i].status = -1;
	    printf ("Signature %d not invertible\n", i+1);
	    continue;
	}
	copy_covariances (temp.sig[0].var, S->sig[i].var, S->nbands);
	if (!eigen(temp.sig[0].var, lambda, S->nbands))
	{
	    S->sig[i].status = -1;
	    printf ("Signature %d can't get eigen values\n", i+1);
	    continue;
	}
	for (j = 0; j < S->nbands; j++)
	{
	    if (lambda[j] <= 0.0)
	    {
		S->sig[i].status = -1;
		printf ("Signature %d not positive definite\n", i+1);
		break;
	    }
	}
    }
    free (lambda);
    I_free_signatures (&temp);
}
