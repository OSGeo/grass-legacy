#include "imagery.h"

I_init_signatures (S, nbands)
    struct Signature *S;
{
    S->nbands   = nbands;
    S->nsigs    = 0;
    S->sig      = NULL;
    S->title[0] = 0;
}

#define SIG struct One_Sig

I_new_signature (S)
    struct Signature *S;
{
    int n;
    int i;

    i = S->nsigs++;
    S->sig = (SIG *) G_realloc (S->sig, S->nsigs * sizeof (SIG));

    S->sig[i].mean = (double *) G_calloc (S->nbands, sizeof(double));
    S->sig[i].var = (double **) G_calloc (S->nbands, sizeof(double *));

    for (n = 0; n < S->nbands; n++)
	S->sig[i].var[n] = (double *) G_calloc (S->nbands, sizeof(double));

    S->sig[i].status = 0;
    S->sig[i].have_color = 0;
    sprintf(S->sig[i].desc, "Class %d", i+1);;
    return S->nsigs;
}

I_free_signatures (S)
    struct Signature *S;
{
    int n;
    int i;

    for (i = 0; i < S->nsigs; i++)
    {
	for (n=0; n < S->nbands; n++)
	    free (S->sig[i].var[n]);
	free (S->sig[i].var);
	free (S->sig[i].mean);
    }
    I_init_signatures (S,0);
}

I_read_one_signature (fd, S)
    FILE *fd;
    struct Signature *S;
{
    int n;
    int i;
    struct One_Sig *s;

    while ((i = fgetc(fd)) != EOF)
	if (i == '#') break;
    if (i != '#') return 0;

    i = I_new_signature (S);
    s = &S->sig[i-1];

    I_get_to_eol (s->desc, sizeof(s->desc), fd);
    G_strip (s->desc);

    if (fscanf (fd, "%d", &s->npoints) != 1)
	return -1;

    for (i=0; i < S->nbands; i++)
    {
	if (fscanf (fd, "%lf", &s->mean[i]) != 1)
	    return -1;
    }

    for (i=0; i < S->nbands; i++)
    {
	for (n=0; n <= i; n++)
	{
	    if (fscanf (fd, "%lf", &s->var[i][n]) != 1)
		return -1;
	    s->var[n][i] = s->var[i][n];	/* added 28 aug 91 */
	}
    }
    if (fscanf (fd,"%f%f%f",&s->r, &s->g, &s->b) == 3 &&
	s->r >= 0.0 && s->r <= 1.0 &&
	s->g >= 0.0 && s->g <= 1.0 &&
	s->b >= 0.0 && s->b <= 1.0)
	    s->have_color = 1;

    s->status = 1;
    return 1;
}

I_read_signatures (fd, S)
    FILE *fd;
    struct Signature *S;
{
    int n;

    S->title[0] = 0;
    while ((n = fgetc(fd)) != EOF)
	if (n == '#') break;
    if (n != '#')
	return -1;
    I_get_to_eol (S->title, sizeof(S->title),fd);
    G_strip (S->title);

    while ((n = I_read_one_signature (fd, S)) == 1)
	;

    if (n < 0) return -1;
    if (S->nsigs == 0) return -1;
    return 1;
}

I_write_signatures (fd, S)
    FILE *fd;
    struct Signature *S;
{
    int k;
    int n;
    int i;
    struct One_Sig *s;

    fprintf (fd, "#%s\n", S->title);
    for (k = 0; k < S->nsigs; k++)
    {
	s = &S->sig[k];
	if (s->status != 1)
	    continue;
	fprintf (fd, "#%s\n", s->desc);
	fprintf (fd, "%d\n", s->npoints);
	for (i=0; i < S->nbands; i++)
	    fprintf(fd, "%lf ", s->mean[i]);
	fprintf (fd, "\n");
	for (i=0; i < S->nbands; i++)
	{
	    for (n=0; n <= i; n++)
		fprintf (fd, "%lf ", s->var[i][n]);
	    fprintf (fd, "\n");
	}
	if (s->have_color)
	    fprintf (fd, "%f %f %f\n", s->r, s->g, s->b);
    }
    return 1;
}
