#include "imagery.h"
extern double **G_alloc_matrix();

I_SigSetNClasses(S)
    struct SigSet *S;
{
    int i, count;

    for (i = 0, count = 0; i < S->nclasses; i++)
	if (S->ClassSig[i].used)
	    count++;
    
    return count;
}


struct ClassData *
I_AllocClassData (S, C, npixels)
    struct SigSet *S;
    struct ClassSig *C;
{
    struct ClassData *Data;

    Data = &(C->ClassData);
    Data->npixels = npixels;
    Data->count = 0;
    Data->x = G_alloc_matrix (npixels, S->nbands);
    Data->p = G_alloc_matrix (npixels, C->nsubclasses);
    return Data;
}

I_InitSigSet (S)
    struct SigSet *S;
{
    S->nbands = 0;
    S->nclasses = 0;
    S->ClassSig = NULL;
    S->title = NULL;
}

I_SigSetNBands (S, nbands)
    struct SigSet *S;
    int nbands;
{
    S->nbands = nbands;
}

struct ClassSig *
I_NewClassSig (S)
    struct SigSet *S;
{
    struct ClassSig *Sp;
    if (S->nclasses == 0)
        S->ClassSig = (struct ClassSig *) G_malloc (sizeof(struct ClassSig));
    else
        S->ClassSig = (struct ClassSig *) G_realloc ((char *)S->ClassSig,
		sizeof(struct ClassSig) * (S->nclasses+1));

    Sp = &S->ClassSig[S->nclasses++];
    Sp->classnum = 0;
    Sp->nsubclasses = 0;
    Sp->used = 1;
    Sp->type = SIGNATURE_TYPE_MIXED;
    Sp->title = NULL;
    return Sp;
}


struct SubSig *
I_NewSubSig (S, C)
    struct SigSet *S;
    struct ClassSig *C;
{
    struct SubSig *Sp;
    int i;

    if (C->nsubclasses == 0)
        C->SubSig = (struct SubSig *) G_malloc (sizeof(struct SubSig));
    else
        C->SubSig = (struct SubSig *) G_realloc ((char *)C->SubSig,
		sizeof(struct SubSig) * (C->nsubclasses+1));

    Sp = &C->SubSig[C->nsubclasses++];
    Sp->used = 1;
    Sp->R = (double **) G_calloc (S->nbands, sizeof(double *));
    Sp->R[0] = (double *) G_calloc (S->nbands * S->nbands, sizeof(double));
    for (i = 1; i < S->nbands; i++)
	Sp->R[i] = Sp->R[i-1] + S->nbands;
    Sp->Rinv = (double **) G_calloc (S->nbands, sizeof(double *));
    Sp->Rinv[0] = (double *) G_calloc (S->nbands * S->nbands, sizeof(double));
    for (i = 1; i < S->nbands; i++)
	Sp->Rinv[i] = Sp->Rinv[i-1] + S->nbands;
    Sp->means = (double *) G_calloc (S->nbands, sizeof(double));
    Sp->N = 0;
    Sp->pi = 0;
    Sp->cnst = 0;
    return Sp;
}

#define eq(a,b) strcmp(a,b)==0

I_ReadSigSet (fd, S)
    FILE *fd;
    struct SigSet *S;
{
    char tag[256];

    I_InitSigSet (S);

    while (gettag(fd, tag))
    {
	if (eq (tag, "title:"))
	    get_title(fd, S);
	if (eq (tag, "nbands:"))
	    get_nbands(fd, S);
	if (eq (tag, "class:"))
	    get_class (fd, S);
    }
    return 1; /* for now assume success */
}

static gettag(fd, tag)
    FILE *fd;
    char *tag;
{
    if(fscanf(fd, "%s", tag) != 1) return 0;
    G_strip (tag);
    return 1;
}

static
get_nbands(fd, S)
    FILE *fd;
    struct SigSet *S;
{
    fscanf (fd, "%d", &S->nbands);
}

static
get_title(fd, S)
    FILE *fd;
    struct SigSet *S;
{
    char title[1024];

    *title = 0;
    fscanf (fd, "%[^\n]", title);
    I_SetSigTitle(S, title);
}

static
get_class(fd, S)
    FILE *fd;
    struct SigSet *S;
{
    char tag[1024];
    struct ClassSig *C;

    C = I_NewClassSig(S);
    while (gettag(fd, tag))
    {
	if (eq(tag, "endclass:"))
	    break;
	if (eq(tag, "classnum:"))
	    get_classnum(fd, C);
	if (eq(tag, "classtype:"))
	    get_classtype(fd, C);
	if (eq(tag, "classtitle:"))
	    get_classtitle(fd, C);
	if (eq(tag, "subclass:"))
	    get_subclass(fd, S, C);
    }
}

static
get_classnum(fd, C)
    FILE *fd;
    struct ClassSig *C;
{
    fscanf (fd, "%d", &C->classnum);
}

static
get_classtype(fd, C)
    FILE *fd;
    struct ClassSig *C;
{
    fscanf (fd, "%d", &C->type);
}

static
get_classtitle(fd, C)
    FILE *fd;
    struct ClassSig *C;
{
    char title[1024];

    *title = 0;
    fscanf (fd, "%[^\n]", title);
    I_SetClassTitle (C, title);
}

static
get_subclass (fd, S, C)
    FILE *fd;
    struct SigSet *S;
    struct ClassSig *C;
{
    struct SubSig *Sp;
    char tag[1024];

    Sp = I_NewSubSig(S,C);

    while (gettag(fd, tag))
    {
	if (eq(tag, "endsubclass:"))
	    break;
	if (eq(tag, "pi:"))
	    get_subclass_pi(fd, Sp);
	if (eq(tag, "means:"))
	    get_subclass_means (fd, Sp, S->nbands);
	if (eq(tag, "covar:"))
	    get_subclass_covar (fd, Sp, S->nbands);
    }
}

static
get_subclass_pi(fd, Sp)
    FILE *fd;
    struct SubSig *Sp;
{
    fscanf (fd, "%lf", &Sp->pi);
}

static
get_subclass_means (fd, Sp, nbands)
    FILE *fd;
    struct SubSig *Sp;
    int nbands;
{
    int i;

    for (i = 0; i < nbands; i++)
	fscanf (fd, "%lf", &Sp->means[i]);
}

static
get_subclass_covar (fd, Sp, nbands)
    FILE *fd;
    struct SubSig *Sp;
    int nbands;
{
    int i,j;

    for (i = 0; i < nbands; i++)
	for (j = 0; j < nbands; j++)
	    fscanf (fd, "%lf", &Sp->R[i][j]);
}

I_SetSigTitle (S, title)
    struct SigSet *S;
    char *title;
{
    if (title == NULL) title = "";
    if (S->title)
        free (S->title);
    S->title = G_malloc (strlen (title)+1);
    strcpy(S->title, title);
}

char *
I_GetSigTitle (S)
    struct SigSet *S;
{
    if (S->title)
        return S->title;
    else
        return "";
}

I_SetClassTitle (C, title)
    struct ClassSig *C;
    char *title;
{
    if (title == NULL) title = "";
    if (C->title)
        free (C->title);
    C->title = G_malloc (strlen (title)+1);
    strcpy(C->title, title);
}

char *
I_GetClassTitle (C)
    struct ClassSig *C;
{
    if (C->title)
        return C->title;
    else
        return "";
}

I_WriteSigSet(fd, S)
    FILE *fd;
    struct SigSet *S;
{
    struct ClassSig *Cp;
    struct SubSig *Sp;
    int i,j, b1, b2;

    fprintf (fd, "title: %s\n",I_GetSigTitle(S));
    fprintf (fd, "nbands: %d\n", S->nbands);
    for (i=0; i < S->nclasses; i++)
    {
	Cp = &S->ClassSig[i];
	if (!Cp->used) continue;
	if (Cp->nsubclasses <= 0) continue;
	fprintf (fd, "class:\n");
	fprintf (fd, " classnum: %d\n", Cp->classnum);
	fprintf (fd, " classtitle: %s\n",I_GetClassTitle(Cp));
	fprintf (fd, " classtype: %d\n", Cp->type);

	for (j = 0; j < Cp->nsubclasses; j++)
	{
	    Sp = &Cp->SubSig[j];
	    fprintf (fd, " subclass:\n");
	    fprintf (fd, "  pi: %lf\n", Sp->pi);
	    fprintf (fd, "  means:");
	    for (b1 = 0; b1 < S->nbands; b1++)
		fprintf (fd, " %lf", Sp->means[b1]);
	    fprintf (fd, "\n");
	    fprintf (fd, "  covar:\n");
	    for (b1 = 0; b1 < S->nbands; b1++)
	    {
		fprintf (fd, "   ");
		for (b2 = 0; b2 < S->nbands; b2++)
		    fprintf (fd, " %lf", Sp->R[b1][b2]);
		fprintf (fd, "\n");
	    }
	    fprintf (fd, " endsubclass:\n");
	}
	fprintf (fd, "endclass:\n");
    }
}
