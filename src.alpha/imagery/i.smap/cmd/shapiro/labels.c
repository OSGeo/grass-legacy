#include "imagery.h"
#include "parms.h"
#include "files.h"

create_output_labels (S, files)
    struct SigSet *S;
    struct files *files;
{
    int n;
    struct ClassSig *C;

    G_init_cats ((CELL)0, S->title, &files->output_labels);
    for (n = 0; n < S->nclasses; n++)
    {
	C = &S->ClassSig[n];
	G_set_cat ((CELL)C->classnum, C->title, &files->output_labels);
    }
}
