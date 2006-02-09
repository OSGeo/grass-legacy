#include <grass/imagery.h>
#include "signature.h"
#include "files.h"
int 
initialize_signatures (struct files *files, struct Signature *S)
{
    int n;

    I_init_signatures(S, files->nbands);
    for (n = 0; n < files->ncats; n++)
    {
	I_new_signature (S);
	S->sig[n].status = 1;
	G_strncpy (S->sig[n].desc,
	           G_get_cat (files->training_cats[n], &files->training_labels),
	           sizeof(S->sig[n].desc)
		  );
    }
}
